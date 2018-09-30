/*
 * Copyright (c) 2014-2018, NVIDIA CORPORATION.  All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

/* ll_abi.c - Lowering arm function calls to LLVM IR.
 *
 * This file implements the AAPCS_VFP procedure call standard for the ARMv7
 * architecture.
 */

#include "gbldefs.h"
#include "global.h"
#include "symtab.h"
#include "llutil.h"
#include "ll_structure.h"
#include "dtypeutl.h"

#define DT_VOIDNONE DT_NONE

#define DT_BASETYPE(dt) (dt)

void
ll_abi_compute_call_conv(LL_ABI_Info *abi, int func_sptr, int jsra_flags)
{
  abi->call_conv = LL_CallConv_C; /* Default */
}

/* AAPCS has the concept of a homogeneous aggregrate. It is an aggregate type
 * where all the fundamental types are the same after flattening all structs
 * and arrays. */
struct arm_homogeneous_aggr {
  LL_Module *module;
  LL_Type *base_type;
  unsigned base_bytes;
};

/* Return 1 if dtype is inconsistent with the homogeneous aggregate
 * information pointed to by context. */
static int
update_homogeneous(void *context, DTYPE dtype, unsigned address,
                   int member_sptr)
{
  struct arm_homogeneous_aggr *ha = (struct arm_homogeneous_aggr *)context;
  unsigned size;
  LL_Type *llt;

  dtype = DT_BASETYPE(dtype);

  if (DTY(dtype) == TY_ARRAY)
    dtype = (DTYPE)DTY(dtype + 1); // ???

  switch (dtype) {
  case DT_CMPLX:
    dtype = DT_FLOAT;
    break;
  case DT_DCMPLX:
    dtype = DT_DBLE;
    break;
  }

  size = size_of(dtype);
  llt = ll_convert_dtype(ha->module, dtype);

  if (!ha->base_type) {
    if (address != 0)
      return 1;
    ha->base_type = llt;
    ha->base_bytes = size;
    return 0;
  }

  /* Check if dtype is consistent with the existing base type. */
  if (size != ha->base_bytes)
    return 1;

  if (!size || address % size != 0)
    return 1;

  /* Vector types just need matching sizes. Elements don't need to match. */
  if (ha->base_type->data_type == LL_VECTOR && llt->data_type == LL_VECTOR)
    return 0;

  /* Other base types must be identical. */
  return ha->base_type != llt;
}

/* Check if dtype is a VFP register candidate. Return the coercion type or NULL.
 */
static LL_Type *
check_vfp(LL_Module *module, DTYPE dtype)
{
  struct arm_homogeneous_aggr aggr = {module, NULL, 0};
  ISZ_T size = size_of(dtype);

  /* Check if dtype is a homogeneous aggregate. */
  if (visit_flattened_dtype(update_homogeneous, &aggr, dtype, 0, 0))
    return NULL;
  if (!aggr.base_type)
    return NULL;

  /* A non-aggregated scalar will simply be copied to base_type. */
  switch (aggr.base_type->data_type) {
  case LL_FLOAT:
  case LL_DOUBLE:
    break;
  case LL_VECTOR:
    /* Only 64-bit or 128-bit vectors supported. */
    if (aggr.base_bytes != 8 && aggr.base_bytes != 16)
      return NULL;
    break;
  default:
    return NULL;
  }

  /* We have a scalar or a homogeneous aggregate of the right type. The ABI
   * supports one to four elements of the base type. */
  if (size > 4 * aggr.base_bytes)
    return NULL;

  /* Single-element aggregate? */
  if (size == aggr.base_bytes)
    return aggr.base_type;

  /* Multiple elements coerced to an array type. */
  return ll_get_array_type(aggr.base_type, size / aggr.base_bytes, 0);
}

/* Classify an integer type for return or arg. */
static enum LL_ABI_ArgKind
classify_int(DTYPE dtype)
{
  /* Integer types smaller than a register must be sign/zero extended. */
  if (size_of(dtype) < 4)
    return DT_ISUNSIGNED(dtype) ? LL_ARG_ZEROEXT : LL_ARG_SIGNEXT;

  return LL_ARG_DIRECT;
}

/* Classify common to args and return values. */
static bool
classify_common(LL_ABI_Info *abi, LL_ABI_ArgInfo *arg, DTYPE dtype)
{
  if (DT_ISINT(dtype)) {
    arg->kind = classify_int(dtype);
    return true;
  }

  /* Basic types can be returned in registers directly. Complex types also
   * get handled correctly. */
  if (dtype == DT_VOIDNONE || DT_ISSCALAR(dtype)) {
    arg->kind = LL_ARG_DIRECT;
    return true;
  }

  /* Check for VFP arguments, but not for varargs calls. */
  if (abi->call_conv == LL_CallConv_AAPCS_VFP) {
    LL_Type *haggr = check_vfp(abi->module, dtype);
    if (haggr) {
      arg->kind = LL_ARG_COERCE;
      arg->type = haggr;
      return true;
    }
  }

  return false;
}

void
ll_abi_classify_return_dtype(LL_ABI_Info *abi, DTYPE dtype)
{
  enum LL_BaseDataType bdt = LL_NOTYPE;

  dtype = DT_BASETYPE(dtype);

  if (classify_common(abi, &abi->arg[0], dtype))
    return;

  /* Small structs can be returned in r0.
   * FIXME: can also be returned in register pair of floating-point registers.
   */
  switch (size_of(dtype)) {
  case 1:
    bdt = LL_I8;
    break;
  case 2:
    bdt = LL_I16;
    break;
  case 3:
  case 4:
    bdt = LL_I32;
    break;
  case 8:
    bdt = LL_I64;
    break;
  }
  if (bdt != LL_NOTYPE) {
    abi->arg[0].kind = LL_ARG_COERCE;
    abi->arg[0].type = ll_create_basic_type(abi->module, bdt, 0);
    return;
  }

  /* Large types must be returned in memory via an sret pointer argument. */
  abi->arg[0].kind = LL_ARG_INDIRECT;
}

void
ll_abi_classify_arg_dtype(LL_ABI_Info *abi, LL_ABI_ArgInfo *arg, DTYPE dtype)
{
  ISZ_T size;

  dtype = DT_BASETYPE(dtype);

  if (classify_common(abi, arg, dtype))
    return;

  /* All other arguments are coerced into an array of 32-bit registers. */
  size = size_of(dtype);
  arg->kind = LL_ARG_COERCE;
  if (alignment(dtype) > 4 && size % 8 == 0) {
    /* The coercion type needs to have the same alignment as the original type.
     */
    arg->type = ll_create_basic_type(abi->module, LL_I64, 0);
    if (size > 8)
      arg->type = ll_get_array_type(arg->type, size / 8, 0);
  } else {
    arg->type = ll_create_basic_type(abi->module, LL_I32, 0);
    if (size > 4)
      arg->type = ll_get_array_type(arg->type, (size + 3) / 4, 0);
  }
}
