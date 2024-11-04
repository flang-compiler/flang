/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

/* ll_abi.c - Lowering RISC-V function calls to LLVM IR.
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
  abi->call_conv = LL_CallConv_C;
  abi->call_as_varargs = false;
}

typedef struct RISCV_ABI_ArgInfo {
  enum LL_ABI_ArgKind kind;
  LL_Type *type;
  bool is_return_val;
} RISCV_ABI_ArgInfo;

inline static void
update_arg_info(LL_ABI_ArgInfo *arg, RISCV_ABI_ArgInfo *riscv_arg)
{
  arg->kind = riscv_arg->kind;
  if (riscv_arg->type != NULL) {
    arg->type = riscv_arg->type;
  }
}

// Classify an integer type for return or arg.
static enum LL_ABI_ArgKind
classify_int(DTYPE dtype)
{
  // Integer types smaller than a register must be sign/zero extended.
  // Unsigned char and char are both 8-bit unsigned integers zero-extended.
  if (size_of(dtype) < 2)
    return LL_ARG_ZEROEXT;
  // Short is widened according to its sign.
  else if (size_of(dtype) < 4)
    return DT_ISUNSIGNED(dtype) ? LL_ARG_ZEROEXT : LL_ARG_SIGNEXT;
  // 32-bit ints are always sign-extended.
  else if (size_of(dtype) < 8)
    return LL_ARG_SIGNEXT;

  return LL_ARG_DIRECT;
}

// Classify common arguments and return values.
// Values are returned in the same manner as a first named argument of the
// same type would be passed. If such an argument would have been passed by
// reference, the caller allocates memory for the return value, and passes
// the address as an implicit first parameter.
static void
classify_common(LL_Module *module, LL_ABI_Info *abi, RISCV_ABI_ArgInfo *arg,
                DTYPE dtype)
{
  if (DT_ISINT(dtype) && size_of(dtype) <= 8) {
    arg->kind = classify_int(dtype);
    return;
  }
  if (dtype == DT_VOIDNONE || (DT_ISSCALAR(dtype) && size_of(dtype) <= 8)) {
    arg->kind = LL_ARG_DIRECT;
    return;
  }
  if (size_of(dtype) <= 16) {
    // Small structs can be returned in up to two GPRs.
    arg->kind = LL_ARG_COERCE;
    arg->type = ll_coercion_type(abi->module, dtype, size_of(dtype), 8);
  } else {
    // Large types must be returned in memory via an sret pointer argument.
    // Scalars and aggregates larger than 2xXLEN must be passed by reference.
    if (arg->is_return_val) {
      arg->kind = LL_ARG_INDIRECT;
    } else {
      arg->kind = LL_ARG_INDIRECT_BUFFERED;
    }
  }
}

void
ll_abi_classify_return_dtype(LL_ABI_Info *abi, DTYPE dtype)
{
  RISCV_ABI_ArgInfo tmp_arg_info = {LL_ARG_UNKNOWN, NULL, true};

  dtype = DT_BASETYPE(dtype);

  classify_common(abi->module, abi, &tmp_arg_info, dtype);
  update_arg_info(&(abi->arg[0]), &tmp_arg_info);
}

void
ll_abi_classify_arg_dtype(LL_ABI_Info *abi, LL_ABI_ArgInfo *arg, DTYPE dtype)
{
  RISCV_ABI_ArgInfo tmp_arg_info = {LL_ARG_UNKNOWN, NULL, false};

  dtype = DT_BASETYPE(dtype);

  classify_common(abi->module, abi, &tmp_arg_info, dtype);
  update_arg_info(arg, &tmp_arg_info);
}
