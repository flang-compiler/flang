/*
 * Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
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

/** \file
 * \brief OpenMP/OpenACC/... atomics expander routines; all targets including
 * LLVM
 */

#include "gbldefs.h"
#include "error.h"
#include "global.h"
#include "symtab.h"
#include "regutil.h"
#include "machreg.h"
#include "ilm.h"
#include "ilmtp.h"
#include "ili.h"
#define EXPANDER_DECLARE_INTERNAL
#include "expand.h"
#include "machar.h"
#include "ccffinfo.h"
#include "pd.h"

static int atomic_capture_created;
static int atomic_capture_update_first;
static int atomic_store_created;
static int is_in_atomic;
static int is_in_atomic_read;
static int is_in_atomic_write;
static int is_in_atomic_capture;

static int capture_read_ili;
static int capture_update_ili;
static int atomic_typecast_operand;
// this is for the non-commutable operators.
// If it is non-zero, AtomicOp.ili_operand is the
// 1st operand of the atomic binary operator.
static int is_atomic_operand1 = 0;

static int cmplx_atomic_opcodes[] = {IL_SCMPLXADD, IL_SCMPLXSUB};
static int num_cmplx_opcodes = sizeof(cmplx_atomic_opcodes) / sizeof(int);

static int float_atomic_opcodes[] = {IL_FADD, IL_FSUB, IL_FDIV,
                                     IL_FMUL, IL_FMAX, IL_FMIN};
static int num_float_opcodes = sizeof(float_atomic_opcodes) / sizeof(int);

static int double_atomic_opcodes[] = {IL_DADD, IL_DSUB, IL_DDIV,
                                      IL_DMUL, IL_DMAX, IL_DMIN};
static int num_double_opcodes = sizeof(double_atomic_opcodes) / sizeof(int);

// FIX: 		integer*4 <-- real*4
// UFIX: 	unsigned integer*4 <-- real*4
// DFIX: 	integer*4 <-- real*8
// DFIXU: 	unsigned integer*4 <-- real*8
static int int_atomic_opcodes[] = {
    IL_IADD,    IL_ISUB,   IL_LEQV,   IL_XOR,   IL_IMUL,  IL_UIMUL,
    IL_AND,     IL_OR,     IL_UIADD,  IL_UISUB, IL_UIMUL, IL_ULSHIFT,
    IL_URSHIFT, IL_LSHIFT, IL_RSHIFT, IL_IDIV,  IL_UIDIV, IL_IMAX,
    IL_IMIN,    IL_NOT,    IL_INEG,   IL_LD};
static int num_int_opcodes = sizeof(int_atomic_opcodes) / sizeof(int);

// FIXK:		integer*8 <-- real*4
// FIXUK:	unsigned integer*8 <-- real*4
// DFIXK:	integer*8 <-- real*8
// DFIXUK:	unsigned integer*8 <-- real*8
static int long_atomic_opcodes[] = {IL_KADD, IL_KSUB, IL_KXOR, IL_KMUL,
                                    IL_UKMUL, IL_KAND, IL_KOR, IL_UKADD,
                                    IL_UKSUB, IL_KDIV, IL_UKDIV
                                    ,
                                    IL_KMAX, IL_KMIN
};
static int num_long_opcodes = sizeof(long_atomic_opcodes) / sizeof(int);

static struct {
  int atomic_operand;
  int ldst_point;
  int ldst_nme;
  int ili_operand;
} AtomicOp;

int
get_atomic_function_ex(ILI_OP opcode)
{
  // the last two/three(if "r" is the last letter) letters of the function name
  // i: integer 32bit
  // f: float, single precision 32bit
  // d: double,double precision 64bit
  // u: unsigned integer 32bit
  // k: integer 64bit
  // l: unsigned 64bit
  // r: reverse, non-commutable operator
  // 32bit integer
  if (atomic_typecast_operand == IL_FIX) {
    switch (opcode) {
    case IL_FADD:
      return mk_prototype("atomicaddif", "pure", DT_INT, 2, DT_CPTR, DT_FLOAT);
    case IL_FSUB:
      if (is_atomic_operand1)
        return mk_prototype("atomicsubifr", "pure", DT_INT, 2, DT_CPTR, DT_FLOAT);
      else
        return mk_prototype("atomicsubif", "pure", DT_INT, 2, DT_CPTR, DT_FLOAT);
    case IL_FMUL:
      return mk_prototype("atomicmulif", "pure", DT_INT, 2, DT_CPTR, DT_FLOAT);
    case IL_FDIV:
      if (is_atomic_operand1)
        return mk_prototype("atomicdivifr", "pure", DT_INT, 2, DT_CPTR, DT_FLOAT);
      else
        return mk_prototype("atomicdivif", "pure", DT_INT, 2, DT_CPTR, DT_FLOAT);
    case IL_FMAX:
      return mk_prototype("atomicmaxif", "pure", DT_INT, 2, DT_CPTR, DT_FLOAT);
    case IL_FMIN:
      return mk_prototype("atomicminif", "pure", DT_INT, 2, DT_CPTR, DT_FLOAT);
    default:;
    }
  } else if (atomic_typecast_operand == IL_UFIX) {
    switch (opcode) {
    case IL_FADD:
      return mk_prototype("atomicadduf", "pure", DT_UINT, 2, DT_CPTR, DT_FLOAT);
    case IL_FSUB:
      if (is_atomic_operand1)
        return mk_prototype("atomicsubufr", "pure", DT_UINT, 2, DT_CPTR, DT_FLOAT);
      else
        return mk_prototype("atomicsubuf", "pure", DT_UINT, 2, DT_CPTR, DT_FLOAT);
    case IL_FMUL:
      return mk_prototype("atomicmuluf", "pure", DT_UINT, 2, DT_CPTR, DT_FLOAT);
    case IL_FDIV:
      if (is_atomic_operand1)
        return mk_prototype("atomicdivufr", "pure", DT_UINT, 2, DT_CPTR, DT_FLOAT);
      else
        return mk_prototype("atomicdivuf", "pure", DT_UINT, 2, DT_CPTR, DT_FLOAT);
    case IL_FMAX:
      return mk_prototype("atomicmaxuf", "pure", DT_UINT, 2, DT_CPTR, DT_FLOAT);
    case IL_FMIN:
      return mk_prototype("atomicminuf", "pure", DT_UINT, 2, DT_CPTR, DT_FLOAT);
    default:;
    }
  } else if (atomic_typecast_operand == IL_DFIX) {
    switch (opcode) {
    case IL_DADD:
      return mk_prototype("atomicaddid", "pure", DT_INT, 2, DT_CPTR, DT_DBLE);
    case IL_DSUB:
      if (is_atomic_operand1)
        return mk_prototype("atomicsubidr", "pure", DT_INT, 2, DT_CPTR, DT_DBLE);
      else
        return mk_prototype("atomicsubid", "pure", DT_INT, 2, DT_CPTR, DT_DBLE);
    case IL_DMUL:
      return mk_prototype("atomicmulid", "pure", DT_INT, 2, DT_CPTR, DT_DBLE);
    case IL_DDIV:
      if (is_atomic_operand1)
        return mk_prototype("atomicdividr", "pure", DT_INT, 2, DT_CPTR, DT_DBLE);
      else
        return mk_prototype("atomicdivid", "pure", DT_INT, 2, DT_CPTR, DT_DBLE);
    case IL_DMAX:
      return mk_prototype("atomicmaxid", "pure", DT_INT, 2, DT_CPTR, DT_DBLE);
    case IL_DMIN:
      return mk_prototype("atomicminid", "pure", DT_INT, 2, DT_CPTR, DT_DBLE);
    default:;
    }
  } else if (atomic_typecast_operand == IL_DFIXU) {
    switch (opcode) {
    case IL_DADD:
      return mk_prototype("atomicaddud", "pure", DT_UINT, 2, DT_CPTR, DT_DBLE);
    case IL_DSUB:
      if (is_atomic_operand1)
        return mk_prototype("atomicsubudr", "pure", DT_UINT, 2, DT_CPTR, DT_DBLE);
      else
        return mk_prototype("atomicsubud", "pure", DT_UINT, 2, DT_CPTR, DT_DBLE);
    case IL_DMUL:
      return mk_prototype("atomicmulud", "pure", DT_UINT, 2, DT_CPTR, DT_DBLE);
    case IL_DDIV:
      if (is_atomic_operand1)
        return mk_prototype("atomicdivudr", "pure", DT_UINT, 2, DT_CPTR, DT_DBLE);
      else
        return mk_prototype("atomicdivud", "pure", DT_UINT, 2, DT_CPTR, DT_DBLE);
    case IL_DMAX:
      return mk_prototype("atomicmaxud", "pure", DT_UINT, 2, DT_CPTR, DT_DBLE);
    case IL_DMIN:
      return mk_prototype("atomicminud", "pure", DT_UINT, 2, DT_CPTR, DT_DBLE);
    default:;
    }
  }
  // 64bits integer
  else if (atomic_typecast_operand == IL_FIXK) {
    switch (opcode) {
    case IL_FADD:
      return mk_prototype("atomicaddkf", "pure", DT_INT8, 2, DT_CPTR, DT_FLOAT);
    case IL_FSUB:
      if (is_atomic_operand1)
        return mk_prototype("atomicsubkfr", "pure", DT_INT8, 2, DT_CPTR, DT_FLOAT);
      else
        return mk_prototype("atomicsubkf", "pure", DT_INT8, 2, DT_CPTR, DT_FLOAT);
    case IL_FMUL:
      return mk_prototype("atomicmulkf", "pure", DT_INT8, 2, DT_CPTR, DT_FLOAT);
    case IL_FDIV:
      if (is_atomic_operand1)
        return mk_prototype("atomicdivkfr", "pure", DT_INT8, 2, DT_CPTR, DT_FLOAT);
      else
        return mk_prototype("atomicdivkf", "pure", DT_INT8, 2, DT_CPTR, DT_FLOAT);
    case IL_FMAX:
      return mk_prototype("atomicmaxkf", "pure", DT_INT8, 2, DT_CPTR, DT_FLOAT);
    case IL_FMIN:
      return mk_prototype("atomicminkf", "pure", DT_INT8, 2, DT_CPTR, DT_FLOAT);
    default:;
    }
  } else if (atomic_typecast_operand == IL_FIXUK) {
    switch (opcode) {
    case IL_FADD:
      return mk_prototype("atomicaddlf", "pure", DT_UINT8, 2, DT_CPTR, DT_FLOAT);
    case IL_FSUB:
      if (is_atomic_operand1)
        return mk_prototype("atomicsublfr", "pure", DT_UINT8, 2, DT_CPTR, DT_FLOAT);
      else
        return mk_prototype("atomicsublf", "pure", DT_UINT8, 2, DT_CPTR, DT_FLOAT);
    case IL_FMUL:
      return mk_prototype("atomicmullf", "pure", DT_UINT8, 2, DT_CPTR, DT_FLOAT);
    case IL_FDIV:
      if (is_atomic_operand1)
        return mk_prototype("atomicdivlfr", "pure", DT_UINT8, 2, DT_CPTR, DT_FLOAT);
      else
        return mk_prototype("atomicdivlf", "pure", DT_UINT8, 2, DT_CPTR, DT_FLOAT);
    case IL_FMAX:
      return mk_prototype("atomicmaxlf", "pure", DT_UINT8, 2, DT_CPTR, DT_FLOAT);
    case IL_FMIN:
      return mk_prototype("atomicminlf", "pure", DT_UINT8, 2, DT_CPTR, DT_FLOAT);
    default:;
    }
  } else if (atomic_typecast_operand == IL_DFIXK) {
    switch (opcode) {
    case IL_DADD:
      return mk_prototype("atomicaddkd", "pure", DT_INT8, 2, DT_CPTR, DT_DBLE);
    case IL_DSUB:
      if (is_atomic_operand1)
        return mk_prototype("atomicsubkdr", "pure", DT_INT8, 2, DT_CPTR, DT_DBLE);
      else
        return mk_prototype("atomicsubkd", "pure", DT_INT8, 2, DT_CPTR, DT_DBLE);
    case IL_DMUL:
      return mk_prototype("atomicmulkd", "pure", DT_INT8, 2, DT_CPTR, DT_DBLE);
    case IL_DDIV:
      if (is_atomic_operand1)
        return mk_prototype("atomicdivkdr", "pure", DT_INT8, 2, DT_CPTR, DT_DBLE);
      else
        return mk_prototype("atomicdivkd", "pure", DT_INT8, 2, DT_CPTR, DT_DBLE);
    case IL_DMAX:
      return mk_prototype("atomicmaxkd", "pure", DT_INT8, 2, DT_CPTR, DT_DBLE);
    case IL_DMIN:
      return mk_prototype("atomicminkd", "pure", DT_INT8, 2, DT_CPTR, DT_DBLE);
    default:;
    }
  } else if (atomic_typecast_operand == IL_DFIXUK) {
    switch (opcode) {
    case IL_DADD:
      return mk_prototype("atomicaddld", "pure", DT_UINT8, 2, DT_CPTR, DT_DBLE);
    case IL_DSUB:
      if (is_atomic_operand1)
        return mk_prototype("atomicsubldr", "pure", DT_UINT8, 2, DT_CPTR, DT_DBLE);
      else
        return mk_prototype("atomicsubld", "pure", DT_UINT8, 2, DT_CPTR, DT_DBLE);
    case IL_DMUL:
      return mk_prototype("atomicmulld", "pure", DT_UINT8, 2, DT_CPTR, DT_DBLE);
    case IL_DDIV:
      if (is_atomic_operand1)
        return mk_prototype("atomicdivldr", "pure", DT_UINT8, 2, DT_CPTR, DT_DBLE);
      else
        return mk_prototype("atomicdivld", "pure", DT_UINT8, 2, DT_CPTR, DT_DBLE);
    case IL_DMAX:
      return mk_prototype("atomicmaxld", "pure", DT_UINT8, 2, DT_CPTR, DT_DBLE);
    case IL_DMIN:
      return mk_prototype("atomicminld", "pure", DT_UINT8, 2, DT_CPTR, DT_DBLE);
    default:;
    }
  } else if (atomic_typecast_operand == IL_SNGL) {
    switch (opcode) {
    case IL_DADD:
     return mk_prototype("atomicaddfd", "pure", DT_FLOAT, 2, DT_CPTR, DT_DBLE);
    case IL_DSUB:
      if (is_atomic_operand1)
        return mk_prototype("atomicsubfdr", "pure", DT_FLOAT, 2, DT_CPTR, DT_DBLE);
      else
        return mk_prototype("atomicsubfd", "pure", DT_FLOAT, 2, DT_CPTR, DT_DBLE);
    case IL_DMUL:
      return mk_prototype("atomicmulfd", "pure", DT_FLOAT, 2, DT_CPTR, DT_DBLE);
    case IL_DDIV:
      if (is_atomic_operand1)
        return mk_prototype("atomicdivfdr", "pure", DT_FLOAT, 2, DT_CPTR, DT_DBLE);
      else
        return mk_prototype("atomicdivfd", "pure", DT_FLOAT, 2, DT_CPTR, DT_DBLE);
    case IL_DMAX:
      return mk_prototype("atomicmaxfd", "pure", DT_FLOAT, 2, DT_CPTR, DT_DBLE);
    case IL_DMIN:
      return mk_prototype("atomicminfd", "pure", DT_FLOAT, 2, DT_CPTR, DT_DBLE);
    default:;
    }
  }

  error(155, 3, gbl.lineno, "Invalid atomic operation.", CNULL);
  return 0;
}

int
get_atomic_function(ILI_OP opcode)
{
  switch (opcode) {
  /*
   * Update:
   */
  case IL_IMUL:
    return mk_prototype("atomicmuli", "pure", DT_INT, 2, DT_CPTR, DT_INT);
  case IL_UIMUL:
    return mk_prototype("atomicmulu", "pure", DT_UINT, 2, DT_CPTR, DT_UINT);
  case IL_KMUL:
    return mk_prototype("atomicmulil", "pure", DT_INT8, 2, DT_CPTR, DT_INT8);
  case IL_UKMUL:
    return mk_prototype("atomicmulul", "pure", DT_UINT8, 2, DT_CPTR, DT_UINT8);
  case IL_FMUL:
    return mk_prototype("atomicmulf", "pure", DT_FLOAT, 2, DT_CPTR, DT_FLOAT);
  case IL_DMUL:
    return mk_prototype("atomicmuld", "pure", DT_DBLE, 2, DT_CPTR, DT_DBLE);
  case IL_IMAX:
    return mk_prototype("atomicmaxi", "pure", DT_INT, 2, DT_CPTR, DT_INT);
  case IL_IMIN:
    return mk_prototype("atomicmini", "pure", DT_INT, 2, DT_CPTR, DT_INT);
  case IL_KMAX:
    return mk_prototype("atomicmaxil", "pure", DT_INT8, 2, DT_CPTR, DT_INT8);
  case IL_KMIN:
    return mk_prototype("atomicminil", "pure", DT_INT8, 2, DT_CPTR, DT_INT8);
  case IL_NOT:
    return mk_prototype("atomicnoti", "pure", DT_INT, 2, DT_CPTR, DT_INT);
  case IL_XOR:
    return mk_prototype("atomicxori", "pure", DT_INT, 2, DT_CPTR, DT_INT);
  case IL_LEQV:
    return mk_prototype("atomicleqvi", "pure", DT_INT, 2, DT_CPTR, DT_INT);
  case IL_KXOR:
    return mk_prototype("atomicxorll", "pure", DT_INT8, 2, DT_CPTR, DT_INT8);
  case IL_AND:
    return mk_prototype("atomicandi", "pure", DT_INT, 2, DT_CPTR, DT_INT);
  case IL_KAND:
    return mk_prototype("atomicandll", "pure", DT_INT8, 2, DT_CPTR, DT_INT8);
  case IL_OR:
    return mk_prototype("atomicori", "pure", DT_INT, 2, DT_CPTR, DT_INT);
  case IL_KOR:
    return mk_prototype("atomicorll", "pure", DT_INT8, 2, DT_CPTR, DT_INT8);
  case IL_IADD:
    return mk_prototype("atomicaddi", "pure", DT_INT, 2, DT_CPTR, DT_INT);
  case IL_KADD:
    return mk_prototype("atomicaddil", "pure", DT_INT8, 2, DT_CPTR, DT_INT8);
  case IL_ISUB:
    return mk_prototype("atomicsubi", "pure", DT_INT, 2, DT_CPTR, DT_INT);
  case IL_KSUB:
    return mk_prototype("atomicsubil", "pure", DT_INT8, 2, DT_CPTR, DT_INT8);
  case IL_UIADD:
    return mk_prototype("atomicaddu", "pure", DT_UINT, 2, DT_CPTR, DT_UINT);
  case IL_UKADD:
    return mk_prototype("atomicaddul", "pure", DT_UINT8, 2, DT_CPTR, DT_UINT8);
  case IL_UISUB:
    return mk_prototype("atomicsubu", "pure", DT_UINT, 2, DT_CPTR, DT_UINT);
  case IL_UKSUB:
    return mk_prototype("atomicsubull", "pure", DT_UINT8, 2, DT_CPTR, DT_UINT8);
  case IL_INEG:
    return mk_prototype("atomicnegi", "pure", DT_INT, 2, DT_CPTR, DT_INT);
  case IL_ULSHIFT:
    return mk_prototype("atomiclshiftu", "pure", DT_UINT, 2, DT_CPTR, DT_UINT);
  case IL_URSHIFT:
    return mk_prototype("atomicrshiftu", "pure", DT_UINT, 2, DT_CPTR, DT_UINT);
  case IL_LSHIFT:
    return mk_prototype("atomiclshifti", "pure", DT_INT, 2, DT_CPTR, DT_INT);
  case IL_RSHIFT:
    return mk_prototype("atomicrshifti", "pure", DT_INT, 2, DT_CPTR, DT_INT);
  case IL_FADD:
    return mk_prototype("atomicaddf", "pure", DT_FLOAT, 2, DT_CPTR, DT_FLOAT);
  case IL_FSUB:
    return mk_prototype("atomicsubf", "pure", DT_FLOAT, 2, DT_CPTR, DT_FLOAT);
  case IL_FMAX:
    return mk_prototype("atomicmaxf", "pure", DT_FLOAT, 2, DT_CPTR, DT_FLOAT);
  case IL_FMIN:
    return mk_prototype("atomicminf", "pure", DT_FLOAT, 2, DT_CPTR, DT_FLOAT);
  case IL_DADD:
    return mk_prototype("atomicaddd", "pure", DT_DBLE, 2, DT_CPTR, DT_DBLE);
  case IL_DSUB:
    return mk_prototype("atomicsubd", "pure", DT_DBLE, 2, DT_CPTR, DT_DBLE);
  case IL_DMAX:
    return mk_prototype("atomicmaxd", "pure", DT_DBLE, 2, DT_CPTR, DT_DBLE);
  case IL_DMIN:
    return mk_prototype("atomicmind", "pure", DT_DBLE, 2, DT_CPTR, DT_DBLE);
  case IL_IDIV:
    return mk_prototype("atomicdivi", "pure", DT_INT, 2, DT_CPTR, DT_INT);
  case IL_KDIV:
    return mk_prototype("atomicdivil", "pure", DT_INT8, 2, DT_CPTR, DT_INT8);
  case IL_UIDIV:
    return mk_prototype("atomicdivu", "pure", DT_UINT, 2, DT_CPTR, DT_UINT);
  case IL_UKDIV:
    return mk_prototype("atomicdivul", "pure", DT_UINT8, 2, DT_CPTR, DT_UINT8);
  case IL_FDIV:
    return mk_prototype("atomicdivf", "pure", DT_FLOAT, 2, DT_CPTR, DT_FLOAT);
  case IL_DDIV:
    return mk_prototype("atomicdivd", "pure", DT_DBLE, 2, DT_CPTR, DT_DBLE);
  /*
   * Read:
   */
#define DT_VOID_NONE DT_NONE
  case IL_LD:
    return mk_prototype("atomicloadi", "pure", DT_VOID_NONE, 2, DT_CPTR, DT_CPTR);
  case IL_LDSP:
    return mk_prototype("atomicloadf", "pure", DT_VOID_NONE, 2, DT_CPTR, DT_CPTR);
  case IL_LDDP:
    return mk_prototype("atomicloadd", "pure", DT_VOID_NONE, 2, DT_CPTR, DT_CPTR);
  case IL_LDKR:
    return mk_prototype("atomicloadl", "pure", DT_VOID_NONE, 2, DT_CPTR, DT_CPTR);
  /*
   * Write:
   */
  case IL_ST:
    return mk_prototype("atomicexchi", "pure", DT_INT, 2, DT_CPTR, DT_INT);
  case IL_STSP:
    return mk_prototype("atomicexchf", "pure", DT_FLOAT, 2, DT_CPTR, DT_FLOAT);
  case IL_STDP:
    return mk_prototype("atomicexchd", "pure", DT_DBLE, 2, DT_CPTR, DT_DBLE);
  case IL_STKR:
    return mk_prototype("atomicexchul", "pure", DT_INT8, 2, DT_CPTR, DT_INT8);
  case IL_SCMPLXADD:
    return mk_prototype("atomicaddcmplx", "pure", DT_VOID_NONE, 3, DT_CPTR, DT_FLOAT, DT_FLOAT);
  case IL_SCMPLXSUB:
    return mk_prototype("atomicsubcmplx", "pure", DT_VOID_NONE, 3, DT_CPTR, DT_FLOAT, DT_FLOAT);
  default:
    interr("Unsupported atomic opcode: ", opcode, 3);
    return 0;
  }
}

int
get_capture_read_ili(void)
{
  return capture_read_ili;
}

void
set_capture_read_ili(int x)
{
  capture_read_ili = x;
}

int
get_capture_update_ili(void)
{
  return capture_update_ili;
}

void
set_capture_update_ili(int x)
{
  capture_update_ili = x;
}

int
get_is_in_atomic(void)
{
  return is_in_atomic;
}

void
set_is_in_atomic(int x)
{
  is_in_atomic = x;
}

int
get_is_in_atomic_read(void)
{
  return is_in_atomic_read;
}

void
set_is_in_atomic_read(int x)
{
  is_in_atomic_read = x;
}

int
get_is_in_atomic_write(void)
{
  return is_in_atomic_write;
}

void
set_is_in_atomic_write(int x)
{
  is_in_atomic_write = x;
}

int
get_is_in_atomic_capture(void)
{
  return is_in_atomic_capture;
}

void
set_is_in_atomic_capture(int x)
{
  is_in_atomic_capture = x;
}

int
get_atomic_capture_created(void)
{
  return atomic_capture_created;
}

void
set_atomic_capture_created(int x)
{
  atomic_capture_created = x;
}

int
get_atomic_store_created(void)
{
  return atomic_store_created;
}

void
set_atomic_store_created(int x)
{
  atomic_store_created = x;
}

int
get_atomic_write_opcode(int current_ili)
{
  int ili = current_ili;
  int store_opcode;
  int store_value;
  int store_pt, store_nme;

  store_opcode = ILI_OPC(ili);
  store_value = ILI_OPND(ili, 1);
  store_pt = ILI_OPND(ili, 2);
  store_nme = ILI_OPND(ili, 3);

  if (store_opcode != IL_ST && store_opcode != IL_STDP &&
      store_opcode != IL_STSP && store_opcode != IL_STKR) {
    /* Rely on the caller to issue an error if necessary */
    return 0;
  }

  AtomicOp.atomic_operand = 0;
  AtomicOp.ldst_point = store_pt;
  AtomicOp.ldst_nme = store_nme;
  AtomicOp.ili_operand = store_value;

  return store_opcode;
}

int
create_atomic_capture_seq(int update_ili, int read_ili, int capture_first)
{
  int function;
  int intarg_opcode, floatarg_opcode, doublearg_opcode, longarg_opcode;
  int ld_opcode;
  int st_opcode;
  int arg_opcode;
  int store_pt, store_nme, arg, garg;
  int argreg = 0;
  int update_operand;
  int load_pt1, load_pt2;
  int op1, op2, opc;
  int update_op;
  int return_op;
  int result;
  int result_arg;
  int msize;
  int allow_capture_last = 1;
  int arg_dt = 0;

#if defined(TARGET_X8664)
  intarg_opcode = IL_DAIR;
  floatarg_opcode = IL_DASP;
  doublearg_opcode = IL_DADP;
  longarg_opcode = IL_DAKR;
#else
  intarg_opcode = IL_ARGIR;
  floatarg_opcode = IL_ARGSP;
  doublearg_opcode = IL_ARGDP;
  longarg_opcode = IL_ARGKR;
#endif

  st_opcode = ILI_OPC(read_ili);
  if (st_opcode != ILI_OPC(update_ili)) {
    /* This is not a legal atomic capture--data type mismatch */
    interr("Atomic Capture: Mismatched storage operations.", 0, 3);
  }

  switch (st_opcode) {
  case IL_ST:
    arg_opcode = intarg_opcode;
    ld_opcode = IL_LD;
    return_op = IL_DFRIR;
    result_arg = IR_RETVAL;
    arg_dt = DT_INT;
#if defined(TARGET_X8664)
    argreg = ARG_IR(1);
#else
#endif
    break;
  case IL_STDP:
    arg_opcode = doublearg_opcode;
    ld_opcode = IL_LDDP;
    arg_dt = DT_DBLE;
#if defined(TARGET_X8664)
    argreg = ARG_XR(0);
    return_op = IL_DFRDP;
    result_arg = FR_RETVAL;
#else
    return_op = IL_DFRDP;
    result_arg = FR_RETVAL;
#endif
    break;
  case IL_STSP:
    arg_opcode = floatarg_opcode;
    ld_opcode = IL_LDSP;
    arg_dt = DT_FLOAT;
#if defined(TARGET_X8664)
    argreg = ARG_XR(0);
    return_op = IL_DFRSP;
    result_arg = FR_RETVAL;
#else
    return_op = IL_DFRSP;
    result_arg = FR_RETVAL;
#endif
    break;
  case IL_STKR:
    arg_opcode = longarg_opcode;
    ld_opcode = IL_LDKR;
    arg_dt = DT_INT8;
#if defined(TARGET_X8664)
    return_op = IL_DFRKR;
    result_arg = KR_RETVAL;
    argreg = ARG_IR(1);
#else
    return_op = IL_DFRKR;
    result_arg = KR_RETVAL;
#endif
    break;
  default:
    interr("Create: Unexpected atomic store opcode", st_opcode, 3);
    break;
  }

  op1 = ILI_OPND(update_ili, 1);
  op2 = ILI_OPND(update_ili, 2);
  store_nme = ILI_OPND(update_ili, 3);

  update_op = op1;
  function = get_atomic_function(ILI_OPC(op1));

  load_pt1 = load_pt2 = -1;

  if (ILI_OPC(ILI_OPND(update_op, 1)) == IL_CSEIR) {
    /* Look through the CSEIR to the "real" load.
     * If the read_ili is done off of a CSE, make sure
     * it is the same.
     */
    if (ILI_OPC(ILI_OPND(read_ili, 1)) == IL_CSEIR) {
      if (ILI_OPND(read_ili, 1) != ILI_OPND(update_op, 1)) {
        interr("Mismatched CSE (1).\n", 0, 0);
      } else {
        allow_capture_last = 0;
      }
    }
    load_pt1 = ILI_OPND(ILI_OPND(update_op, 1), 1);
  } else if (ILI_OPC(ILI_OPND(update_op, 1)) == ld_opcode) {
    load_pt1 = ILI_OPND(update_op, 1);
  }

  if (ILI_OPC(ILI_OPND(update_op, 2)) == IL_CSEIR) {
    /* Look through the CSEIR to the "real" load.
     * If the read_ili is done off of a CSE, make sure
     * it is the same.
     */
    if (ILI_OPC(ILI_OPND(read_ili, 1)) == IL_CSEIR) {
      if (ILI_OPND(read_ili, 1) != ILI_OPND(update_op, 2)) {
        interr("Mismatched CSE (2).\n", 0, 0);
      } else {
        allow_capture_last = 0;
      }
    }
    load_pt2 = ILI_OPND(ILI_OPND(update_op, 2), 1);
  } else if (ILI_OPC(ILI_OPND(update_op, 2)) == ld_opcode) {
    load_pt2 = ILI_OPND(update_op, 2);
  }

  if (load_pt1 == -1 && load_pt2 == -1) {
    interr("Can't find matching load operation in atomic capture.", 0, 3);
    return 0;
  }

  store_pt = op2;
  if (ILI_OPC(store_pt) == IL_CSEAR) {
    store_pt = ILI_OPND(store_pt, 1);
  }

  /* Determine which operand from update_op comes from the load,
   * and which operand comes from the "updating" part.
   */
  if (ILI_OPND(load_pt1, 1) == store_pt) {
    /* The first argument for the update_op was the load/store point.
     * Use the second as the update_operand.
     */
    update_operand = ILI_OPND(update_op, 2);
  } else if (ILI_OPND(load_pt2, 1) == store_pt) {
    /* The second argument for the update_op was the load/store point.
     * Use the first as the update_operand.
     */
    update_operand = ILI_OPND(update_op, 1);
  } else {
    interr("Can't find load operation in atomic capture.", 0, 3);
    return 0;
  }

  arg = ad1ili(IL_NULL, 0);
#if defined(TARGET_X8664)
  arg = ad3ili(arg_opcode, update_operand, argreg, arg);
  arg = ad3ili(IL_DAAR, store_pt, ARG_IR(0), arg);
#else
  arg = ad3ili(arg_opcode, update_operand, arg, arg);
  arg = ad3ili(IL_ARGAR, store_pt, arg, 0);
#endif
  garg = ad1ili(IL_NULL, 0);
  garg = ad4ili(IL_GARG, update_operand, garg, arg_dt, 0);
  garg = ad4ili(IL_GARG, store_pt, garg, DT_CPTR, store_nme);
  arg = ad2ili(IL_JSR, function, arg);
  garg = ad3ili(IL_GJSR, function, garg, 0);
  ILI_ALT(arg) = garg;
  arg = ad2ili(return_op, arg, result_arg);
  if (!capture_first && allow_capture_last) {
    arg = ad2ili(ILI_OPC(op1), arg, update_operand);
  }
  /* Replicate the store for the original read_ili, except the
   * value that is being stored.
   */
  result = ad4ili(st_opcode, arg, ILI_OPND(read_ili, 2), ILI_OPND(read_ili, 3),
                  ILI_OPND(read_ili, 4));
  return result;
}

int
create_atomic_write_seq(int store_ili)
{
  int arg, garg;
  int function;
  int store_pt, store_nme;
  int arg_opcode;
  int intarg_opcode, floatarg_opcode, doublearg_opcode, longarg_opcode;
  int argreg;
  int arg_dt = 0;

#if defined(TARGET_X8664)
  intarg_opcode = IL_DAIR;
  floatarg_opcode = IL_DASP;
  doublearg_opcode = IL_DADP;
  longarg_opcode = IL_DAKR;
#else
  intarg_opcode = IL_ARGIR;
  floatarg_opcode = IL_ARGSP;
  doublearg_opcode = IL_ARGDP;
  longarg_opcode = IL_ARGKR;
#endif

  switch (ILI_OPC(store_ili)) {
  case IL_ST:
    arg_dt = DT_INT;
    arg_opcode = intarg_opcode;
#if defined(TARGET_X8664)
    argreg = ARG_IR(1);
#endif
    break;
  case IL_STDP:
    arg_dt = DT_DBLE;
    arg_opcode = doublearg_opcode;
#if defined(TARGET_X8664)
    argreg = ARG_XR(0);
#endif
    break;
  case IL_STSP:
    arg_dt = DT_FLOAT;
    arg_opcode = floatarg_opcode;
#if defined(TARGET_X8664)
    argreg = ARG_XR(0);
#endif
    break;
  case IL_STKR:
    arg_dt = DT_INT8;
    arg_opcode = longarg_opcode;
#if defined(TARGET_X8664)
    argreg = ARG_IR(1);
#endif
    break;
  default:
    interr("Create: Unexpected atomic store opcode", ILI_OPC(store_ili), 3);
    break;
  }

  /* Create a call to:
   * atomicexch*(store_pt, load_val)
   * which stores val atomically into store_pt.
   */

  store_pt = ILI_OPND(store_ili, 2);
  store_nme = ILI_OPND(store_ili, 3);
  function = get_atomic_function(ILI_OPC(store_ili));
  arg = ad1ili(IL_NULL, 0);
  garg = ad1ili(IL_NULL, 0);
#if defined(TARGET_X8664)
  arg = ad3ili(arg_opcode, AtomicOp.ili_operand, argreg, arg);
  arg = ad3ili(IL_DAAR, store_pt, ARG_IR(0), arg);
#else
  arg = ad3ili(arg_opcode, AtomicOp.ili_operand, arg, arg);
  arg = ad3ili(IL_ARGAR, store_pt, arg, 0);
#endif
  garg = ad4ili(IL_GARG, AtomicOp.ili_operand, garg, arg_dt, 0);
  garg = ad4ili(IL_GARG, store_pt, garg, DT_CPTR, store_nme);
  arg = ad2ili(IL_JSR, function, arg);
  garg = ad3ili(IL_GJSR, function, garg, 0);
  ILI_ALT(arg) = garg;
  return arg;
}

int
get_atomic_read_opcode(int current_ili)
{
  int ili = current_ili;
  int load_opcode, store_opcode;
  int ld_op;
  int load_pt, load_nme, store_pt;

  store_opcode = ILI_OPC(ili);
  ld_op = ILI_OPND(ili, 1);
  store_pt = ILI_OPND(ili, 2);

  load_opcode = ILI_OPC(ld_op);

  if (load_opcode == IL_CSEIR) {
    /* Look through the CSEIR opcode */
    ld_op = ILI_OPND(ILI_OPND(ili, 1), 1);
    load_opcode = ILI_OPC(ld_op);
  }

  if (load_opcode != IL_LD && load_opcode != IL_LDDP &&
      load_opcode != IL_LDSP && load_opcode != IL_LDKR) {
    /* Rely on the caller to issue an error if necessary */
    return 0;
  }

  load_pt = ILI_OPND(ld_op, 1);
  load_nme = ILI_OPND(ld_op, 2);
  AtomicOp.atomic_operand = ld_op;
  AtomicOp.ldst_point = load_pt;
  AtomicOp.ldst_nme = load_nme;
  AtomicOp.ili_operand = 0;

  return store_opcode;
}

int
create_atomic_read_seq(int store_ili)
{
  int arg, garg;
  int function;
  int store_pt, store_nme;

  /* Create a call to:
   * atomicload*(store_pt, load_pt)
   * which loads (atomically from load_pt), and stores (non-atomically)
   * into store_pt.
   */
  store_pt = ILI_OPND(store_ili, 2);
  store_nme = ILI_OPND(store_ili, 3);
  function = get_atomic_function(ILI_OPC(AtomicOp.atomic_operand));
  arg = ad1ili(IL_NULL, 0);
#if defined(TARGET_X8664)
  arg = ad3ili(IL_DAAR, AtomicOp.ldst_point, ARG_IR(1), arg);
  arg = ad3ili(IL_DAAR, store_pt, ARG_IR(0), arg);
#else
  arg = ad3ili(IL_ARGAR, AtomicOp.ldst_point, arg, 0);
  arg = ad3ili(IL_ARGAR, store_pt, arg, 0);
#endif
  garg = ad1ili(IL_NULL, 0);
  garg = ad4ili(IL_GARG, AtomicOp.ldst_point, garg, DT_CPTR, AtomicOp.ldst_nme);
  garg = ad4ili(IL_GARG, store_pt, garg, DT_CPTR, store_nme);
  arg = ad2ili(IL_JSR, function, arg);
  garg = ad3ili(IL_GJSR, function, garg, 0);
  ILI_ALT(arg) = garg;
  return arg;
}

/*setup the atomic operands and opcode
when high-precision to low-precision conversion happens
*/
static void
set_atomic_typecast_h2l(int tcast_ili)
{
  atomic_typecast_operand = tcast_ili;
}

static void
reset_atomic_typecast_h2l()
{
  atomic_typecast_operand = 0;
  is_atomic_operand1 = 0;
}

static int
is_atomic_typcast_h2l()
{
  return atomic_typecast_operand != 0;
}

ILI_OP
get_atomic_update_opcode(int current_ili)
{
  int ili = current_ili;
  int bin_op, op1, op2, store_pt, store_nme, load_pt1, load_pt2;
  int opc;
  ILI_OP store_opcode, load_opcode;

  load_pt1 = 0;
  load_pt2 = 0;

  store_opcode = ILI_OPC(ili);

  if (store_opcode == IL_FREEIR) {
    AtomicOp.atomic_operand = ili;
    AtomicOp.ldst_point = 0;
    AtomicOp.ldst_nme = 0;
    AtomicOp.ili_operand = 0;
    return IL_FREEIR;
  }

  if (store_opcode != IL_ST && store_opcode != IL_STDP &&
      store_opcode != IL_STSP && store_opcode != IL_STKR && 
      store_opcode != IL_STSCMPLX) {
    interr("Error: Detected unexpected atomic store opcode.", store_opcode, 3);
    return 0;
  }

  bin_op = ILI_OPND(ili, 1);
  store_pt = ILI_OPND(ili, 2);
  store_nme = ILI_OPND(ili, 3);

  if (ILI_OPC(store_pt) == IL_CSEAR) {
    store_pt = ILI_OPND(store_pt, 1);
  }
  // check the high precision to low precision type cast first
  // a demo example of double to unsigned long long translation
  // 106  LDKR           48^    12~ <array[0]>    i8
  // 107  DAKR          106^ kr( 5)     1^
  // 108  QJSR          212~<__mth_i_dfloatuk>   107^
  // 109  DFRDP         108^ dp( 1)
  // 110  DFLOATUK      106^   109^-alt
  // 111  DADD          105^   110^
  // 112  DADP          111^ dp( 1)     1^
  // 113  QJSR          213~<__mth_i_dfixuk>   112^
  // 114  DFRKR         113^ kr( 1)
  // 115  STKR          114^    48^    12~ <array[0]>    i8
  // The following if statement takes care of the ili 115 ~ ili 111
  // There is another if statement (in this function) takes care of
  // ili 110 ~ ili 106
  if (store_opcode == IL_STKR && ILI_OPC(bin_op) == IL_DFRKR &&
      ILI_OPC(ILI_OPND(bin_op, 1)) == IL_QJSR &&
      (strcmp(SYMNAME(ILI_OPND(ILI_OPND(bin_op, 1), 1)), "__mth_i_fixuk") ==
           0 ||
       strcmp(SYMNAME(ILI_OPND(ILI_OPND(bin_op, 1), 1)), "__mth_i_fixukx") ==
           0)) {
    set_atomic_typecast_h2l(IL_FIXUK);
    bin_op = ILI_OPND(ILI_OPND(ILI_OPND(bin_op, 1), 2), 1);
    store_opcode = IL_STSP;
  } else if (store_opcode == IL_STKR && ILI_OPC(bin_op) == IL_DFRKR &&
             ILI_OPC(ILI_OPND(bin_op, 1)) == IL_QJSR &&
             (strcmp(SYMNAME(ILI_OPND(ILI_OPND(bin_op, 1), 1)),
                     "__mth_i_dfixuk") == 0 ||
              strcmp(SYMNAME(ILI_OPND(ILI_OPND(bin_op, 1), 1)),
                     "__mth_i_dfixukx") == 0)) {
    set_atomic_typecast_h2l(IL_DFIXUK);
    bin_op = ILI_OPND(ILI_OPND(ILI_OPND(bin_op, 1), 2), 1);
    store_opcode = IL_STDP;
  } else if (store_opcode == IL_STKR && ILI_OPC(bin_op) == IL_DFRKR &&
             ILI_OPC(ILI_OPND(bin_op, 1)) == IL_QJSR &&
             strcmp(SYMNAME(ILI_OPND(ILI_OPND(bin_op, 1), 1)),
                    "__mth_i_kfixx") == 0) {
    set_atomic_typecast_h2l(IL_FIXK);
    bin_op = ILI_OPND(ILI_OPND(ILI_OPND(bin_op, 1), 2), 1);
    store_opcode = IL_STSP;
  } else if (store_opcode == IL_STKR && ILI_OPC(bin_op) == IL_DFRKR &&
             ILI_OPC(ILI_OPND(bin_op, 1)) == IL_QJSR &&
             strcmp(SYMNAME(ILI_OPND(ILI_OPND(bin_op, 1), 1)),
                    "__mth_i_dkfixx") == 0) {
    set_atomic_typecast_h2l(IL_DFIXK);
    bin_op = ILI_OPND(ILI_OPND(ILI_OPND(bin_op, 1), 2), 1);
    store_opcode = IL_STDP;
  } else if (ILI_OPC(bin_op) == IL_FIX || ILI_OPC(bin_op) == IL_UFIX ||
             ILI_OPC(bin_op) == IL_FIXK || ILI_OPC(bin_op) == IL_FIXUK) {
    set_atomic_typecast_h2l(ILI_OPC(bin_op));
    bin_op = ILI_OPND(bin_op, 1);
    store_opcode = IL_STSP;
  } else if (ILI_OPC(bin_op) == IL_DFIX || ILI_OPC(bin_op) == IL_DFIXU ||
             ILI_OPC(bin_op) == IL_DFIXK || ILI_OPC(bin_op) == IL_DFIXUK ||
             ILI_OPC(bin_op) == IL_SNGL) {
    set_atomic_typecast_h2l(ILI_OPC(bin_op));
    bin_op = ILI_OPND(bin_op, 1);
    store_opcode = IL_STDP;
  } else if (ILI_OPC(bin_op) == IL_DFRIR &&
             strcmp(SYMNAME(ILI_OPND(ILI_OPND(bin_op, 1), 1)),
                    "__mth_i_fixux") == 0) {
    // float 32bit to unsigned 32bit on 32bit machine x86 machine
    set_atomic_typecast_h2l(IL_UFIX);
    bin_op = ILI_OPND(ILI_OPND(ILI_OPND(bin_op, 1), 2), 1);
    store_opcode = IL_STSP;
  } else if (ILI_OPC(bin_op) == IL_DFRIR &&
             strcmp(SYMNAME(ILI_OPND(ILI_OPND(bin_op, 1), 1)),
                    "__mth_i_dfixux") == 0) {
    // float 32bit to unsigned 32bit on 32bit machine x86 machine
    set_atomic_typecast_h2l(IL_DFIXU);
    bin_op = ILI_OPND(ILI_OPND(ILI_OPND(bin_op, 1), 2), 1);
    store_opcode = IL_STDP;
  }
  // check if it is translated from float/double to unsigned
  else if (ILI_OPC(bin_op) == IL_KIMV) {
    bin_op = ILI_OPND(bin_op, 1);
    if (ILI_OPC(bin_op) == IL_FIXK) {
      set_atomic_typecast_h2l(IL_UFIX);
      store_opcode = IL_STSP;
    } else if (ILI_OPC(bin_op) == IL_DFIXK) {
      set_atomic_typecast_h2l(IL_DFIXU);
      store_opcode = IL_STDP;
    }
    bin_op = ILI_OPND(bin_op, 1);
  }

  if (store_opcode == IL_ST) {
    /* Look through the int opcodes */
    for (opc = 0; opc < num_int_opcodes; opc++) {
      if (int_atomic_opcodes[opc] == ILI_OPC(bin_op))
        break;
    }
    if (opc == num_int_opcodes) {
      return 0;
    }
    AtomicOp.atomic_operand = bin_op;
    AtomicOp.ldst_point = store_pt;
    AtomicOp.ldst_nme = store_nme;
    load_opcode = IL_LD;
  } else if (store_opcode == IL_STSP) {
    /* Look through the float opcodes */
    for (opc = 0; opc < num_float_opcodes; opc++) {
      if (float_atomic_opcodes[opc] == ILI_OPC(bin_op))
        break;
    }
    if (opc == num_float_opcodes) {
      return 0;
    }
    AtomicOp.atomic_operand = bin_op;
    AtomicOp.ldst_point = store_pt;
    AtomicOp.ldst_nme = store_nme;
    if (atomic_typecast_operand == IL_FIX || atomic_typecast_operand == IL_UFIX)
      load_opcode = IL_LD;
    else if (atomic_typecast_operand == IL_FIXK ||
             atomic_typecast_operand == IL_FIXUK)
      load_opcode = IL_LDKR;
    else
      load_opcode = IL_LDSP;
  } else if (store_opcode == IL_STDP) {
    /* Look through the double opcodes */
    for (opc = 0; opc < num_double_opcodes; opc++) {
      if (double_atomic_opcodes[opc] == ILI_OPC(bin_op))
        break;
    }
    if (opc == num_double_opcodes) {
      return 0;
    }
    AtomicOp.atomic_operand = bin_op;
    AtomicOp.ldst_point = store_pt;
    AtomicOp.ldst_nme = store_nme;
    if (atomic_typecast_operand == IL_DFIX ||
        atomic_typecast_operand == IL_DFIXU)
      load_opcode = IL_LD;
    else if (atomic_typecast_operand == IL_DFIXK ||
             atomic_typecast_operand == IL_DFIXUK)
      load_opcode = IL_LDKR;
    else if (atomic_typecast_operand == IL_SNGL)
      load_opcode = IL_LDSP;
    else
      load_opcode = IL_LDDP;
  } else if (store_opcode == IL_STKR) {
    for (opc = 0; opc < num_long_opcodes; opc++) {
      if (long_atomic_opcodes[opc] == ILI_OPC(bin_op))
        break;
    }
    if (opc == num_long_opcodes) {
      return 0;
    }
    AtomicOp.atomic_operand = bin_op;
    AtomicOp.ldst_point = store_pt;
    AtomicOp.ldst_nme = store_nme;
    load_opcode = IL_LDKR;
  } else if(store_opcode == IL_STSCMPLX) {
    for (opc = 0; opc < num_cmplx_opcodes; opc++) {
      if (cmplx_atomic_opcodes[opc] == ILI_OPC(bin_op))
        break;
    }
    if (opc == num_cmplx_opcodes) {
      return 0;
    }
    AtomicOp.atomic_operand = bin_op;
    AtomicOp.ldst_point = store_pt;
    AtomicOp.ldst_nme = store_nme;
    load_opcode = IL_LDSCMPLX;
  }

  op1 = ILI_OPND(bin_op, 1);
  if (ILI_OPC(op1) == IL_FLOAT || ILI_OPC(op1) == IL_DFLOAT ||
      ILI_OPC(op1) == IL_FLOATU || ILI_OPC(op1) == IL_DFLOATU ||
      ILI_OPC(op1) == IL_FLOATUK || ILI_OPC(op1) == IL_DFLOATUK ||
      ILI_OPC(op1) == IL_FLOATK || ILI_OPC(op1) == IL_DFLOATK ||
      ILI_OPC(op1) == IL_DBLE)
    op1 = ILI_OPND(op1, 1);
  // if the conversion is from unsigned integer to float/double
  // Locate the real LD
  if (ILI_OPC(op1) == IL_UIKMV)
    op1 = ILI_OPND(op1, 1);
  if ((ILI_OPC(op1) == IL_DFRSP || ILI_OPC(op1) == IL_DFRDP
#ifdef IL_DFRDPX87
       || ILI_OPC(op1) == IL_DFRDPX87
#endif
#ifdef IL_DFRSPX87
       || ILI_OPC(op1) == IL_DFRSPX87
#endif
       ) &&
      ILI_OPC(ILI_OPND(op1, 1)) == IL_QJSR &&
      (strcmp(SYMNAME(ILI_OPND(ILI_OPND(op1, 1), 1)), "__mth_i_dfloatuk") ==
           0 ||
       strcmp(SYMNAME(ILI_OPND(ILI_OPND(op1, 1), 1)), "__mth_i_floatuk") == 0 ||
       strcmp(SYMNAME(ILI_OPND(ILI_OPND(op1, 1), 1)), "__mth_i_floatux") == 0 ||
       strcmp(SYMNAME(ILI_OPND(ILI_OPND(op1, 1), 1)), "__mth_i_dfloatux") ==
           0 ||
       strcmp(SYMNAME(ILI_OPND(ILI_OPND(op1, 1), 1)), "__mth_i_dfloatk") == 0 ||
       strcmp(SYMNAME(ILI_OPND(ILI_OPND(op1, 1), 1)), "__mth_i_floatk") == 0)) {
    op1 = ILI_OPND(ILI_OPND(ILI_OPND(op1, 1), 2), 1);
  }

  op2 = ILI_OPND(bin_op, 2);
  if (ILI_OPC(op2) == IL_FLOAT || ILI_OPC(op2) == IL_DFLOAT ||
      ILI_OPC(op2) == IL_FLOATU || ILI_OPC(op2) == IL_DFLOATU ||
      ILI_OPC(op2) == IL_FLOATUK || ILI_OPC(op2) == IL_DFLOATUK ||
      ILI_OPC(op2) == IL_FLOATK || ILI_OPC(op2) == IL_DFLOATK ||
      ILI_OPC(op2) == IL_DBLE)
    op2 = ILI_OPND(op2, 1);
  // if the conversion is from unsigned integer to float/double
  // Locate the real LD
  if (ILI_OPC(op2) == IL_UIKMV)
    op2 = ILI_OPND(op2, 1);
  if ((ILI_OPC(op2) == IL_DFRSP || ILI_OPC(op2) == IL_DFRDP
#ifdef IL_DFRDPX87
       || ILI_OPC(op2) == IL_DFRDPX87
#endif
#ifdef IL_DFRSPX87
       || ILI_OPC(op2) == IL_DFRSPX87
#endif
       ) &&
      ILI_OPC(ILI_OPND(op2, 1)) == IL_QJSR &&
      (strcmp(SYMNAME(ILI_OPND(ILI_OPND(op2, 1), 1)), "__mth_i_dfloatuk") ==
           0 ||
       strcmp(SYMNAME(ILI_OPND(ILI_OPND(op2, 1), 1)), "__mth_i_floatuk") == 0 ||
       strcmp(SYMNAME(ILI_OPND(ILI_OPND(op2, 1), 1)), "__mth_i_floatux") == 0 ||
       strcmp(SYMNAME(ILI_OPND(ILI_OPND(op2, 1), 1)), "__mth_i_dfloatux") ==
           0 ||
       strcmp(SYMNAME(ILI_OPND(ILI_OPND(op2, 1), 1)), "__mth_i_dfloatk") == 0 ||
       strcmp(SYMNAME(ILI_OPND(ILI_OPND(op2, 1), 1)), "__mth_i_floatk") == 0)) {
    op2 = ILI_OPND(ILI_OPND(ILI_OPND(op2, 1), 2), 1);
  }

  if (ILI_OPC(op1) == IL_CSEIR) {
    /* Look through the CSEIR to the "real" load */
    op1 = ILI_OPND(op1, 1);
  }

  if (ILI_OPC(op1) == load_opcode) {
    load_pt1 = ILI_OPND(op1, 1);
  }

  if (ILI_OPC(op2) == load_opcode) {
    load_pt2 = ILI_OPND(op2, 1);
  }

  if (load_pt1 == 0 && load_pt2 == 0) {
    /* This is an error */
    return 0;
  }

  /*
   * If the operation operand 1 is the same as the store point, then we need the
   * second operand for the modifier for the atomic operation.
   */
  if (load_pt1 == store_pt) {
    AtomicOp.ili_operand = op2;
    return ILI_OPC(bin_op);
  }

  /*
   * If the operation operand 2 is the same as the store point, then we need the
   * first operand for the modifier for the atomic operation.
   */
  if (load_pt2 == store_pt) {
    AtomicOp.ili_operand = op1;
    is_atomic_operand1 = 1;
    return ILI_OPC(bin_op);
  }

  /* This is also an error */
  AtomicOp.atomic_operand = 0;
  return 0;
}

int
create_atomic_seq(int store_ili)
{
  int ili = store_ili;
  int arg, garg;
  int function;
  int store_symbol;
  int atomic_mod, op2, const_val, load_op, store_op, store_pt, load_pt,
      store_nme;
  int realilix, imagilix;
  ILI_OP intarg_opcode, floatarg_opcode, doublearg_opcode, longarg_opcode, scmplx_opcode;
  ILI_OP arg_opcode;
  int is_add;
  int arg_dt = 0;

#if defined(TARGET_X8664)
  intarg_opcode = IL_DAIR;
  floatarg_opcode = IL_DASP;
  doublearg_opcode = IL_DADP;
  longarg_opcode = IL_DAKR;
  scmplx_opcode = IL_DASP;
#else
  intarg_opcode = IL_ARGIR;
  floatarg_opcode = IL_ARGSP;
  doublearg_opcode = IL_ARGDP;
  longarg_opcode = IL_ARGKR;
  /* As now we only support single precision complex */
  scmplx_opcode = IL_ARGSP;
#endif

  atomic_mod = AtomicOp.ili_operand;
  store_pt = ILI_OPND(ili, 2);
  store_nme = ILI_OPND(ili, 3);

  if (ILI_OPC(store_pt) == IL_CSEAR) {
    store_pt = ILI_OPND(store_pt, 1);
  } else if (ILI_OPC(store_pt) == IL_ACON) {
    store_symbol = ILI_OPND(store_pt, 1);
    store_symbol = CONVAL1G(store_symbol);
    ADDRTKNP(store_symbol, 1);
  } else if (ILI_OPC(store_pt) == IL_AADD || ILI_OPC(store_pt) == IL_ASUB) {
    int acon_ili;
    acon_ili = ILI_OPND(store_pt, 1);
    if (ILI_OPC(acon_ili) != IL_ACON) {
      while (ILI_OPC(acon_ili) == IL_AADD || ILI_OPC(acon_ili) == IL_ASUB) {
        acon_ili = ILI_OPND(acon_ili, 1);
      }

      /* If the base is not a constant (perhaps it is a compiler temp)
       * then don't try and mark.
       */
      if (ILI_OPC(acon_ili) == IL_ACON) {
        store_symbol = ILI_OPND(acon_ili, 1);
        store_symbol = CONVAL1G(store_symbol);
        ADDRTKNP(store_symbol, 1);
      }
    } else {
      store_symbol = ILI_OPND(acon_ili, 1);
      store_symbol = CONVAL1G(store_symbol);
      ADDRTKNP(store_symbol, 1);
    }
  }

  arg = ad1ili(IL_NULL, 0);
  garg = ad1ili(IL_NULL, 0);
  if (is_atomic_typcast_h2l())
    function = get_atomic_function_ex(ILI_OPC(AtomicOp.atomic_operand));
  else
    function = get_atomic_function(ILI_OPC(AtomicOp.atomic_operand));

  switch (ILI_OPC(store_ili)) {
  case IL_ST:
    if (atomic_typecast_operand == IL_DFIX ||
        atomic_typecast_operand == IL_DFIXU) {
      arg_opcode = doublearg_opcode;
      arg_dt = DT_DBLE;
    }
    else if (atomic_typecast_operand == IL_FIX ||
             atomic_typecast_operand == IL_UFIX) {
      arg_opcode = floatarg_opcode;
      arg_dt = DT_FLOAT;
    }
    else  {
      arg_opcode = intarg_opcode;
      arg_dt = DT_INT;
    }
    break;
  case IL_STDP:
    arg_opcode = doublearg_opcode;
    arg_dt = DT_DBLE;
    break;
  case IL_STSP:
    if (atomic_typecast_operand == IL_SNGL) {
      arg_opcode = doublearg_opcode;
      arg_dt = DT_DBLE;
    }
    else {
      arg_opcode = floatarg_opcode;
      arg_dt = DT_FLOAT;
    }
    break;
  case IL_STKR:
    if (atomic_typecast_operand == IL_DFIXK ||
        atomic_typecast_operand == IL_DFIXUK) {
      arg_opcode = doublearg_opcode;
      arg_dt = DT_DBLE;
    }
    else if (atomic_typecast_operand == IL_FIXK ||
             atomic_typecast_operand == IL_FIXUK) {
      arg_opcode = floatarg_opcode;
      arg_dt = DT_FLOAT;
    }
    else {
      arg_opcode = longarg_opcode;
      arg_dt = DT_INT8;
    }
    break;
  case IL_STSCMPLX:
    arg_opcode = scmplx_opcode;
    arg_dt = DT_FLOAT;
    break;
  default:
    interr("Create: Unexpected atomic store opcode", ILI_OPC(store_ili), 3);
    break;
  }
#if defined(TARGET_X8664)
  if(ILI_OPC(store_ili) == IL_STSCMPLX)  {
    /* split the real and img parts */
    /* real part */
    realilix = ad1ili(IL_SCMPLX2REAL, atomic_mod);
    /* imag part */
    imagilix = ad1ili(IL_SCMPLX2IMAG, atomic_mod);
    /* call the ili utlity function to gen param list */
    initcallargs(3);
    addcallarg(store_pt, 0, DT_CPTR);
    addcallarg(realilix, 0, arg_dt);
    addcallarg(imagilix, 0, arg_dt);
    /* create argument list */
    arg = gencallargs();
  }
  else {
    initcallargs(2);
    addcallarg(store_pt, 0, DT_CPTR);
    addcallarg(atomic_mod, 0, arg_dt);
    arg = gencallargs();
  }
#else
  if(ILI_OPC(store_ili) == IL_STSCMPLX)  {
    /* split the real and img parts */
    /* real part */
    realilix = ad1ili(IL_SCMPLX2REAL, atomic_mod);
    /* imag part */
    imagilix = ad1ili(IL_SCMPLX2IMAG, atomic_mod);
    /* create argument list */
    arg = ad2ili(arg_opcode, imagilix, arg);
    arg = ad2ili(arg_opcode, realilix, arg);
  }
  else {
    arg = ad2ili(arg_opcode, atomic_mod, arg);
  }
  arg = ad3ili(IL_ARGAR, store_pt, arg, 0);
#endif
  arg = ad2ili(IL_JSR, function, arg);
  if(ILI_OPC(store_ili) == IL_STSCMPLX)  {
    /* split the real and img parts */
    /* real part */
    realilix = ad1ili(IL_SCMPLX2REAL, atomic_mod);
    /* imag part */
    imagilix = ad1ili(IL_SCMPLX2IMAG, atomic_mod);
    /* create argument list */
    garg = ad4ili(IL_GARG, imagilix, garg, arg_dt, 0);
    garg = ad4ili(IL_GARG, realilix, garg, arg_dt, 0);
  }
  else {
    garg = ad4ili(IL_GARG, atomic_mod, garg, arg_dt, 0);
  }
  garg = ad4ili(IL_GARG, store_pt, garg, DT_CPTR, store_nme);
  garg = ad3ili(IL_GJSR, function, garg, 0);
  ILI_ALT(arg) = garg;
  return arg;
}

LOGICAL
exp_end_atomic(int store, int curilm)
{
  if (is_in_atomic) {
    int atomic_opcode;
    atomic_opcode = get_atomic_update_opcode(store);
    if (atomic_opcode != 0) {
      if (get_atomic_store_created()) {
        error(155, 3, gbl.lineno, "Invalid atomic expression", CNULL);
      } else if (atomic_opcode != IL_FREEIR) {
        int atomic_seq;
        atomic_seq = create_atomic_seq(store);
        iltb.callfg = 1;
        chk_block(atomic_seq);
        ILM_RESULT(curilm) = atomic_seq;
        ILM_BLOCK(curilm) = expb.curbih;
        set_atomic_store_created(1);
        reset_atomic_typecast_h2l();
      } else {
        /* Is there anything to do with FREEIR */
      }
    } else {
      error(155, 3, gbl.lineno, "Invalid atomic expression", CNULL);
    }
    return TRUE;
  }
  if (is_in_atomic_read) {
    int atomic_opcode;
    atomic_opcode = get_atomic_read_opcode(store);
    if (atomic_opcode != 0) {
      if (get_atomic_store_created()) {
        error(155, 3, gbl.lineno, "Invalid atomic read expression", CNULL);
      } else if (atomic_opcode != IL_FREEIR) {
        int atomic_seq;
        atomic_seq = create_atomic_read_seq(store);
        iltb.callfg = 1;
        chk_block(atomic_seq);
        ILM_RESULT(curilm) = atomic_seq;
        ILM_BLOCK(curilm) = expb.curbih;
        set_atomic_store_created(1);
      } else {
        error(155, 3, gbl.lineno, "Invalid atomic read expression", CNULL);
      }
    } else {
      error(155, 3, gbl.lineno, "Invalid atomic read expression", CNULL);
    }
    return TRUE;
  }
  if (is_in_atomic_write) {
    int atomic_opcode;
    atomic_opcode = get_atomic_write_opcode(store);
    if (atomic_opcode != 0) {
      if (get_atomic_store_created()) {
        error(155, 3, gbl.lineno, "Invalid atomic write expression", CNULL);
      } else if (atomic_opcode != IL_FREEIR) {
        int atomic_seq;
        atomic_seq = create_atomic_write_seq(store);
        iltb.callfg = 1;
        chk_block(atomic_seq);
        ILM_RESULT(curilm) = atomic_seq;
        ILM_BLOCK(curilm) = expb.curbih;
        set_atomic_store_created(1);
      } else {
        error(155, 3, gbl.lineno, "Invalid atomic write expression", CNULL);
      }
    } else {
      error(155, 3, gbl.lineno, "Invalid atomic write expression", CNULL);
    }
    return TRUE;
  }
  if (is_in_atomic_capture) {
    int atomic_opcode;
    atomic_opcode = get_atomic_read_opcode(store);
    if (atomic_opcode != 0 && atomic_opcode != IL_FREEIR) {
      if (capture_read_ili != 0) {
        error(155, 3, gbl.lineno,
              "Invalid atomic capture block, multiple reads.", CNULL);
      } else {
        capture_read_ili = store;
        if (capture_update_ili != 0) {
          int atomic_seq;
          /* We have both parts of the capture, capture (write) is
           * not first. */
          atomic_seq = create_atomic_capture_seq(capture_update_ili,
                                                 capture_read_ili, 0);
          iltb.callfg = 1;
          chk_block(atomic_seq);
          ILM_RESULT(curilm) = atomic_seq;
          ILM_BLOCK(curilm) = expb.curbih;
          set_atomic_capture_created(1);
        }
      }
    }

    atomic_opcode = get_atomic_update_opcode(store);
    if (atomic_opcode != 0) {
      if (capture_update_ili != 0 && atomic_opcode != IL_FREEIR) {
        error(155, 3, gbl.lineno,
              "Invalid atomic capture block, multiple updates.", CNULL);
      } else if (atomic_opcode != IL_FREEIR) {
        capture_update_ili = store;
        if (capture_read_ili != 0) {
          /* We have both parts of the capture, capture (write) is
           * first. */
          int atomic_seq;
          atomic_seq = create_atomic_capture_seq(capture_update_ili,
                                                 capture_read_ili, 1);
          iltb.callfg = 1;
          chk_block(atomic_seq);
          ILM_RESULT(curilm) = atomic_seq;
          ILM_BLOCK(curilm) = expb.curbih;
          set_atomic_capture_created(1);
        }
      } else {
        /* Set the result of the FREEIR ILM to the regular
         * storage element (basically, a fallthrough).
         */
        iltb.callfg = 1;
        chk_block(store);
        ILM_RESULT(curilm) = store;
        ILM_BLOCK(curilm) = expb.curbih;
      }
    }
    return TRUE;
  }
  return FALSE;
}

#ifdef PD_IS_ATOMIC

/* Macro for generating case labels for GNU read-modify-write intrinsics. */
/* clang-format off */
#define EACH_SUBOP(s,t) \
       s##_add_##t: \
  case s##_sub_##t: \
  case s##_and_##t: \
  case s##_or_##t: \
  case s##_xor_##t
/* clang-format on */

/** Return MSZ for an atomic intrinsic */
static MSZ
msz_from_atomic_pd(PD_KIND pd)
{
  switch (pd) {
  default:
    assert(0, "msz_from_atomic_pd: pd not atomic or not implemented", pd, 4);

  case PD_atomic_load_1:
  case PD_atomic_store_1:
  case PD_atomic_exchange_1:
  case PD_atomic_compare_exchange_1:
  case EACH_SUBOP(PD_atomic_fetch, 1):
  case EACH_SUBOP(PD_atomic, fetch_1):
  case PD_atomic_test_and_set:
  case PD_atomic_clear:
    return MSZ_SBYTE;

  case PD_atomic_load_2:
  case PD_atomic_store_2:
  case PD_atomic_exchange_2:
  case PD_atomic_compare_exchange_2:
  case EACH_SUBOP(PD_atomic_fetch, 2):
  case EACH_SUBOP(PD_atomic, fetch_2):
    return MSZ_SHWORD;

  case PD_atomic_load_4:
  case PD_atomic_store_4:
  case PD_atomic_exchange_4:
  case PD_atomic_compare_exchange_4:
  case EACH_SUBOP(PD_atomic_fetch, 4):
  case EACH_SUBOP(PD_atomic, fetch_4):
    return MSZ_SWORD;

  case PD_atomic_load_8:
  case PD_atomic_store_8:
  case PD_atomic_exchange_8:
  case PD_atomic_compare_exchange_8:
  case EACH_SUBOP(PD_atomic_fetch, 8):
  case EACH_SUBOP(PD_atomic, fetch_8):
    return MSZ_SLWORD;

  case PD_atomic_thread_fence:
  case PD_atomic_signal_fence:
    return MSZ_UNDEF;
  }
}

/** Categorization of atomic intrinsics that abstracts out details. */
typedef enum ATOMIC_OP_CATEGORY {
  AOC_LOAD,
  AOC_STORE,
  AOC_EXCHANGE,
  AOC_COMPARE_EXCHANGE,
  AOC_OP_FETCH,
  AOC_FETCH_OP,
  AOC_TEST_AND_SET,
  AOC_CLEAR,
  AOC_FENCE
} ATOMIC_OP_CATEGORY;

/** struct that holds some ILI ops required to implement an atomic op. */
typedef struct ATOMIC_IMPL_OPS {
  /** atomic operation used to implement a given operation.
      Not set if value is implied by the corresponding AOC_... value. */
  ILI_OP atomic;
  /* Rest of fields set only for cmpxchg operations. */
  ILI_OP old; /**< IL_CMPXCHG_OLDx */
  ILI_OP st;  /**< IL_STx */
  ILI_OP ld;  /**< IL_LDx */
} ATOMIC_IMPL_OPS;

/** Given a PD_KIND, get its category and implementatino ops. */
static ATOMIC_OP_CATEGORY
atomic_op_category_from_pd(PD_KIND pd, ATOMIC_IMPL_OPS *ops)
{
  switch (pd) {
  default:
    interr("atomic_op_category_from_pd: pd not atomic or not implemented", pd,
           4);

  /* load */
  case PD_atomic_load_1:
  case PD_atomic_load_2:
  case PD_atomic_load_4:
    ops->atomic = IL_ATOMICLDI;
    return AOC_LOAD;
  case PD_atomic_load_8:
    ops->atomic = IL_ATOMICLDKR;
    return AOC_LOAD;

  /* store */
  case PD_atomic_store_1:
  case PD_atomic_store_2:
  case PD_atomic_store_4:
    ops->atomic = IL_ATOMICSTI;
    return AOC_STORE;
  case PD_atomic_store_8:
    ops->atomic = IL_ATOMICSTKR;
    return AOC_STORE;

  /* exchange */
  case PD_atomic_exchange_1:
  case PD_atomic_exchange_2:
  case PD_atomic_exchange_4:
    ops->atomic = IL_ATOMICRMWI;
    return AOC_EXCHANGE;
  case PD_atomic_exchange_8:
    ops->atomic = IL_ATOMICRMWKR;
    return AOC_EXCHANGE;

  /* compare_exchange */
  case PD_atomic_compare_exchange_1:
  case PD_atomic_compare_exchange_2:
  case PD_atomic_compare_exchange_4:
    ops->atomic = IL_CMPXCHGI;
    ops->ld = IL_LD;
    ops->st = IL_ST;
    ops->old = IL_CMPXCHG_OLDI;
    return AOC_COMPARE_EXCHANGE;
  case PD_atomic_compare_exchange_8:
    ops->atomic = IL_CMPXCHGKR;
    ops->ld = IL_LDKR;
    ops->st = IL_STKR;
    ops->old = IL_CMPXCHG_OLDKR;
    return AOC_COMPARE_EXCHANGE;

  /* fetch_op */
  case EACH_SUBOP(PD_atomic_fetch, 1):
  case EACH_SUBOP(PD_atomic_fetch, 2):
  case EACH_SUBOP(PD_atomic_fetch, 4):
    ops->atomic = IL_ATOMICRMWI;
    return AOC_FETCH_OP;
  case EACH_SUBOP(PD_atomic_fetch, 8):
    ops->atomic = IL_ATOMICRMWKR;
    return AOC_FETCH_OP;

  /* op_fetch */
  case EACH_SUBOP(PD_atomic, fetch_1):
  case EACH_SUBOP(PD_atomic, fetch_2):
  case EACH_SUBOP(PD_atomic, fetch_4):
    ops->atomic = IL_ATOMICRMWI;
    return AOC_OP_FETCH;
  case EACH_SUBOP(PD_atomic, fetch_8):
    ops->atomic = IL_ATOMICRMWKR;
    return AOC_OP_FETCH;

  /* test and set */
  case PD_atomic_test_and_set:
    return AOC_TEST_AND_SET;

  /* clear */
  case PD_atomic_clear:
    return AOC_CLEAR;

  /* fence */
  case PD_atomic_thread_fence:
  case PD_atomic_signal_fence:
    return AOC_FENCE;
  }
}

/** Return ATOMIC_RMW_OP for given predefined op that is either an atomic
    "op_fetch" or "fetch_op".  Set *replay to the operation required to "replay"
    the operation. */
static ILI_OP
atomic_rmw_op_from_pd(PD_KIND pd, ILI_OP *replay)
{
  switch (pd) {
  default:
    assert(0, "op_for_replay: pd not an atomic_op_fetch or not implemented", pd,
           4);

  case PD_atomic_fetch_add_1:
  case PD_atomic_fetch_add_2:
  case PD_atomic_fetch_add_4:
  case PD_atomic_add_fetch_1:
  case PD_atomic_add_fetch_2:
  case PD_atomic_add_fetch_4:
    *replay = IL_IADD;
    return AOP_ADD;
  case PD_atomic_fetch_add_8:
  case PD_atomic_add_fetch_8:
    *replay = IL_KADD;
    return AOP_ADD;

  case PD_atomic_fetch_sub_1:
  case PD_atomic_fetch_sub_2:
  case PD_atomic_fetch_sub_4:
  case PD_atomic_sub_fetch_1:
  case PD_atomic_sub_fetch_2:
  case PD_atomic_sub_fetch_4:
    *replay = IL_ISUB;
    return AOP_SUB;
  case PD_atomic_fetch_sub_8:
  case PD_atomic_sub_fetch_8:
    *replay = IL_KSUB;
    return AOP_SUB;

  case PD_atomic_fetch_and_1:
  case PD_atomic_fetch_and_2:
  case PD_atomic_fetch_and_4:
  case PD_atomic_and_fetch_1:
  case PD_atomic_and_fetch_2:
  case PD_atomic_and_fetch_4:
    *replay = IL_AND;
    return AOP_AND;

  case PD_atomic_fetch_and_8:
  case PD_atomic_and_fetch_8:
    *replay = IL_KAND;
    return AOP_AND;

  case PD_atomic_fetch_or_1:
  case PD_atomic_fetch_or_2:
  case PD_atomic_fetch_or_4:
  case PD_atomic_or_fetch_1:
  case PD_atomic_or_fetch_2:
  case PD_atomic_or_fetch_4:
    *replay = IL_OR;
    return AOP_OR;
  case PD_atomic_fetch_or_8:
  case PD_atomic_or_fetch_8:
    *replay = IL_KOR;
    return AOP_OR;

  case PD_atomic_fetch_xor_1:
  case PD_atomic_fetch_xor_2:
  case PD_atomic_fetch_xor_4:
  case PD_atomic_xor_fetch_1:
  case PD_atomic_xor_fetch_2:
  case PD_atomic_xor_fetch_4:
    *replay = IL_XOR;
    return AOP_XOR;
  case PD_atomic_fetch_xor_8:
  case PD_atomic_xor_fetch_8:
    *replay = IL_KXOR;
    return AOP_XOR;
  }
}

/** Object that assists generation of temporaries.
    See functions auto_stash and auto_retrieve for how it is used. */
typedef struct auto_temp {
  int expr; /**< An ilix for a store into a temporary, or ilix of a constant. */
} auto_temp;

/** \brief Generate ILI so that value of an ILI expression can be retrieved
   later.

    \param temp pointer to object that remembers how to recover the value
    \param ilix ILI expression to be stashed/retrieved
    \param st_op IL_STx operation to be used to store value if necessary
    \param msz machine size of value to be stored.
  */
static void
auto_stash(auto_temp *temp, int ilix, ILI_OP st_op, MSZ msz)
{
  int nme, acon, store;
  SPTR sym;
  switch (ILI_OPC(ilix)) {
  case IL_ACON:
  case IL_ICON:
    /* Do not need a temporary */
    temp->expr = ilix;
    return;
  default:
    break;
  }
  sym = mkrtemp(ilix);
  acon = ad_acon(sym, (INT)0);
  nme = addnme(NT_VAR, sym, 0, (INT)0);
  store = ad4ili(st_op, ilix, acon, nme, msz);
  chk_block(store);
  temp->expr = store;
}

/** \brief Generate ILI to retrieve previously stashed value.

    \param temp pointer to object set by routine auto_stash
  */
static int
auto_retrieve(auto_temp *temp)
{
  switch (IL_TYPE(ILI_OPC(temp->expr))) {
  default:
    interr("auto_retrieve: unexpected IL_TYPE", IL_TYPE(temp->expr), 4);
  case ILTY_STORE:
  case ILTY_PSTORE:
    return ad_load(temp->expr);
  case ILTY_CONS:
    return temp->expr;
  }
}

/* \brief Expand a GNU atomic intrinsic.

   \param pd - a PD_... value from pd.h for which PD_IS_ATOMIC is true.
   \param ilmp - pointer to call site for an atomic intrinsic */
void
exp_atomic_intrinsic(PD_KIND pd, ILM *ilmp, int curilm)
{
  int i, n;
  int opnd[6]; /* ILI "ptrs".  Most is 6, for __atomic_compare_exchange_... */
  int nme[6];
  int callee_index = ilm_callee_index(ilmp->opc);
  int stc, result;
  ILI_OP ili_op_for_replay;
  ATOMIC_IMPL_OPS ops;
  MSZ msz;
  ATOMIC_OP_CATEGORY aoc = atomic_op_category_from_pd(pd, &ops);
  DEBUG_ASSERT(ilmp->opc == IM_FAPPLY || ilmp->opc == IM_VAPPLY,
               "atomic ops cannot throw");

  /* Get # of arguments */
  n = ILM_OPND(ilmp, 1);
  /* FIXME - do we need to check argument count and issue error message to
     user if there are the wrong number of arguments, or did the front-end
     already deal with that? */
  DEBUG_ASSERT(0 <= n && n <= 6, "exp_atomic_intrinsic: bad ILM");
  for (i = 0; i < n; ++i) {
    int ilmx = ILM_OPND(ilmp, callee_index + 1 + i); /* locates ARG ilm */
    ILM *ilmpx = (ILM *)(ilmb.ilm_base + ilmx);
    ilmx = ILM_OPND(ilmpx, 2);
    nme[i] = NME_OF(ilmx);
    opnd[i] = ILI_OF(ilmx);
  }
  msz = msz_from_atomic_pd(pd);

  switch (aoc) {
  default:
    assert(false, "exp_atomic_intrinsic: unimplemented op class", aoc, 4);

  case AOC_LOAD:
    stc = atomic_encode(msz, SS_PROCESS, AORG_CPLUS);
    result = ad4ili(ops.atomic, opnd[0], nme[0], stc, opnd[1]);
    break;

  case AOC_STORE:
    stc = atomic_encode(msz, SS_PROCESS, AORG_CPLUS);
    result = ad5ili(ops.atomic, opnd[1], opnd[0], nme[0], stc, opnd[2]);
    chk_block(result);
    break;

  case AOC_EXCHANGE:
    stc = atomic_encode_rmw(msz, SS_PROCESS, AORG_CPLUS, AOP_XCHG);
    result = ad5ili(ops.atomic, opnd[1], opnd[0], nme[0], stc, opnd[2]);
    break;

  case AOC_COMPARE_EXCHANGE: {
    int expected_ptr, comparand, cmpxchg, succ, oldval;
    int comparand_nme, label;
    auto_temp expected_ptr_save, oldval_save, succ_save;
    stc = atomic_encode(msz, SS_PROCESS, AORG_CPLUS);

    /* Get the comparand ("expected") */
    comparand_nme = addnme(NT_IND, 0, nme[1], (INT)0);
    comparand = ad3ili(ops.ld, opnd[1], comparand_nme, msz);

    /* Save the expected_ptr */
    expected_ptr = ad_cse(opnd[1]);
    auto_stash(&expected_ptr_save, expected_ptr, IL_STA, MSZ_PTR);

    /* Do the compare-exchange */
    cmpxchg = ad_cmpxchg(ops.atomic, opnd[2], opnd[0], nme[0], stc, comparand,
                         opnd[3], opnd[4], opnd[5]);

    /* Stash old value returned by cmpxchg */
    oldval = ad1ili(ops.old, cmpxchg);
    auto_stash(&oldval_save, oldval, ops.st, msz);

    /* Stash success flag returned by cmpxchg */
    succ = ad1ili(IL_CMPXCHG_SUCCESS, cmpxchg);
    succ = ad_cse(succ);
    auto_stash(&succ_save, succ, IL_ST, MSZ_SBYTE);

    /* Branch on success. */
    label = getlab();
    chk_block(ad3ili(IL_ICJMPZ, succ, CC_NE, label));

    /* Store old value into *expected_ptr. */
    expected_ptr = auto_retrieve(&expected_ptr_save);
    oldval = auto_retrieve(&oldval_save);
    chk_block(ad4ili(ops.st, oldval, expected_ptr, nme[1], msz));

    /* Emit label */
    wr_block();
    cr_block();
    BIH_LABEL(expb.curbih) = label;
    ILIBLKP(label, expb.curbih);
    RFCNTP(label, 1);

    /* Result is value of flag. */
    result = auto_retrieve(&succ_save);
  } break;

  case AOC_FETCH_OP:
    /* FIXME - should use IL_ATOMICRMWA if first operand is pointer to
       pointer.  Use DTYPE to tell?  Or is IL_ATOMICRMWA fundmentally a bad
       idea? */
    stc = atomic_encode_rmw(msz, SS_PROCESS, AORG_CPLUS,
                            atomic_rmw_op_from_pd(pd, &ili_op_for_replay));
    result = ad5ili(ops.atomic, opnd[1], opnd[0], nme[0], stc, opnd[2]);
    break;

  case AOC_OP_FETCH:
    stc = atomic_encode_rmw(msz, SS_PROCESS, AORG_CPLUS,
                            atomic_rmw_op_from_pd(pd, &ili_op_for_replay));
    /* Need to "replay" operation to get final result, so we need to use opnd[1]
     * twice. */
    opnd[1] = ad_cse(opnd[1]);
    result = ad5ili(ops.atomic, opnd[1], opnd[0], nme[0], stc, opnd[2]);
    result = ad2ili(ili_op_for_replay, result, opnd[1]);
    break;

  case AOC_TEST_AND_SET:
    /* Treat as atomic exchange on a byte. */
    stc = atomic_encode_rmw(msz, SS_PROCESS, AORG_CPLUS, AOP_XCHG);
    result = ad5ili(IL_ATOMICRMWI, ad_icon(1), opnd[0], nme[0], stc, opnd[1]);
    break;

  case AOC_CLEAR:
    /* Treat as atomic store of a zero byte. */
    stc = atomic_encode(msz, SS_PROCESS, AORG_CPLUS);
    result = ad5ili(IL_ATOMICSTI, ad_icon(0), opnd[0], nme[0], stc, opnd[1]);
    chk_block(result);
    break;

  case AOC_FENCE: {
    SYNC_SCOPE ss = pd == PD_atomic_signal_fence ? SS_SINGLETHREAD : SS_PROCESS;
    stc = atomic_encode(MSZ_UNDEF, ss, AORG_CPLUS);
    result = ad2ili(IL_FENCE, stc, opnd[0]);
  } break;
  }
  if (ilmp->opc == IM_VAPPLY) {
    /* result not used */
    if (aoc != AOC_FENCE && aoc != AOC_STORE && aoc != AOC_CLEAR)
      result = ad_free(result);
    chk_block(result);
  } else {
    DEBUG_ASSERT(aoc != AOC_FENCE, "IM_VAPPLY expected for fence intrinsics");
    ILM_RESULT(curilm) = result;
  }
}
#endif
