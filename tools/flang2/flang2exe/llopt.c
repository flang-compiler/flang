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

/**
   \file
   \brief optimization/peephole/inst simplification routines for LLVM Code
   Generator
 */

#include "gbldefs.h"
#include "global.h"
#include "error.h"
#include "symtab.h"
#include "llutil.h"
#include "cgllvm.h"
#include "ili.h"
#include <stdlib.h>

#define DEC_UCOUNT(i) ((i)->tmps->use_count--)

static void
replace_by_call_to_llvm_instrinsic(INSTR_LIST *instr, char *fname,
                                   OPERAND *params)
{
  OPERAND *call_op;
  char *intrinsic_name;
  static char buf[MAXIDLEN];
  LL_Type *return_ll_type = NULL;

  DBGXTRACEIN1(DBGBIT(12, 0x20), 1, "ilix %d", instr->ilix);

  intrinsic_name = (char *)getitem(LLVM_LONGTERM_AREA, strlen(fname) + 1);
  strcpy(intrinsic_name, fname);
  return_ll_type = instr->ll_type;
  instr->i_name = I_PICALL;
  instr->flags = 0;
  instr->flags |= CALL_INTRINSIC_FLAG;
  call_op = make_operand();
  call_op->ot_type = OT_CALL;
  call_op->string = intrinsic_name;
  call_op->ll_type = return_ll_type;
  instr->operands = call_op;
  call_op->next = params;
  /* add global define of llvm.xxx to external function list, if needed */
  sprintf(buf, "declare %s %s(", return_ll_type->str, intrinsic_name);
  if (params) {
    sprintf(buf, "%s%s", buf, params->ll_type->str);
    params = params->next;
  }
  while (params) {
    sprintf(buf, "%s, %s", buf, params->ll_type->str);
    params = params->next;
  }
  strcat(buf, ")");
  update_external_function_declarations(buf, EXF_INTRINSIC);

  DBGXTRACEOUT1(DBGBIT(12, 0x20), 1, " %s", buf)
}

static void
update_param_use_count(OPERAND *params)
{
  while (params) {
    if (params->ot_type == OT_TMP) {
      params->tmps->use_count++;
    }
    params = params->next;
  }
}

static void
replace_by_fma_intrinsic(INSTR_LIST *instr, OPERAND *op, INSTR_LIST *mul_instr)
{
  OPERAND *params;
  char *intrinsic_name = NULL;

  switch (instr->ll_type->data_type) {
  case LL_FLOAT:
    if (instr->i_name == I_FADD)
      intrinsic_name = "@llvm.pgi.arm.vmla.f32";
    else if (instr->i_name == I_FSUB)
      intrinsic_name = "@llvm.pgi.arm.vmls.f32";
    break;
  case LL_DOUBLE:
    if (instr->i_name == I_FADD)
      intrinsic_name = "@llvm.pgi.arm.vmla.f64";
    else if (instr->i_name == I_FSUB)
      intrinsic_name = "@llvm.pgi.arm.vmls.f64";
    break;
  default:
    break;
  }
  if (intrinsic_name) {
    params = gen_copy_op(op);
    params->next = gen_copy_list_op(mul_instr->operands);
    update_param_use_count(params);
    replace_by_call_to_llvm_instrinsic(instr, intrinsic_name, params);
    DEC_UCOUNT(mul_instr);
  }
}

static INSTR_LIST *
is_from_instr(int i_name, OPERAND *op)
{
  if (op->ot_type == OT_TMP) {
    INSTR_LIST *idef;
    idef = op->tmps->info.idef;
    if (idef && (idef->i_name == i_name))
      return idef;
  }
  return NULL;
}

static void
optimize_instruction(INSTR_LIST *instr)
{
  INSTR_LIST *op_instr;

  if (instr->tmps && instr->tmps->use_count == 0)
    return;
  switch (instr->i_name) {
  default:
    break;
  case I_FADD:
    if ((op_instr = is_from_instr(I_FMUL, instr->operands)))
      replace_by_fma_intrinsic(instr, instr->operands, op_instr);
    else if ((op_instr = is_from_instr(I_FMUL, instr->operands->next)))
      replace_by_fma_intrinsic(instr, instr->operands->next, op_instr);
    break;
  case I_FSUB:
    if ((op_instr = is_from_instr(I_FMUL, instr->operands->next)))
      replace_by_fma_intrinsic(instr, instr->operands->next, op_instr);
    break;
  }
}

void
optimize_block(INSTR_LIST *last_block_instr)
{
#ifdef TARGET_LLVM_ARM
  INSTR_LIST *instr, *last_instr;

  last_instr = NULL;
  for (instr = last_block_instr; instr; instr = instr->prev) {
    instr->flags |= INST_VISITED;

    if (last_instr == NULL && instr->i_name == I_NONE)
      last_instr = instr;
    if (instr->flags & STARTEBB) {
      if (last_instr != NULL)
        break;
    }
  }

  for (instr = last_block_instr; instr; instr = instr->prev) {
    optimize_instruction(instr);

    if (last_instr == NULL && instr->i_name == I_NONE)
      last_instr = instr;
    if (instr->flags & STARTEBB) {
      if (last_instr != NULL)
        break;
    }
  }

  for (instr = last_block_instr; instr; instr = instr->prev) {
    instr->flags &= ~INST_VISITED;

    if (last_instr == NULL && instr->i_name == I_NONE)
      last_instr = instr;
    if (instr->flags & STARTEBB) {
      if (last_instr != NULL)
        break;
    }
  }
#endif
}

/**
   \brief Determine if \p cand has the form <tt>1.0 / y</tt>
 */
static LOGICAL
is_recip(OPERAND *cand)
{
  if (cand && cand->tmps) {
    INSTR_LIST *il = cand->tmps->info.idef;
    const int divIli = il ? il->ilix : 0;
    OPERAND *ilOp = divIli ? il->operands : NULL;
    if (ilOp && (cand->tmps->use_count == 1) && (il->i_name == I_FDIV) &&
        (ilOp->ot_type == OT_CONSTSPTR)) {
      const int sptr = ilOp->val.sptr;
      switch (ILI_OPC(ILI_OPND(divIli, 1))) {
      case IL_FCON:
        return sptr == stb.flt1;
      case IL_DCON:
        return sptr == stb.dbl1;
      default:
        break;
      }
    }
  }
  return FALSE;
}

/**
   \brief Helper function
   \param x  This is the \c x operand in a <tt>(/ x)</tt> insn [precondition]
   \param recip  The <tt>(/ 1.0 y)</tt> term for splicing

   Peephole rewrite of the bridge IR. The C compiler will DCE the unused div
   operation. The C++ compiler will not, but instead leans on LLVM to DCE the
   <tt>(/ 1.0 undef)</tt> operation.
 */
static void
fixup_recip_div(OPERAND *x, OPERAND *recip)
{
  INSTR_LIST *il = recip->tmps->info.idef; // il <- (/ 1.0 y)
  OPERAND *undef = make_undef_op(il->operands->next->ll_type);
  x->next = il->operands->next; // (/ x) ==> (/ x y)
  il->operands->next = undef;   // (/ 1.0 y) ==> (/ 1.0 undef)
  recip->tmps->use_count--;
}

/**
   \brief Translate a fp mul to a fp div ILI opcode
   \param opc  The opcode to translate
   \return The DIV form if \c opc is a FP MUL, otherwise \c opc itself

   NB: Used to overwrite the opcode in the ILI. Any subsequent passes (FMA) that
   examine the ILI must not conclude that this is still a multiply operation.
 */
static int
convert_mul_to_div(int opc)
{
  switch (opc) {
  case IL_FMUL:
    return IL_FDIV;
  case IL_DMUL:
    return IL_DDIV;
  default:
    break;
  }
  return opc;
}

/**
   \brief Translate <tt>x * 1.0 / y</tt> to <tt>x / y</tt>.
   \param mul  A FP multiply instruction

   Preconditions: \p mul is a well-formed I_FMUL, has a positive use count
 */
void
maybe_undo_recip_div(INSTR_LIST *mul)
{
  OPERAND *lop = mul->operands;
  OPERAND *rop = lop->next;

  if (is_recip(lop)) {
    // case: (1.0 / y) * x
    mul->i_name = I_FDIV;
    ILI_OPC(mul->ilix) = convert_mul_to_div(ILI_OPC(mul->ilix));
    mul->operands = rop; // x
    fixup_recip_div(rop, lop);
  } else if (is_recip(rop)) {
    // case: x * (1.0 / y)
    mul->i_name = I_FDIV;
    ILI_OPC(mul->ilix) = convert_mul_to_div(ILI_OPC(mul->ilix));
    fixup_recip_div(lop, rop);
  } else {
    // mul not recognized as a mult-by-recip form
    // ok, do nothing
  }
}
