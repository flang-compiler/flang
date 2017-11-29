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
 * \brief Fortran-specific expander routines
 */

#include "gbldefs.h"
#include "error.h"
#include "global.h"
#include "symtab.h"
#include "regutil.h"
#include "machreg.h"
#include "ilm.h"
#include "fih.h"
#include "ilmtp.h"
#include "ili.h"
#define EXPANDER_DECLARE_INTERNAL
#include "expand.h"
#include "machar.h"
#define RTE_C
#include "rte.h"
#undef RTE_C
#include "x86.h"
#include "pragma.h"
#include "rtlRtns.h"

extern LOGICAL func_in(int);
extern int charlen(int);
extern int charaddr(int);
extern int ad2func_kint(int, char *, int, int);
extern void exp_header(int);
extern void exp_end(ILM *, int, LOGICAL);
extern void exp_qjsr(char *, int, ILM *, int);
extern void ref_threadprivate_var(int, int *, int *, int);
extern void exp_agoto(ILM *, int);
extern void exp_cgoto(ILM *, int);
extern void set_is_in_atomic_capture(int);
extern void set_atomic_capture_created(int);
extern int get_atomic_capture_created(void);
extern void set_atomic_store_created(int);
extern int get_atomic_store_created(void);
extern void set_is_in_atomic_capture(int);
extern void ccff_open_unit(void);
extern void ref_threadprivate(int, int *, int *);
int exp_get_sdsc_len(int, int, int);
extern LOGICAL is_llvm_local_private(int);
#ifdef DEBUG
extern void dumpdtype(int);
extern void dilitre(int);
#endif

static void begin_entry(int); /* interface to exp_header */
static void store_aret(int);
static int vf_addr;    /* addr of temp environ for var.fmt. funcs */
static int entry_sptr; /* entry (primary or secondary) sptr processed
                        * by begin_entry() -- IM_ENLAB needs the
                        * sptr.
                        */
static int arglist = 0;
static int accreduct_op;

/*
 * MTH, FMTH, ... names
 */
#include "mth.h"

#define mk_prototype mk_prototype_llvm

static int
forceK(int ili)
{
  int ilix;

  ilix = ili;
  if (XBIT(124, 0x400)) {
    ilix = ikmove(ilix);
  }
  return ilix;
}

static int
double_is_small_int(int ilix)
{
  int sptr;
  int i;
  double d;
  INT con[2];
  int ret_ili = 0; /* false */

  assert((ILI_OPC(ilix) == IL_DCON), "double_is_small_int expects dcon", ilix,
         3);

  sptr = ILI_OPND(ilix, 1);
  if (sptr == stb.dbl2) {
    ret_ili = ad_icon(2);
  } else {
    /* probably a better way to do this */
    for (i = 3; i < 7; i++) {
      d = (double)i;
      xmdtod(d, con);
      if (con[0] == CONVAL1G(sptr) && con[1] == CONVAL2G(sptr)) {
        ret_ili = ad_icon(i);
        break;
      }
    }
  }
  return ret_ili;
}
void
exp_ac(ILM_OP opc, ILM *ilmp, int curilm)
{
  int opcx;
  ILM *ilmpx;
  int ilmx;
  int tmp;
  int nme, nmsym;
  INT val[2];
  int ilix, ilixi, ilixr;
  int op1, op2, op3;
  /*
   * the ILMs here are special cased so we can perform some special
   * transformations, modify the calling sequence, or pass back
   * a names entry.
   */
  nme = 0;
  switch (opc) {
  default:
    interr("exp_ac:ilm not cased", opc, 3);
    return;
  case IM_LNOT:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    if (XBIT(125, 0x8))
      ilix = ad2ili(IL_XOR, op1, ad_icon(1)); /*  1 => true */
    else
      ilix = ad1ili(IL_NOT, op1); /* -1 => true */
    ILM_RESULT(curilm) = ilix;
    return;
  case IM_LEQV:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    if (XBIT(125, 0x8))
      /* -Munixlogical */
      ilix = ad3ili(IL_ICMP, op1, op2, CC_EQ);
    else {
      ilix = ad2ili(IL_LEQV, op1, op2);
    }
    ILM_RESULT(curilm) = ilix;
    return;
  case IM_LNOT8:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    if (XBIT(124, 0x400)) {
      if (XBIT(125, 0x8)) {
        val[0] = 0;
        val[1] = 1;
        ilix = ad1ili(IL_KCON, getcon(val, DT_INT8));
        ilix = ad2ili(IL_KXOR, op1, ilix); /*  1 => true */
      } else
        ilix = ad1ili(IL_KNOT, op1); /* -1 => true */
    } else if (XBIT(125, 0x8))
      ilix = ad2ili(IL_XOR, op1, ad_icon(1)); /*  1 => true */
    else
      ilix = ad1ili(IL_NOT, op1); /* -1 => true */
    ILM_RESULT(curilm) = ilix;
    return;
  case IM_LNOP8:
    ilix = ILI_OF(ILM_OPND(ilmp, 1));
    /* next line is never used, so I have commented it out */
    /* tmp = ad2ili(IL_MVKR, op1, KR_RETVAL); */
    /* fix tpr 1510. If the LNOP8 points to a nonK result,
     * need to insert an IKMV.
     */
    ILM_RESULT(curilm) = forceK(ilix);
    return;
  case IM_KADD:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    if (XBIT(124, 0x400))
      ilix = ad2ili(IL_KADD, op1, op2);
    else {
      op1 = kimove(op1);
      op2 = kimove(op2);
      ilix = ad2ili(IL_IADD, op1, op2);
    }
    ILM_RESULT(curilm) = ilix;
    return;
  case IM_KSUB:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    if (XBIT(124, 0x400))
      ilix = ad2ili(IL_KSUB, op1, op2);
    else {
      op1 = kimove(op1);
      op2 = kimove(op2);
      ilix = ad2ili(IL_ISUB, op1, op2);
    }
    ILM_RESULT(curilm) = ilix;
    return;
  case IM_KMUL:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    if (XBIT(124, 0x400))
      ilix = ad2ili(IL_KMUL, op1, op2);
    else {
      op1 = kimove(op1);
      op2 = kimove(op2);
      ilix = ad2ili(IL_IMUL, op1, op2);
    }
    ILM_RESULT(curilm) = ilix;
    return;
  case IM_KDIV:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    if (XBIT(124, 0x400))
      ilix = ad2ili(IL_KDIV, op1, op2);
    else {
      op1 = kimove(op1);
      op2 = kimove(op2);
      ilix = ad2ili(IL_IDIV, op1, op2);
    }
    ILM_RESULT(curilm) = ilix;
    return;
  case IM_KPOPCNT:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    if (XBIT(124, 0x400))
      ilix = ad1ili(IL_KPOPCNT, op1);
    else {
      op1 = kimove(op1);
      ilix = ad1ili(IL_IPOPCNT, op1);
    }
    ILM_RESULT(curilm) = ilix;
    return;
  case IM_KLEADZ:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    if (XBIT(124, 0x400))
      ilix = ad1ili(IL_KLEADZ, op1);
    else {
      op1 = kimove(op1);
      ilix = ad1ili(IL_ILEADZ, op1);
    }
    ILM_RESULT(curilm) = ilix;
    return;
  case IM_KPOPPAR:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    if (XBIT(124, 0x400))
      ilix = ad1ili(IL_KPOPPAR, op1);
    else {
      op1 = kimove(op1);
      ilix = ad1ili(IL_IPOPPAR, op1);
    }
    ILM_RESULT(curilm) = ilix;
    return;
  case IM_RDIV:
    tmp = exp_mac(IM_RDIV, ilmp, curilm);
    return;
  case IM_DDIV:
    tmp = exp_mac(IM_DDIV, ilmp, curilm);
    return;

  case IM_REAL:
    if (XBIT(70, 0x40000000)) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      ilix = ad1ili(IL_SCMPLX2REAL, op1);
      ILM_RESULT(curilm) = ilix;
      return;
    }
    tmp = exp_mac(opc, ilmp, curilm);
    return;
  case IM_IMAG:
    if (XBIT(70, 0x40000000)) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      ilix = ad1ili(IL_SCMPLX2IMAG, op1);
      ILM_RESULT(curilm) = ilix;
      return;
    }
    tmp = exp_mac(opc, ilmp, curilm);
    return;
  case IM_DREAL:
    if (XBIT(70, 0x40000000)) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      ilix = ad1ili(IL_DCMPLX2REAL, op1);
      ILM_RESULT(curilm) = ilix;
      return;
    }
    tmp = exp_mac(opc, ilmp, curilm);
    return;
  case IM_DIMAG:
    if (XBIT(70, 0x40000000)) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      ilix = ad1ili(IL_DCMPLX2IMAG, op1);
      ILM_RESULT(curilm) = ilix;
      return;
    }
    tmp = exp_mac(opc, ilmp, curilm);
    return;
  case IM_CMPLX:
    ilixr = ILI_OF(ILM_OPND(ilmp, 1)); /* real part */
    ilixi = ILI_OF(ILM_OPND(ilmp, 2)); /* imag part */
    /***********************************************************
     * scn (03 Oct 2014): -0.0 is not considered to be 0.0 here
     * and below
     ***********************************************************/
    if (XBIT(70, 0x40000000)) {
      if (ILI_OPC(ilixi) == IL_FCON && ILI_OPND(ilixi, 1) == stb.flt0)
        ilix = ad1ili(IL_SPSP2SCMPLXI0, ilixr);
      else
        ilix = ad2ili(IL_SPSP2SCMPLX, ilixr, ilixi);
      ILM_RESULT(curilm) = ilix;
    } else
    {
      ILM_RRESULT(curilm) = ilixr;
      ILM_IRESULT(curilm) = ilixi;
      ILM_RESTYPE(curilm) = ILM_ISCMPLX;
    }
    return;
  case IM_DCMPLX:
    ilixr = ILI_OF(ILM_OPND(ilmp, 1)); /* real part */
    ilixi = ILI_OF(ILM_OPND(ilmp, 2)); /* imag part */
    if (XBIT(70, 0x40000000)) {
      if (ILI_OPC(ilixi) == IL_DCON && ILI_OPND(ilixi, 1) == stb.dbl0)
        ilix = ad1ili(IL_DPDP2DCMPLXI0, ilixr);
      else
        ilix = ad2ili(IL_DPDP2DCMPLX, ilixr, ilixi);
      ILM_RESULT(curilm) = ilix;
    } else
    {
      ILM_RRESULT(curilm) = ilixr;
      ILM_IRESULT(curilm) = ilixi;
      ILM_RESTYPE(curilm) = ILM_ISDCMPLX;
    }
    return;
  case IM_ITOSC:
    val[1] = size_of(DT_BINT);
    goto sconv_shared;
  case IM_ITOS:
    val[1] = size_of(DT_SINT);
  sconv_shared:
    op1 = ILI_OF(ILM_OPND(ilmp, 1)); /* ili to be converted */
    /*
     * if truncation is requested when narrowing to a smaller signed type,
     * generate "code" to left shift, then arithmetically right shift (addili
     * will take care of constants).  another condition to handle is a constant
     * operand of an intrinsic (earlier processing does not catch this case -
     * tpr555)
     */
    if (XBIT(124, 1) || (ILI_OPC(op1) == IL_ICON)) {
      val[1] = size_of(DT_INT) - val[1]; /* difference in bytes */
      val[1] <<= 3;                      /* difference in bits */
      op2 = ad_icon(val[1]);
      tmp = ad2ili(IL_LSHIFT, op1, op2);
      op1 = ad2ili(IL_ARSHIFT, tmp, op2);
    }
    ILM_RESULT(curilm) = op1;
    return;

  /* complex arithmetics/intinsics */
  case IM_CABS:
    if (XBIT(70, 0x40000000)) {
      int r = ILM_RESULT(ILM_OPND(ilmp, 1));
      op1 = ad1ili(IL_DBLE, ad1ili(IL_SCMPLX2IMAG, r));
      op2 = ad1ili(IL_DBLE, ad1ili(IL_SCMPLX2REAL, r));
      op1 = ad2ili(IL_DMUL, op1, op1);
      op2 = ad2ili(IL_DMUL, op2, op2);
      tmp = ad2ili(IL_DADD, op1, op2);
      tmp = ad1ili(IL_DSQRT, tmp);
      tmp = ad1ili(IL_SNGL, tmp);
      ILM_RESULT(curilm) = tmp;
    } else
      tmp = exp_mac(IM_CABS, ilmp, curilm);
    return;
  case IM_CDABS:
    if (XBIT(70, 0x40000000)) {
      int r = ILM_RESULT(ILM_OPND(ilmp, 1));
      op1 = ad1ili(IL_DCMPLX2IMAG, r);
      op2 = ad1ili(IL_DCMPLX2REAL, r);
      tmp = ad1ili(IL_NULL, 0);
      tmp = ad3ili(IL_DADP, op1, DP(0), tmp);
      tmp = ad3ili(IL_DADP, op2, DP(1), tmp);
      op3 = mk_prototype("__mth_i_cdabs", "pure", DT_DBLE, 2, DT_DBLE, DT_DBLE);
      tmp = ad2ili(IL_QJSR, op3, tmp);
      ILM_RESULT(curilm) = ad2ili(IL_DFRDP, tmp, DP_RETVAL);
    } else
      tmp = exp_mac(IM_CDABS, ilmp, curilm);
    return;
  /*
   * For the old calling sequence, all arithmetic/intrinsic QJSRs which
   * return complex are turned into regular complex function calls where the
   * result of the function is returned via a pointer passed as an extra arg.
   *
   * Currently, the C ABI for complex is only used for native -- enabling for
   * LLVM targets will occur when the new complex ILI is fully supported.
   */
  case IM_CTOI:
    if (XBIT(70, 0x40000000) && XBIT_NEW_MATH_NAMES_CMPLX) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      op2 = ILI_OF(ILM_OPND(ilmp, 2));
      ilix = ad2ili(IL_SCMPLXPOWI, op1, op2);
      ILM_RESULT(curilm) = ilix;
      return;
    }
    exp_qjsr("__mth_i_cpowi", DT_CMPLX, ilmp, curilm);
    return;
  case IM_CDTOI:
    if (XBIT(70, 0x40000000) && XBIT_NEW_MATH_NAMES_CMPLX) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      op2 = ILI_OF(ILM_OPND(ilmp, 2));
      ilix = ad2ili(IL_DCMPLXPOWI, op1, op2);
      ILM_RESULT(curilm) = ilix;
      return;
    }
    exp_qjsr("__mth_i_cdpowi", DT_DCMPLX, ilmp, curilm);
    return;
  case IM_CTOC:
    if (XBIT(70, 0x40000000) && XBIT_NEW_MATH_NAMES_CMPLX) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      op2 = ILI_OF(ILM_OPND(ilmp, 2));
      ilix = ad2ili(IL_SCMPLXPOW, op1, op2);
      ILM_RESULT(curilm) = ilix;
      return;
    }
    exp_qjsr("__mth_i_cpowc", DT_CMPLX, ilmp, curilm);
    return;
  case IM_CDTOCD:
    if (XBIT(70, 0x40000000) && XBIT_NEW_MATH_NAMES_CMPLX) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      op2 = ILI_OF(ILM_OPND(ilmp, 2));
      ilix = ad2ili(IL_DCMPLXPOW, op1, op2);
      ILM_RESULT(curilm) = ilix;
      return;
    }
    exp_qjsr("__mth_i_cdpowcd", DT_DCMPLX, ilmp, curilm);
    return;
  case IM_CSQRT:
    exp_qjsr("__mth_i_csqrt", DT_CMPLX, ilmp, curilm);
    return;
  case IM_CDSQRT:
    exp_qjsr("__mth_i_cdsqrt", DT_DCMPLX, ilmp, curilm);
    return;
  case IM_CEXP:
    /*
     *  exp(cmplx(0.0, a)) ->  cmplx(cos(a), sin(a))
     */
    ilixr = ILM_RESULT(ILM_OPND(ilmp, 1)); /* real part */
    if (ILI_OPC(ilixr) == IL_FCON && is_flt0(ILI_OPND(ilixr, 1))) {
      ilixi = ILM_IRESULT(ILM_OPND(ilmp, 1)); /* imag part */
      ilixr = ad1ili(IL_FCOS, ilixi);
      ilixi = ad1ili(IL_FSIN, ilixi);
      ILM_RRESULT(curilm) = ilixr;
      ILM_IRESULT(curilm) = ilixi;
      ILM_RESTYPE(curilm) = ILM_ISCMPLX;
      return;
    }
    else if (XBIT(70, 0x40000000)) {
      if (ILI_OPC(ilixr) == IL_SCMPLXCON) {
        tmp = ILI_OPND(ilixr, 1);
        if (is_creal_flt0(tmp)) {
          val[0] = 0;
          val[1] = CONVAL2G(tmp);
          ilixi = ad1ili(IL_FCON, getcon(val, DT_FLOAT));
          ilixr = ad1ili(IL_FCOS, ilixi);
          ilixi = ad1ili(IL_FSIN, ilixi);
          ilix = ad2ili(IL_SPSP2SCMPLX, ilixr, ilixi);
          ILM_RESULT(curilm) = ilix;
          return;
        }
      } else if (ILI_OPC(ilixr) == IL_SPSP2SCMPLX) {
        ilixi = ILI_OPND(ilixr, 2);
        ilixr = ILI_OPND(ilixr, 1);
        if (ILI_OPC(ilixr) == IL_FCON && is_flt0(ILI_OPND(ilixr, 1))) {
          ilixr = ad1ili(IL_FCOS, ilixi);
          ilixi = ad1ili(IL_FSIN, ilixi);
          ilix = ad2ili(IL_SPSP2SCMPLX, ilixr, ilixi);
          ILM_RESULT(curilm) = ilix;
          return;
        }
      }
    }
    exp_qjsr("__mth_i_cexp", DT_CMPLX, ilmp, curilm);
    return;
  case IM_CDEXP:
    /*
     *  exp(cmplx(0.0, a)) ->  cmplx(cos(a), sin(a))
     */
    ilixr = ILM_RESULT(ILM_OPND(ilmp, 1)); /* real part */
    if (ILI_OPC(ilixr) == IL_DCON && is_dbl0(ILI_OPND(ilixr, 1))) {
      ilixi = ILM_IRESULT(ILM_OPND(ilmp, 1)); /* imag part */
      ilixr = ad1ili(IL_DCOS, ilixi);
      ilixi = ad1ili(IL_DSIN, ilixi);
      ILM_RRESULT(curilm) = ilixr;
      ILM_IRESULT(curilm) = ilixi;
      ILM_RESTYPE(curilm) = ILM_ISDCMPLX;
      return;
    }
    else if (XBIT(70, 0x40000000)) {
      if (ILI_OPC(ilixr) == IL_DCMPLXCON) {
        tmp = ILI_OPND(ilixr, 1);
        if (is_dbl0(CONVAL1G(tmp))) {
          ilixi = ad1ili(IL_DCON, CONVAL2G(tmp));
          ilixr = ad1ili(IL_DCOS, ilixi);
          ilixi = ad1ili(IL_DSIN, ilixi);
          ilix = ad2ili(IL_DPDP2DCMPLX, ilixr, ilixi);
          ILM_RESULT(curilm) = ilix;
          return;
        }
      } else if (ILI_OPC(ilixr) == IL_DPDP2DCMPLX) {
        ilixi = ILI_OPND(ilixr, 2);
        ilixr = ILI_OPND(ilixr, 1);
        if (ILI_OPC(ilixr) == IL_DCON && is_dbl0(ILI_OPND(ilixr, 1))) {
          ilixr = ad1ili(IL_DCOS, ilixi);
          ilixi = ad1ili(IL_DSIN, ilixi);
          ilix = ad2ili(IL_DPDP2DCMPLX, ilixr, ilixi);
          ILM_RESULT(curilm) = ilix;
          return;
        }
      }
    }
#if defined(TARGET_X8664)
    exp_qjsr(relaxed_math("exp", 's', 'z', "__mth_i_cdexp"), DT_DCMPLX, ilmp,
             curilm);
#else
    exp_qjsr("__mth_i_cdexp", DT_DCMPLX, ilmp, curilm);
#endif
    return;
  case IM_CLOG:
    exp_qjsr("__mth_i_clog", DT_CMPLX, ilmp, curilm);
    return;
  case IM_CDLOG:
    exp_qjsr("__mth_i_cdlog", DT_DCMPLX, ilmp, curilm);
    return;
  case IM_CSIN:
    exp_qjsr("__mth_i_csin", DT_CMPLX, ilmp, curilm);
    return;
  case IM_CDSIN:
    exp_qjsr("__mth_i_cdsin", DT_DCMPLX, ilmp, curilm);
    return;
  case IM_CCOS:
    exp_qjsr("__mth_i_ccos", DT_CMPLX, ilmp, curilm);
    return;
  case IM_CDCOS:
    exp_qjsr("__mth_i_cdcos", DT_DCMPLX, ilmp, curilm);
    return;
  case IM_CASIN:
    exp_qjsr("__mth_i_casin", DT_CMPLX, ilmp, curilm);
    return;
  case IM_CDASIN:
    exp_qjsr("__mth_i_cdasin", DT_DCMPLX, ilmp, curilm);
    return;
  case IM_CACOS:
    exp_qjsr("__mth_i_cacos", DT_CMPLX, ilmp, curilm);
    return;
  case IM_CDACOS:
    exp_qjsr("__mth_i_cdacos", DT_DCMPLX, ilmp, curilm);
    return;
  case IM_CATAN:
    exp_qjsr("__mth_i_catan", DT_CMPLX, ilmp, curilm);
    return;
  case IM_CDATAN:
    exp_qjsr("__mth_i_cdatan", DT_DCMPLX, ilmp, curilm);
    return;
  case IM_CCOSH:
    exp_qjsr("__mth_i_ccosh", DT_CMPLX, ilmp, curilm);
    return;
  case IM_CDCOSH:
    exp_qjsr("__mth_i_cdcosh", DT_DCMPLX, ilmp, curilm);
    return;
  case IM_CSINH:
    exp_qjsr("__mth_i_csinh", DT_CMPLX, ilmp, curilm);
    return;
  case IM_CDSINH:
    exp_qjsr("__mth_i_cdsinh", DT_DCMPLX, ilmp, curilm);
    return;
  case IM_CTANH:
    exp_qjsr("__mth_i_ctanh", DT_CMPLX, ilmp, curilm);
    return;
  case IM_CDTANH:
    exp_qjsr("__mth_i_cdtanh", DT_DCMPLX, ilmp, curilm);
    return;
  case IM_CTAN:
    exp_qjsr("__mth_i_ctan", DT_CMPLX, ilmp, curilm);
    return;
  case IM_CDTAN:
    exp_qjsr("__mth_i_cdtan", DT_DCMPLX, ilmp, curilm);
    return;
  case IM_CDIV:
    {
      if (XBIT(70, 0x40000000)) {
        exp_qjsr("__mth_i_cdiv", DT_CMPLX, ilmp, curilm);
        return;
      } else {
        tmp = ILM_OPND(ilmp, 2);
        ilix = ILM_IRESULT(tmp);
        if (!flg.ieee && !XBIT(70, 0x40000000) && ILI_OPC(ilix) == IL_FCON &&
            is_flt0(ILI_OPND(ilix, 1)) && ILM_RRESULT(tmp) != ilix) {
          ILM_OPC(ilmp) = IM_CDIVR;
          ILM_RESULT(tmp) = ILM_RRESULT(tmp);
          ILM_RESTYPE(tmp) = 0; /* real result */
          tmp = exp_mac((int)ILM_OPC(ilmp), ilmp, curilm);
          return;
        }
        exp_qjsr("__mth_i_cdiv", DT_CMPLX, ilmp, curilm);
      }
    }
    return;
  case IM_CDDIV:
    {
      if (XBIT(70, 0x40000000)) {
        exp_qjsr("__mth_i_cddiv", DT_DCMPLX, ilmp, curilm);
        return;
      } else {
        tmp = ILM_OPND(ilmp, 2);
        ilix = ILM_IRESULT(tmp);
        if (!flg.ieee && !XBIT(70, 0x40000000) && ILI_OPC(ilix) == IL_DCON &&
            is_dbl0(ILI_OPND(ilix, 1)) && ILM_RRESULT(tmp) != ilix) {
          ILM_OPC(ilmp) = IM_CDDIVD;
          ILM_RESULT(tmp) = ILM_RRESULT(tmp);
          ILM_RESTYPE(tmp) = 0; /* double result */
          tmp = exp_mac((int)ILM_OPC(ilmp), ilmp, curilm);
          return;
        }
        exp_qjsr("__mth_i_cddiv", DT_DCMPLX, ilmp, curilm);
      }
    }
    return;
  case IM_CADD:
    if (XBIT(70, 0x40000000)) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      op2 = ILI_OF(ILM_OPND(ilmp, 2));
      ilix = ad2ili(IL_SCMPLXADD, op1, op2);
      ILM_RESULT(curilm) = ilix;
    } else {
      tmp = exp_mac(IM_CADD, ilmp, curilm);
    }
    return;
  case IM_CDADD:
    if (XBIT(70, 0x40000000)) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      op2 = ILI_OF(ILM_OPND(ilmp, 2));
      ilix = ad2ili(IL_DCMPLXADD, op1, op2);
      ILM_RESULT(curilm) = ilix;
    } else {
      tmp = exp_mac(IM_CDADD, ilmp, curilm);
    }
    return;
  case IM_CSUB:
    if (XBIT(70, 0x40000000)) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      op2 = ILI_OF(ILM_OPND(ilmp, 2));
      ilix = ad2ili(IL_SCMPLXSUB, op1, op2);
      ILM_RESULT(curilm) = ilix;
    } else {
      tmp = exp_mac(IM_CSUB, ilmp, curilm);
    }
    return;
  case IM_CDSUB:
    if (XBIT(70, 0x40000000)) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      op2 = ILI_OF(ILM_OPND(ilmp, 2));
      ilix = ad2ili(IL_DCMPLXSUB, op1, op2);
      ILM_RESULT(curilm) = ilix;
    } else {
      tmp = exp_mac(IM_CDSUB, ilmp, curilm);
    }
    return;
  case IM_CMUL:
    if (XBIT(70, 0x40000000)) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      op2 = ILI_OF(ILM_OPND(ilmp, 2));
      ilix = ad2ili(IL_SCMPLXMUL, op1, op2);
      ILM_RESULT(curilm) = ilix;
    } else {
      tmp = exp_mac(IM_CMUL, ilmp, curilm);
    }
    return;
  case IM_CDMUL:
    if (XBIT(70, 0x40000000)) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      op2 = ILI_OF(ILM_OPND(ilmp, 2));
      ilix = ad2ili(IL_DCMPLXMUL, op1, op2);
      ILM_RESULT(curilm) = ilix;
    } else {
      tmp = exp_mac(IM_CDMUL, ilmp, curilm);
    }
    return;
  case IM_CNEG:
    if (XBIT(70, 0x40000000)) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      op2 = ILI_OF(ILM_OPND(ilmp, 2));
      ilix = ad1ili(IL_SCMPLXNEG, op1);
      ILM_RESULT(curilm) = ilix;
    } else {
      tmp = exp_mac(opc, ilmp, curilm);
    }
    return;
  case IM_CDNEG:
    if (XBIT(70, 0x40000000)) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      op2 = ILI_OF(ILM_OPND(ilmp, 2));
      ilix = ad1ili(IL_DCMPLXNEG, op1);
      ILM_RESULT(curilm) = ilix;
    } else {
      tmp = exp_mac(opc, ilmp, curilm);
    }
    return;
  case IM_CONJG:
    if (XBIT(70, 0x40000000)) {
      /* convert to xorps signbit*/
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      ilix = ad1ili(IL_SCMPLXCONJG, op1);
      ILM_RESULT(curilm) = ilix;
    } else {
      tmp = exp_mac(opc, ilmp, curilm);
    }
    return;
  case IM_DCONJG:
    if (XBIT(70, 0x40000000)) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      ilix = ad1ili(IL_DCMPLXCONJG, op1);
      ILM_RESULT(curilm) = ilix;
    } else {
      tmp = exp_mac(opc, ilmp, curilm);
    }
    return;

  /* special handling of 64 bit precision integer ilms */
  /* -- type -- arithmetic */

  case IM_KNEG:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    ILM_RESULT(curilm) = ad1ili(IL_KNEG, op1);
    return;
  case IM_KABS:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    tmp = ad1ili(IL_NULL, 0);
    tmp = ad2ili(IL_ARGKR, op1, tmp);
    op3 = mk_prototype("ftn_i_kabs", "pure", DT_INT8, 1, DT_INT8);
    tmp = ad2ili(IL_QJSR, op3, tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRKR, tmp, KR_RETVAL);
    return;
  case IM_KFIX:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    tmp = ad1ili(IL_NULL, 0);
    tmp = ad2ili(IL_ARGSP, op1, tmp);
    op3 = mk_prototype(MTH_I_FIXK, "pure", DT_INT8, 1, DT_REAL);
    tmp = ad2ili(IL_QJSR, op3, tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRKR, tmp, KR_RETVAL);
    return;
  case IM_KDFIX:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    ILM_RESULT(curilm) = ad1ili(IL_DFIXK, op1);
    return;
  case IM_ITOI8:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    ILM_RESULT(curilm) = ad1ili(IL_IKMV, op1);
    return;
  case IM_I8TOI:
    if (XBIT(124, 0x400)) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      ILM_RESULT(curilm) = ad1ili(IL_KIMV, op1);
      return;
    }
    tmp = exp_mac(IM_I8TOI, ilmp, curilm);
    return;
  case IM_KMAX:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    ILM_RESULT(curilm) = ad2ili(IL_KMAX, op1, op2);
    return;
  case IM_KMIN:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    ILM_RESULT(curilm) = ad2ili(IL_KMIN, op1, op2);
    return;
  case IM_KMOD:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    ILM_RESULT(curilm) = ad2ili(IL_KMOD, op1, op2);
    return;
#ifdef IM_KMERGE
  case IM_KMERGE:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    op3 = ILI_OF(ILM_OPND(ilmp, 3));
    tmp = ad1ili(IL_NULL, 0);
    tmp = ad2ili(IL_ARGIR, op3, tmp);
    tmp = ad2ili(IL_ARGKR, op2, tmp);
    tmp = ad2ili(IL_ARGKR, op1, tmp);
    tmp = ad2ili(IL_QJSR, mkfunc("ftn_i_kmerge"), tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRKR, tmp, KR_RETVAL);
    return;
#endif
  case IM_KSIGN:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    ILM_RESULT(curilm) = ad2func_kint(IL_QJSR, "ftn_i_kisign", op1, op2);
    return;
  case IM_KAND:
  case IM_LAND8:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    ILM_RESULT(curilm) = ad2ili(IL_KAND, op1, op2);
    return;
  case IM_KOR:
  case IM_LOR8:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    ILM_RESULT(curilm) = ad2ili(IL_KOR, op1, op2);
    return;
  case IM_KXOR:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    ILM_RESULT(curilm) = ad2ili(IL_KXOR, op1, op2);
    return;
  case IM_KNOT:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    ILM_RESULT(curilm) = ad1ili(IL_KNOT, op1);
    return;
  case IM_KBITS:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = kimove(ILI_OF(ILM_OPND(ilmp, 2)));
    op3 = kimove(ILI_OF(ILM_OPND(ilmp, 3)));
    tmp = ad1ili(IL_NULL, 0);
    tmp = ad2ili(IL_ARGIR, op3, tmp);
    tmp = ad2ili(IL_ARGIR, op2, tmp);
    tmp = ad2ili(IL_ARGKR, op1, tmp);
    op3 = mk_prototype(MTH_I_KBITS, "pure", DT_INT8, 3, DT_INT8, DT_INT8,
                       DT_INT8);
    tmp = ad2ili(IL_QJSR, op3, tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRKR, tmp, KR_RETVAL);
    return;
  case IM_KBSET:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = kimove(ILI_OF(ILM_OPND(ilmp, 2)));
    tmp = ad1ili(IL_NULL, 0);
    tmp = ad2ili(IL_ARGIR, op2, tmp);
    tmp = ad2ili(IL_ARGKR, op1, tmp);
    op3 = mk_prototype(MTH_I_KBSET, "pure", DT_INT8, 2, DT_INT8, DT_INT8);
    tmp = ad2ili(IL_QJSR, op3, tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRKR, tmp, KR_RETVAL);
    return;
  case IM_KBTEST:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = kimove(ILI_OF(ILM_OPND(ilmp, 2)));
    tmp = ad1ili(IL_NULL, 0);
    tmp = ad2ili(IL_ARGIR, op2, tmp);
    tmp = ad2ili(IL_ARGKR, op1, tmp);
    op3 = mk_prototype(MTH_I_KBTEST, "pure", DT_INT8, 2, DT_INT8, DT_INT8);
    tmp = ad2ili(IL_QJSR, op3, tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRKR, tmp, KR_RETVAL);
    return;
  case IM_KBCLR:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = kimove(ILI_OF(ILM_OPND(ilmp, 2)));
    tmp = ad1ili(IL_NULL, 0);
    tmp = ad2ili(IL_ARGIR, op2, tmp);
    tmp = ad2ili(IL_ARGKR, op1, tmp);
    op3 = mk_prototype(MTH_I_KBCLR, "pure", DT_INT8, 2, DT_INT8, DT_INT8);
    tmp = ad2ili(IL_QJSR, op3, tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRKR, tmp, KR_RETVAL);
    return;
  case IM_KURSHIFT:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = kimove(ILI_OF(ILM_OPND(ilmp, 2)));
    op2 = ad1ili(IL_INEG, op2);
    tmp = ad1ili(IL_NULL, 0);
    tmp = ad2ili(IL_ARGIR, op2, tmp);
    tmp = ad2ili(IL_ARGKR, op1, tmp);
    op3 = mkfunc("ftn_i_kishft");
    tmp = ad2ili(IL_QJSR, op3, tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRKR, tmp, KR_RETVAL);
    return;
  case IM_KULSHIFT:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = kimove(ILI_OF(ILM_OPND(ilmp, 2)));
    tmp = ad1ili(IL_NULL, 0);
    tmp = ad2ili(IL_ARGIR, op2, tmp);
    tmp = ad2ili(IL_ARGKR, op1, tmp);
    op3 = mkfunc("ftn_i_kishft");
    tmp = ad2ili(IL_QJSR, op3, tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRKR, tmp, KR_RETVAL);
    return;
  case IM_KEQV:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    ILM_RESULT(curilm) =
        ad2func_kint(IL_QJSR, "ftn_i_xnori64", forceK(op1), forceK(op2));
    return;
  case IM_LEQV8:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    tmp = ad3ili(IL_KCMP, op1, op2, CC_EQ);
    ILM_RESULT(curilm) = ad1ili(IL_IKMV, tmp);
    return;
  case IM_LNEQV8:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    ILM_RESULT(curilm) =
        ad2func_kint(IL_QJSR, "ftn_i_xori64", forceK(op1), forceK(op2));
    return;
  case IM_FLOATK:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    tmp = ad1ili(IL_NULL, 0);
    op2 = mk_prototype(MTH_I_FLOATK, "pure", DT_REAL, 1, DT_INT8);
    tmp = ad2ili(IL_ARGKR, op1, tmp);
    tmp = ad2ili(IL_QJSR, op2, tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRSP, tmp, SP(0));
    return;
  case IM_DFLOATK:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    tmp = ad1ili(IL_NULL, 0);
    op2 = mk_prototype(MTH_I_DFLOATK, "pure", DT_DBLE, 1, DT_INT8);
    tmp = ad2ili(IL_ARGKR, op1, tmp);
    tmp = ad2ili(IL_QJSR, op2, tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRDP, tmp, DP(0));
    return;
  case IM_D2K:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    ILM_RESULT(curilm) = ad1ili(IL_DP2KR, op1);
    return;
  case IM_R2K:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    ILM_RESULT(curilm) = ad1ili(IL_SP2KR, op1);
    return;
  case IM_I2K:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    ILM_RESULT(curilm) = ad1ili(IL_IKMV, op1);
    return;
  case IM_UI2K:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    ILM_RESULT(curilm) = ad1ili(IL_UIKMV, op1);
    return;
  /* -- type -- intrinsic */
  case IM_KTOI:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = kimove(ILI_OF(ILM_OPND(ilmp, 2)));
    tmp = ad1ili(IL_NULL, 0);
    tmp = ad2ili(IL_ARGIR, op2, tmp);
    tmp = ad2ili(IL_ARGKR, op1, tmp);
    tmp = ad2ili(IL_QJSR, mkfunc("__mth_i_kpowi"), tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRKR, tmp, KR_RETVAL);
    return;
  case IM_KTOK:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    tmp = ad1ili(IL_NULL, 0);
    tmp = ad2ili(IL_ARGKR, op2, tmp);
    tmp = ad2ili(IL_ARGKR, op1, tmp);
    tmp = ad2ili(IL_QJSR, mkfunc("__mth_i_kpowk"), tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRKR, tmp, KR_RETVAL);
    return;
  case IM_RTOK:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    tmp = ad1ili(IL_NULL, 0);
    tmp = ad2ili(IL_ARGKR, op2, tmp);
    tmp = ad2ili(IL_ARGSP, op1, tmp);
    tmp = ad2ili(IL_QJSR, mkfunc("__mth_i_rpowk"), tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRSP, tmp, SP(0));
    return;
  case IM_DTOK:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    tmp = ad1ili(IL_NULL, 0);
    tmp = ad2ili(IL_ARGKR, op2, tmp);
    tmp = ad2ili(IL_ARGDP, op1, tmp);
    tmp = ad2ili(IL_QJSR, mkfunc("__mth_i_dpowk"), tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRDP, tmp, DP(0));
    return;
  case IM_CTOK:
    if (XBIT(70, 0x40000000) && XBIT_NEW_MATH_NAMES_CMPLX) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      op2 = ILI_OF(ILM_OPND(ilmp, 2));
      ilix = ad2ili(IL_SCMPLXPOWK, op1, op2);
      ILM_RESULT(curilm) = ilix;
      return;
    }
    exp_qjsr("__mth_i_cpowk", DT_CMPLX, ilmp, curilm);
    return;
  case IM_CDTOK:
    if (XBIT(70, 0x40000000) && XBIT_NEW_MATH_NAMES_CMPLX) {
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      op2 = ILI_OF(ILM_OPND(ilmp, 2));
      ilix = ad2ili(IL_DCMPLXPOWK, op1, op2);
      ILM_RESULT(curilm) = ilix;
      return;
    }
    exp_qjsr("__mth_i_cdpowk", DT_DCMPLX, ilmp, curilm);
    return;
  case IM_KDIM:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    ILM_RESULT(curilm) = ad2func_kint(IL_QJSR, "ftn_i_kidim", op1, op2);
    return;
  case IM_KNINT:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    tmp = ad1ili(IL_NULL, 0);
    tmp = ad3ili(IL_DASP, op1, SP(0), tmp);
    (void)mk_prototype("__mth_i_knint", "pure", DT_INT8, 1, DT_FLOAT);
    tmp = ad2ili(IL_QJSR, mkfunc("__mth_i_knint"), tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRKR, tmp, KR_RETVAL);
    return;
  case IM_KDNINT:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    tmp = ad1ili(IL_NULL, 0);
    tmp = ad3ili(IL_DADP, op1, DP(0), tmp);
    (void)mk_prototype("__mth_i_kidnnt", "pure", DT_INT8, 1, DT_DBLE);
    tmp = ad2ili(IL_QJSR, mkfunc("__mth_i_kidnnt"), tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRKR, tmp, KR_RETVAL);
    return;
  case IM_KISHFT:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = kimove(ILI_OF(ILM_OPND(ilmp, 2)));
    tmp = ad1ili(IL_NULL, 0);
    tmp = ad2ili(IL_ARGIR, op2, tmp);
    tmp = ad2ili(IL_ARGKR, op1, tmp);
    op3 = mkfunc("ftn_i_kishft");
    tmp = ad2ili(IL_QJSR, op3, tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRKR, tmp, KR_RETVAL);
    return;
  case IM_KSHFTC:
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    op2 = kimove(ILI_OF(ILM_OPND(ilmp, 2)));
    op3 = kimove(ILI_OF(ILM_OPND(ilmp, 3)));
    tmp = ad1ili(IL_NULL, 0);
    tmp = ad2ili(IL_ARGIR, op3, tmp);
    tmp = ad2ili(IL_ARGIR, op2, tmp);
    tmp = ad2ili(IL_ARGKR, op1, tmp);
    tmp = ad2ili(IL_QJSR, mkfunc("ftn_i_kishftc"), tmp);
    ILM_RESULT(curilm) = ad2ili(IL_DFRKR, tmp, KR_RETVAL);
    return;
  /* -- type -- constant */
  case IM_KCON:
    tmp = ILM_OPND(ilmp, 1);
    if (XBIT(124, 0x400)) {
      ILM_RESULT(curilm) = ad1ili(IL_KCON, tmp);
      rcandb.kr = 1;
    } else {
      val[0] = 0;
      val[1] = CONVAL2G(tmp);
      ILM_RESULT(curilm) = ad1ili(IL_ICON, getcon(val, DT_INT));
    }
    break;

  case IM_CCON:
    if (XBIT(70, 0x40000000)) {
      tmp = ILM_OPND(ilmp, 1);
      ILM_RESULT(curilm) = ad1ili(IL_SCMPLXCON, tmp);
    } else {
      /* complex constant; create 2 rcons */
      val[0] = 0;
      val[1] = CONVAL1G(tmp = ILM_OPND(ilmp, 1));
      ILM_RRESULT(curilm) = ad1ili(IL_FCON, getcon(val, DT_REAL));
      val[1] = CONVAL2G(tmp);
      ILM_IRESULT(curilm) = ad1ili(IL_FCON, getcon(val, DT_REAL));
      ILM_RESTYPE(curilm) = ILM_ISCMPLX;
    }
    break;
  case IM_CDCON:
    if (XBIT(70, 0x40000000)) {
      tmp = ILM_OPND(ilmp, 1);
      ILM_RESULT(curilm) = ad1ili(IL_DCMPLXCON, tmp);
    } else {
      /* complex double constant; create 2 dcons */
      tmp = ILM_OPND(ilmp, 1);
      val[0] = CONVAL1G(CONVAL1G(tmp));
      val[1] = CONVAL2G(CONVAL1G(tmp));
      ILM_RRESULT(curilm) = ad1ili(IL_DCON, getcon(val, DT_DBLE));
      val[0] = CONVAL1G(CONVAL2G(tmp));
      val[1] = CONVAL2G(CONVAL2G(tmp));
      ILM_IRESULT(curilm) = ad1ili(IL_DCON, getcon(val, DT_DBLE));
      ILM_RESTYPE(curilm) = ILM_ISDCMPLX;
    }
    break;

  case IM_LOC:
    /* merely copy up results, move from AR to DR */
    tmp = ILM_OPND(ilmp, 1);
    ilmpx = (ILM *)(ilmb.ilm_base + tmp);
    nme = NME_OF(tmp);
    ilix = ILI_OF(tmp);

    ILM_RESULT(curilm) = ad1ili(IL_AKMV, ilix);
    loc_of(nme);
    {
      int sptr = basesym_of(nme);
      LOCARGP(sptr, 1);
    }
    break;
  case IM_ACON:
    nmsym = ILM_OPND(ilmp, 1);
    if (STYPEG(nmsym) == ST_LABEL) {
      if ((tmp = FMTPTG(nmsym)) != 0) {
        /* format statement label */
        nmsym = get_acon(tmp, 0);
        ILM_RESULT(curilm) = ad1ili(IL_ACON, nmsym);
        nme = NME_UNK;
      } else {
        /*
         * executable statement label; add nmsym to list of
         * executable statement labels appearing in assignment
         * statements.
         */
        if (SYMLKG(nmsym) == 0) {
          SYMLKP(nmsym, gbl.asgnlbls);
          gbl.asgnlbls = nmsym;
        }
        nmsym = get_acon(nmsym, 0);
        ILM_RESULT(curilm) = ad2ili(IL_ACEXT, nmsym, 0);
        nme = NME_UNK;
      }
      break;
    }
    /* not a label */
    ILM_RESULT(curilm) = ad1ili(IL_ACON, nmsym);
    nme = NME_UNK;
    break;
/*
 * For the compare ILMs, no code is generated at this time. Pass up
 * the equivalent ILI opcode to the relational or conditional branch
 * ILM using the compare.
 */
  case IM_KCMP:
    if (XBIT(124, 0x400))
      ILM_NME(curilm) = IL_KCMP;
    else
      ILM_NME(curilm) = IL_ICMP;
    return;
  case IM_ICMP:
    ILM_NME(curilm) = IL_ICMP;
    return;
  case IM_RCMP:
    ILM_NME(curilm) = IL_FCMP;
    return;
  case IM_DCMP:
    ILM_NME(curilm) = IL_DCMP;
    return;
  case IM_UICMP:
    ILM_NME(curilm) = IL_UICMP;
    return;
  case IM_UDICMP:
    interr("exp_ac: no IL_UDICMP ??", curilm, 3);
    ILM_NME(curilm) = IL_ICMP;
    return;
  case IM_PCMP:
    ILM_NME(curilm) = IL_ACMP;
    op2 = ILI_OF(ILM_OPND(ilmp, 2));
    if (IL_RES(ILI_OPC(op2)) == ILIA_IR) {
      op2 = ad1ili(IL_IAMV, op2);
      ILM_RESULT(ILM_OPND(ilmp, 2)) = op2;
    }
    else if (IL_RES(ILI_OPC(op2)) == ILIA_KR) {
      op2 = ad1ili(IL_KAMV, op2);
      ILM_RESULT(ILM_OPND(ilmp, 2)) = op2;
    }
    op1 = ILI_OF(ILM_OPND(ilmp, 1));
    if (IL_RES(ILI_OPC(op1)) != ILIA_AR) {
      /*
       * Inlining can create a situation where an actual argument
       * is now used in a pointer comparison, e.g.,
       *   call foo(1)
       *   ...
       * foo(i):
       *   if (present(i)) ...
       *
       * Recover if the 2nd operand is 'null' by creating a
       * suitable non-null value for the 1st operand.
       */
      if (ILI_OPC(op2) == IL_ACON) {
        int s;
        s = ILI_OPND(op2, 1);
        if (CONVAL1G(s) == 0 && CONVAL2G(s) == 0) {
          op1 = ad_aconi(17);
          ILM_RESULT(ILM_OPND(ilmp, 1)) = op1;
        }
      }
    }
    return;
/*
 * Mark complex compares so that the relational will generate
 * the compares of the real and imaginary parts.  The relational
 * will need to know which ILI to use and the fact that it's
 * complex.  NOTE that even for a complex double compare, the
 * type passed up is single complex; this is done so that the
 * relational can combine the handling of both types.
 */
  case IM_CCMP:
    if (XBIT(70, 0x40000000)) {
      ILM_NME(curilm) = IL_FCMP;
      return;
    }
    ILM_RESTYPE(curilm) = ILM_ISCMPLX;
    ILM_NME(curilm) = IL_FCMP;
    return;
  case IM_CDCMP:
    if (XBIT(70, 0x40000000)) {
      ILM_NME(curilm) = IL_DCMP;
      return;
    }
    ILM_RESTYPE(curilm) = ILM_ISCMPLX;
    ILM_NME(curilm) = IL_DCMP;
    return;

  /*
   * For a relational, pick up the ILI opcode to be used from the names
   * entry of its operand (a compare).  Also, the operands of this ILI
   * are the ILIs created for the operands of the compare ILM and an
   * immediate value denoting the relation.
   */
  case IM_EQ8:
  case IM_NE8:
  case IM_LT8:
  case IM_GE8:
  case IM_LE8:
  case IM_GT8:
    tmp = opc - IM_EQ8 + 1;
    goto relational_shared;
  case IM_EQ:
  case IM_NE:
  case IM_LT:
  case IM_GE:
  case IM_LE:
  case IM_GT:
    tmp = opc - IM_EQ + 1;
  relational_shared:
    ilmx = ILM_OPND(ilmp, 1); /* locate compare ILM */
    ilmpx = (ILM *)(ilmb.ilm_base + ilmx);
#if DEBUG
    assert(ILM_OPC(ilmpx) >= IM_ICMP && ILM_OPC(ilmpx) <= IM_NSCMP ||
               ILM_OPC(ilmpx) == IM_KCMP || ILM_OPC(ilmpx) == IM_PCMP,
           "expand:compare not operand of rel.", curilm, 3);
#endif
    if (ILM_RESTYPE(ilmx) == ILM_ISCHAR) {
/* a string compare may be handled by the external function,
 * ftn_strcmp.  The value of the function is -1 if '<',
 * 0 if '=', * and 1 if '>'; its value is compared with
 * integer 0.  If the ili of the SCMP ilm is ICMP, then
 * the compare was optimized.
 */
#if DEBUG
      assert(ILM_OPC(ilmpx) == IM_SCMP || ILM_OPC(ilmpx) == IM_NSCMP,
             "expand:nme of compare zero, SCMP expected", curilm, 3);
#endif
      ilix = ILI_OF(ilmx);
      if (ILI_OPC(ilix) == IL_ICMP)
        ILM_RESULT(curilm) = ad3ili(IL_ICMP, (int)ILI_OPND(ilix, 1),
                                    (int)ILI_OPND(ilix, 2), tmp);
      else
        ILM_RESULT(curilm) = ad3ili(IL_ICMP, ilix, ad_icon((INT)0), tmp);
      return;
    }
    if (ILM_RESTYPE(ilmx) == ILM_ISCMPLX) {
      int ilm1; /* ILM index of first operand of compare */
      int ilm2; /* ILM index of second operand */
      int il1, il2;
      ilm1 = ILM_OPND(ilmpx, 1);
      ilm2 = ILM_OPND(ilmpx, 2);
      opcx = NME_OF(ilmx);
      il1 = ad3ili(opcx, (int)ILM_RRESULT(ilm1), (int)ILM_RRESULT(ilm2), tmp);
      il2 = ad3ili(opcx, (int)ILM_IRESULT(ilm1), (int)ILM_IRESULT(ilm2), tmp);
      ILM_RESULT(curilm) = (opc == IM_EQ || opc == IM_EQ8)
                               ? ad2ili(IL_AND, il1, il2)
                               : ad2ili(IL_OR, il1, il2);
      if (XBIT(124, 0x400) && opc >= IM_EQ8 && opc <= IM_GT8)
        ILM_RESULT(curilm) = ad1ili(IL_IKMV, ILM_RESULT(curilm));
      return;
    } else if ((ILM_OPC(ilmpx) == IM_CCMP || ILM_OPC(ilmpx) == IM_CDCMP) &&
               XBIT(70, 0x40000000)) {
      int ilm1; /* ILM index of first operand of compare */
      int ilm2; /* ILM index of second operand */
      int il1, il2, opci, opcr;
      ilm1 = ILM_OPND(ilmpx, 1);
      ilm2 = ILM_OPND(ilmpx, 2);
      opcx = NME_OF(ilmx);
      if (ILM_OPC(ilmpx) == IM_CCMP) {
        opcr = IL_SCMPLX2REAL;
        opci = IL_SCMPLX2IMAG;
      } else {
        opcr = IL_DCMPLX2REAL;
        opci = IL_DCMPLX2IMAG;
      }
      il1 = ad3ili(opcx, (int)ad1ili(opcr, ILM_RESULT(ilm1)),
                   (int)ad1ili(opcr, ILM_RESULT(ilm2)), tmp);
      il2 = ad3ili(opcx, (int)ad1ili(opci, ILM_RESULT(ilm1)),
                   (int)ad1ili(opci, ILM_RESULT(ilm2)), tmp);
      ILM_RESULT(curilm) = (opc == IM_EQ || opc == IM_EQ8)
                               ? ad2ili(IL_AND, il1, il2)
                               : ad2ili(IL_OR, il1, il2);
      if (XBIT(124, 0x400) && opc >= IM_EQ8 && opc <= IM_GT8)
        ILM_RESULT(curilm) = ad1ili(IL_IKMV, ILM_RESULT(curilm));
      return;
    }
    opcx = NME_OF(ilmx);
    /*
     * If the compare is an unsigned compare for equality or
     * non-equality, use the signed integer compare.
     */
    if (opcx == IL_UICMP && tmp <= 2)
      opcx = IL_ICMP;
    op1 = ILI_OF(ILM_OPND(ilmpx, 1));
    op2 = ILI_OF(ILM_OPND(ilmpx, 2));
    if (opcx == IL_ICMP) {
      op1 = kimove(op1);
      op2 = kimove(op2);
    }
    ILM_RESULT(curilm) = ad3ili(opcx, op1, op2, tmp);
    if (XBIT(124, 0x400) && opc >= IM_EQ8 && opc <= IM_GT8)
      ILM_RESULT(curilm) = ad1ili(IL_IKMV, ILM_RESULT(curilm));
    return;
#ifdef IM_ALLOCA
  case IM_ALLOCA:
    if (!bihb.parfg && !bihb.taskfg && ILM_OPND(ilmp, 4) != 1) {
      /*
      fprintf(stderr, "ALLOCA %s\n", SYMNAME(ILM_OPND(ilmp,3)));
      */
      ilix = exp_alloca(ilmp);
    } else {
      int arg;
      int sym;
/*
 * 64-bit:
 *    void *RTE_auto_allocv(I64 n, int sz)
 * 32-bit
 *    void *RTE_auto_allocv(int n, int sz)
 */
      sym = mkfunc(mkRteRtnNm(RTE_auto_allocv));
      DTYPEP(sym, DT_CPTR); /* else defaults to 'int' return type */
      op1 = ILI_OF(ILM_OPND(ilmp, 1));
      op2 = ILI_OF(ILM_OPND(ilmp, 2));
      arg = ad1ili(IL_NULL, 0);
      arg = ad2ili(IL_ARGIR, op2, arg);
      arg = ad2ili(IL_ARGIR, op1, arg);
      ilix = ad2ili(IL_JSR, sym, arg);
      ilix = ad2ili(IL_DFRAR, ilix, AR_RETVAL);
    }
    ILM_RESULT(curilm) = ilix;
    break;
#endif
  }
  ILM_NME(curilm) = nme; /* save NME entry  */
}

/***************************************************************/

static int
genload(int sym, LOGICAL bigobj)
{
  int acon;
  if (STYPEG(sym) == ST_CONST) {
    if (bigobj) {
      return ad1ili(IL_KCON, sym);
    } else {
      return ad1ili(IL_ICON, sym);
    }
  }
/* generate load of sym */
  if (flg.smp || XBIT(34, 0x200)) {
    if (SCG(sym) == SC_STATIC)
      sym_is_refd(sym);
  }
  acon = compute_address(sym);
  if (bigobj) {
    return ad3ili(IL_LDKR, acon, addnme(NT_VAR, sym, 0, (INT)0), MSZ_I8);
  } else {
    return ad3ili(IL_LD, acon, addnme(NT_VAR, sym, 0, (INT)0), MSZ_WORD);
  }
} /* genload */

/*
 * components of a subscripted reference, computed by compute_subscr() and
 * inlarr(), and possibly modified by inlarr().
 * NOTE that the ili expressions for the zero base offset and subscript
 * offset do not have the element size factored in.
 */
static struct {
  int base;     /* base ili (type ar) of the array */
  int basenm;   /* base nme of subscripted ref */
  int zbase;    /* final zero base offset (ili, type ir ) */
  int offset;   /* ili expr of subscripts with consts factored out */
  int scale;    /* scaling factor to be applied to any offsets */
  int elmscz;   /* ili of element size (to be scaled, type ir) */
  int elmsz;    /* ili of actual element size (type ir) */
  int eldt;     /* data type of element */
  int nsubs;    /* number of subscripts */
  int sub[7];   /* ili for each (actual) subscript */
  int finalnme; /* final NME */
} subscr;

extern void create_array_subscr(int nmex, int sym, int dtype, int nsubs,
                                int *subs, int ilix);
extern int create_array_ref(int nmex, int sptr, DTYPE dtype, int nsubs,
                            int *subs, int ilix, int sdscilix, int inline_flag,
                            int *pnme);

static void
compute_subscr(ILM *ilmp, LOGICAL bigobj)
{
  ADSC *adp;  /* array descriptor */
  int dtype;  /* array data type */
  int arrilm; /* ilm for array */
  int zbase;  /* zbase sym/ili ptr */
  int i;
  int sym;
  ILM *ilmp1;
  int sub;
  int mplyr;
  int offset;
  int ili2;
  ISZ_T coffset;
  int any_kr;
  int sub_1;

  subscr.nsubs = ILM_OPND(ilmp, 1);
#if DEBUG
  assert(subscr.nsubs <= (sizeof(subscr.sub) / sizeof(int)),
         "compute_subscr:nsubs exceeded", subscr.nsubs, 3);
#endif
  arrilm = ILM_OPND(ilmp, 2);
  dtype = ILM_OPND(ilmp, 3);
  {
    int subs[7];
    ilmp1 = (ILM *)(ilmb.ilm_base + arrilm);
    if (ILM_OPC(ilmp1) == IM_PLD) {
      /* rewritten arguments */
      sym = ILM_OPND(ilmp1, 2);
      if (ORIGDUMMYG(sym)) {
        sym = ORIGDUMMYG(sym);
      }
    } else {
      sym = ILM_OPND(ilmp1, 1); /* symbol pointer */
    }
    for (i = 0; i < subscr.nsubs; ++i) {
      subs[i] = ILI_OF(ILM_OPND(ilmp, 4 + i)); /* subscript ili */
    }
    create_array_subscr(NME_OF(arrilm), sym, dtype, ILM_OPND(ilmp, 1), subs,
                        ILI_OF(arrilm));
    return;
  }
}

static int
compute_nme(int sptr, int constant, int basenm)
{
  /* build up the array nme from the sdsc - should be
   * exactly a 1-dimensional array (since it's the sdsc).
   */
  int i, nme, sub;
  LOGICAL inl_flg = FALSE;
  if (STYPEG(sptr) == ST_MEMBER)
    nme = addnme(NT_MEM, sptr, basenm, (INT)0);
  else
    nme = addnme(NT_VAR, sptr, 0, (INT)0);

  /* ORIGDIM field not set for inlined variables
  assert(ORIGDIMG(sptr) == 1,"compute_nme: not 1-D",ORIGDIMG(sptr),3);
  */

  /* maybe if we have an INLELEM we need inl flg ? */
  sub = ad_icon(constant);
  nme = add_arrnme(NT_ARR, 0, nme, constant, sub, inl_flg);
  return nme;
}

/*
 * compute subscript expressions using descriptors
 * only for PGF90
 */

static int add_ptr_subscript(int, int, int, int, int, int, ADSC *, int, int);

extern int get_sdsc_element(int, int, int, int);
static void
compute_sdsc_subscr(ILM *ilmp)
{
  int i, fi, sdsc, nme;
  int ili1, ili2, ili3, ili4, ili5;
  int base = 0;
  int basenm = 0, basesym;
  int dtype;  /* array data type */
  ADSC *adp;  /* array descriptor */
  int arrilm; /* ilm for array */
  int sym = 0, sub;
  ILM *basep, *ilmp1;
  int any_kr;
  int ptrexpand = 0;
  int sub_1;
  int offset;
  ISZ_T coffset;
  int zoffset;
  int oldnme;

  dtype = ILM_OPND(ilmp, 3);
  adp = AD_DPTR(dtype);

  /*  useful information re: the storage class of sptr:
   *     assumed shape => SC_LOCAL, pointer => SC_BASED,
   *	   allocatable => SC_BASED , automatic => SC_DUMMY
   */

  sdsc = AD_SDSC(adp);
  assert(sdsc != 0, "compute_sdsc_subscr: sdsc is zero", sdsc, 3);
  PTRSAFEP(sdsc, 1);

  /* this code duplicates much of what is done in compute_subscr(),
   * filling in the the subscr fields, except for subscr.zbase and
   * subscr.offset, which are different here due to the late
   * linearization of assumed shape and pointer arrays.
   */

  subscr.nsubs = ILM_OPND(ilmp, 1);
  subscr.zbase = 0;
#if DEBUG
  assert(subscr.nsubs <= (sizeof(subscr.sub) / sizeof(int)),
         "compute_sdsc_subscr:nsubs exceeded", subscr.nsubs, 3);
#endif
  arrilm = ILM_OPND(ilmp, 2);
  subscr.eldt = DTY(dtype + 1); /* element data type */

  if (subscr.eldt != DT_ASSCHAR && subscr.eldt != DT_ASSNCHAR &&
      subscr.eldt != DT_DEFERCHAR && subscr.eldt != DT_DEFERNCHAR) {
    if (XBIT(68, 0x1)) {
      ISZ_T val;
      subscr.elmsz = ad_kconi(size_of(subscr.eldt));
      subscr.scale = Scale_Of(subscr.eldt, &val);
      subscr.elmscz = ad_kconi(val);
    } else {
      INT val;
      subscr.elmsz = ad_icon(size_of(subscr.eldt));
      subscr.scale = scale_of(subscr.eldt, &val);
      subscr.elmscz = ad_icon(val);
    }
  } else if (subscr.eldt == DT_DEFERCHAR || subscr.eldt == DT_DEFERNCHAR) {
    /* deferred-size character; size is in symtab */
    int bytes;

    ilmp1 = (ILM *)(ilmb.ilm_base + arrilm);
    if (ILM_OPC(ilmp1) == IM_PLD) {
      /* rewritten arguments */
      sym = ILM_OPND(ilmp1, 2);
      if (ORIGDUMMYG(sym) && !XBIT(57, 0x80000)) {
        /* still using pghpf_ptr_in/out */
        sym = ORIGDUMMYG(sym);
      }
    } else {
#if DEBUG
      assert(ILM_OPC(ilmp1) == IM_BASE,
             "compute_sdsc_subscr: DEFERCH array not base", arrilm, 3);
#endif
      sym = ILM_OPND(ilmp1, 1); /* symbol pointer */
    }
#if DEBUG
    assert((STYPEG(sym) == ST_ARRAY ||
            (STYPEG(sym) == ST_MEMBER &&
             (subscr.eldt == DT_DEFERCHAR || subscr.eldt == DT_DEFERNCHAR))),
           "compute_sdsc_subscr: ASSCH/DEFERCH sym not array", sym, 3);
#endif
    /* generate load of elem size */
    if (STYPEG(sym) == ST_MEMBER) {
      /* Could member be called in this function ever? */
      int base;
      ILM *basep;
      basep = (ILM *)(ilmb.ilm_base + ILM_OPND(ilmp1, 1));
      base = ILM_OPND(basep, 1);
      bytes = exp_get_sdsc_len(sym, ILI_OF(base), NME_OF(base));
    } else
      bytes = exp_get_sdsc_len(sym, 0, 0);
    if (subscr.eldt == DT_DEFERNCHAR) /* assumed size kanji dummy */
      bytes = ad2ili(IL_IMUL, bytes, ad_icon(2L));
    subscr.elmscz = subscr.elmsz = bytes;
    subscr.scale = 0;
  } else {
    /* assumed-size character; size is in symtab */
    int bytes;

    ilmp1 = (ILM *)(ilmb.ilm_base + arrilm);
    if (ILM_OPC(ilmp1) == IM_PLD) {
      /* rewritten arguments */
      sym = ILM_OPND(ilmp1, 2);
      if (ORIGDUMMYG(sym) && !XBIT(57, 0x80000)) {
        /* still using pghpf_ptr_in/out */
        sym = ORIGDUMMYG(sym);
      }
    } else {
#if DEBUG
      assert(ILM_OPC(ilmp1) == IM_BASE,
             "compute_sdsc_subscr: ASSCH/DEFERCH array not base", arrilm, 3);
#endif
      sym = ILM_OPND(ilmp1, 1); /* symbol pointer */
    }
#if DEBUG
    assert(STYPEG(sym) == ST_ARRAY,
           "compute_sdsc_subscr: ASSCH/DEFERCH sym not array", sym, 3);
#endif
    /* generate load of elem size */
    bytes = charlen(sym);
    if (subscr.eldt == DT_ASSNCHAR) /* assumed size kanji dummy */
      bytes = ad2ili(IL_IMUL, bytes, ad_icon(2L));
    subscr.elmscz = subscr.elmsz = bytes;
    subscr.scale = 0;
  }

  subscr.basenm = NME_OF(arrilm);
  basesym = 0;
  oldnme = subscr.basenm;

  /*
   * when XBIT(183,0x80000) is set, expand.c:update_local_nme() is called
   * and may produce an NT_IND
   */
  if (XBIT(183, 0x80000) && NME_TYPE(subscr.basenm) == NT_IND) {
    /* unsure if this code ever sees NT_IND of NT_IND of NT_VAR; if so
     * tmpsym is invalid -- revisit and perhaps add an assert of NT_VAR.
     */
    int tmpnme = NME_NM(subscr.basenm);
    int tmpsym = NME_SYM(tmpnme);
    if (gbl.outlined && tmpsym > 0 && PARREFG(tmpsym) &&
        !is_llvm_local_private(tmpsym))
      oldnme = tmpnme;
  }
  if (oldnme && NME_TYPE(oldnme) == NT_VAR) {
    basesym = NME_SYM(oldnme);
    /*
     * -Mcray=pointer could be in effect at this point; however,
     * if the pointee is a POINTER, the extra calculations are
     * still needed for
     *    real,target   :: arr(100)
     *    real, pointer :: p(:)
     *    p => arr(11:20)
     * With -Mcray=pointer, still need to check to see if ptrexpand
     * must be set.
     * Note that checks are similar to the PLD case, with the addition
     * of the NOCONFLICT check performed in expand.c (for facerec).
     */
    if (!XBIT(125, 0x400) && basesym && XBIT(58, 0x8000000) &&
        POINTERG(basesym) && !NOCONFLICTG(basesym))
      ptrexpand = 1;
  } else {
    basep = (ILM *)(ilmb.ilm_base + ILM_OPND(ilmp, 2));
    if (ILM_OPC(basep) == IM_PLD) {
      /* get corresponding symbol */
      basesym = ILM_OPND(basep, 2);
    }
    if (basesym && XBIT(58, 0x8000000) && POINTERG(basesym))
      ptrexpand = 1;
  }

  /* record subscripts in subscr.sub[i] */
  any_kr = 0;
  if (XBIT(125, 0x20000))
    any_kr = 1;
  for (i = 0; i < subscr.nsubs; ++i) {
    sub = ILI_OF(ILM_OPND(ilmp, 4 + i)); /* subscript ili */
    subscr.sub[i] = sub;
    if (IL_RES(ILI_OPC(sub)) == ILIA_KR)
      any_kr = 1;
  }
  if (any_kr) {
    for (i = 0; i < subscr.nsubs; ++i) {
      subscr.sub[i] = ikmove(subscr.sub[i]);
    }
  }
  sub_1 = subscr.sub[0];

  subscr.base = ILI_OF(arrilm);

  /* is the sdsc of type ST_MEMBER? */
  if (STYPEG(sdsc) == ST_MEMBER) {
    /* find the base ILM and NME */
    basep = (ILM *)(ilmb.ilm_base + ILM_OPND(ilmp, 2));
    assert(ILM_OPC(basep) == IM_PLD, "compute_sdsc_subscr: not PLD",
           ILM_OPND(ilmp, 2), 3);
    basep = (ILM *)(ilmb.ilm_base + ILM_OPND(basep, 1));
    assert(ILM_OPC(basep) == IM_MEMBER, "compute_sdsc_subscr: not MEMBER",
           ILM_OPND(ilmp, 2), 3);
    base = ILM_OPND(basep, 1);
    basenm = NME_OF(base);
    base = ILI_OF(base);
    assert(base, "compute_sdsc_subscr: base is NULL", base, 3);
  }

  /* compute the static descriptor linearized version of this
   * array reference.
   */
  fi = 0;
  if (!any_kr)
    offset = ad_icon(0);
  else
    offset = ad1ili(IL_KCON, stb.k0);
  if (!SDSCS1G(sdsc) && !CONTIGATTRG(basesym)) {
    ili1 = offset;
  } else {
    ili1 = subscr.sub[0];
    fi = 1;
  }
  coffset = 0;
  zoffset = 0;
  if (fi == 1) {
    /*
     * given a subscripted non-pointer array (assumed-shape),
     * if the first/left-most subscript is a constant, the initial value
     * of the constant offset is the subscript's value and the first
     * subscript must be set to 0.
     */
    if (!XBIT(125, 0x4000) && IL_TYPE(ILI_OPC(sub_1)) == ILTY_CONS) {
      coffset = get_isz_cval(ILI_OPND(sub_1, 1));
      ili1 = subscr.sub[0] = offset; /* the zero */
    } else if ((ILI_OPC(sub_1) == IL_IADD) &&
               ILI_OPC(ili2 = ILI_OPND(sub_1, 2)) == IL_ICON) {
      /*
       * subcript is of the form i + c, where c is a constant.
       */
      coffset = CONVAL2G(ILI_OPND(ili2, 1));
      ili1 = ILI_OPND(sub_1, 1);
    } else if ((ILI_OPC(sub_1) == IL_ISUB) &&
               ILI_OPC(ili2 = ILI_OPND(sub_1, 2)) == IL_ICON) {
      /*
       * subscript is of the form i - c, where c is a constant.
       */
      coffset = -CONVAL2G(ILI_OPND(ili2, 1));
      ili1 = ILI_OPND(sub_1, 1);
    } else if ((ILI_OPC(sub_1) == IL_KADD) &&
             ILI_OPC(ili2 = ILI_OPND(sub_1, 2)) == IL_KCON) {
      /*
       * subcript is of the form i + c, where c is a constant.
       */
      coffset = get_isz_cval(ILI_OPND(ili2, 1));
      ili1 = ILI_OPND(sub_1, 1);
    } else if ((ILI_OPC(sub_1) == IL_KSUB) &&
               ILI_OPC(ili2 = ILI_OPND(sub_1, 2)) == IL_KCON) {
      /*
       * subscript is of the form i - c, where c is a constant.
       */
      coffset = -get_isz_cval(ILI_OPND(ili2, 1));
      ili1 = ILI_OPND(sub_1, 1);
    }
  }
  for (i = fi; i < subscr.nsubs; ++i) {
    /* let DIM_x(i) be DESC_HDR_LEN + i*DESC_DIM_LEN + DESC_DIM_x
     *  subscript term = subscr.sub[i] * sd[DIM_LMULT(i)]
     * if ptrexpand is set, the term is more complex:
     * subscript term =
     *  (subscr.sub[i] * sd[DIM_SSTRIDE(i)] + sd[DIM_SOFFSET(i)]) *
     * sd[DIM_LMULT(i)]
     */
    ili1 = add_ptr_subscript(i, subscr.sub[i], ili1, base, basesym, basenm, adp,
                             ptrexpand, any_kr);
  }
  /* offset is in ili1 */
  if (XBIT(57, 0x10000) && basesym &&
      ((SCG(basesym) == SC_DUMMY && !POINTERG(basesym))
#ifdef INLNARRG
       || (INLNARRG(basesym))
#endif
           )) {
    int zbase, nme;
    if (any_kr) {
      ili1 = ikmove(ili1);
    }
    /* the front end has folded the offset computation
     * for assumed-shape dummies into the ZBASE field */
    zbase = AD_ZBASE(adp);
    ili3 = mk_address(zbase);
    nme = addnme(NT_VAR, zbase, 0, (INT)0);
    if (DTYPEG(zbase) == DT_INT8)
      ili3 = ad3ili(IL_LDKR, ili3, nme, MSZ_I8);
    else {
      ili3 = ad3ili(IL_LD, ili3, nme, MSZ_WORD);
      if (any_kr)
        ili3 = ad1ili(IL_IKMV, ili3);
    }
    if (!any_kr) {
      ili1 = ad2ili(IL_IADD, ili1, ili3);
    } else {
      ili1 = ad2ili(IL_KADD, ili1, ili3);
    }
    subscr.offset = ili1;
  } else {
    /* add the lower bound - local base index offset folds in
     * amount so that zero-based references work. It's
     * located at $sd(DESC_HDR_LBASE).
     */
    ili3 = get_sdsc_element(sdsc, DESC_HDR_LBASE, base, basenm);
    if (!any_kr)
      subscr.offset = ad2ili(IL_IADD, ili1, ili3);
    else {
      ili3 = ikmove(ili3);
      subscr.offset = ad2ili(IL_KADD, ili1, ili3);
    }
  }
  if (!SDSCS1G(sdsc) && !CONTIGATTRG(basesym) && !XBIT(28, 0x20)) {
/*
 * A pointer array may not be contiguous, so using the 'element'
 * size as the final multiplier is insufficient.
 * Define the multiplier to be the 'byte length' as stored in the
 * descriptor; this is the length between elements of the array
 * and is located at $sd(DESC_HDR_BYTE_LEN).
 */
#ifdef SDSCCONTIGG
    if (!SDSCCONTIGG(sdsc))
#endif
    {
      subscr.scale = 0;
      subscr.elmscz =
          kimove(get_sdsc_element(sdsc, DESC_HDR_BYTE_LEN, base, basenm));
    }
    if (subscr.zbase == 0) {
      if (any_kr)
        subscr.zbase = ad_kconi(1);
      else
        subscr.zbase = ad_icon(1);
    }
  } else if (subscr.zbase == 0) {
    if (any_kr)
      subscr.zbase = ad_kconi(1);
    else
      subscr.zbase = ad_icon(1);
  } else {
    subscr.zbase = ad2ili(IL_IADD, subscr.zbase, ad_icon(1));
    if (any_kr) {
      subscr.zbase = ad1ili(IL_IKMV, subscr.zbase);
    }
  }
  subscr.sub[0] = sub_1;
  if (coffset) {
    if (any_kr)
      subscr.zbase = ad2ili(IL_KSUB, subscr.zbase, ad_kconi(coffset));
    else
      subscr.zbase = ad2ili(IL_ISUB, subscr.zbase, ad_icon(coffset));
  }


  if (zoffset) {
/*
 * Moving zoffset into the base will ultimately yield adding
 * an IAMV/KAMV to both operands of an AADD.  iliutil.c:addarth()
 * will combine the operands of the IAMV/KAMV, so need to ensure
 * that XBIT(15,0x100) is default or temporarily set.
 * NOTE -- unless the code above which sets zoffset to the first
 * subscript if constant is enabled, zoffset is always zero.
 */
    if (any_kr) {
      ili2 = ikmove(subscr.elmscz);
      if (ILI_OPC(zoffset) == IL_KMUL) {
        /*
         *  zoffset <-- <stride> * cnst; form
         *  zoffset <-- (<stride> * <elmscz>) * cnst
         */
        ili2 = ad2ili(IL_KMUL, ILI_OPND(zoffset, 1), ili2);
        ili2 = ad2ili(IL_KMUL, ili2, ILI_OPND(zoffset, 2));
      } else {
        /*
         *  zoffset <-- <stride>; form
         *  zoffset <-- <stride> * <elmscz>)
         */
        /**** zoffset <-- <<<sdsc_stride>>> ****/
        ili2 = ad2ili(IL_KMUL, zoffset, ili2);
      }
      ili2 = ad1ili(IL_KAMV, ili2);
    } else {
      if (ILI_OPC(zoffset) == IL_IMUL) {
        /*
         *  zoffset <-- <stride> * cnst; form
         *  zoffset <-- (<stride> * <elmscz>) * cnst
         */
        ili2 = ad2ili(IL_IMUL, ILI_OPND(zoffset, 1), subscr.elmscz);
        ili2 = ad2ili(IL_IMUL, ili2, ILI_OPND(zoffset, 2));
      } else {
        /*
         *  zoffset <-- <stride>; form
         *  zoffset <-- <stride> * <elmscz>)
         */
        ili2 = ad2ili(IL_IMUL, zoffset, subscr.elmscz);
      }
      ili2 = ad1ili(IL_IAMV, ili2);
    }
    subscr.base = ad3ili(IL_AADD, subscr.base, ili2, 0);
  }

}

static int
add_ptr_subscript(int i, int sub, int ili1, int base, int basesym, int basenm,
                  ADSC *adp, int ptrexpand, int any_kr)
{
  int ili2, ili3, ili4, ili5;
  int val;
  int sdsc;

  sdsc = AD_SDSC(adp);
  ili2 = sub;
  ili4 = 0;
  ili5 = 0;
  if (XBIT(57, 0x10000) && basesym &&
      ((SCG(basesym) == SC_DUMMY && !POINTERG(basesym))
#ifdef INLNARRG
       || (INLNARRG(basesym))
#endif
           )) {
    int m, nme;
    m = AD_MLPYR(adp, i);
    ili3 = mk_address(m);
    nme = addnme(NT_VAR, m, 0, (INT)0);
    if (DTYPEG(m) == DT_INT8)
      ili3 = ad3ili(IL_LDKR, ili3, nme, MSZ_I8);
    else
      ili3 = ad3ili(IL_LD, ili3, nme, MSZ_WORD);
    /* ### probably need to check this for ptrexpand, for inlined routines */
  } else {
    if (ptrexpand) {
      if (!XBIT(58, 0x40000000)) {
        /* with section stride/offset */
        val = DESC_HDR_LEN + i * DESC_DIM_LEN + DESC_DIM_SSTRIDE;
        ili4 = get_sdsc_element(sdsc, val, base, basenm);
        val = DESC_HDR_LEN + i * DESC_DIM_LEN + DESC_DIM_SOFFSET;
        ili5 = get_sdsc_element(sdsc, val, base, basenm);
      }
    }
    /* the (i+1)st dimension subscript ili is located at subscr.sub[i] */
    val = DESC_HDR_LEN + i * DESC_DIM_LEN + DESC_DIM_LMULT;
    ili3 = get_sdsc_element(sdsc, val, base, basenm);
  }
  if (!any_kr) {
    if (!XBIT(58, 0x40000000)) {
      /* with section stride/offset */
      if (ptrexpand && ili5) {
        ili2 = ad2ili(IL_IMUL, ili2, ili4);
        ili2 = ad2ili(IL_IADD, ili2, ili5);
      }
      ili2 = ad2ili(IL_IMUL, ili2, ili3);
    } else {
      /* no section stride/offset */
      ili2 = ad2ili(IL_IMUL, ili2, ili3);
      if (ptrexpand && ili5) {
        ili2 = ad2ili(IL_IADD, ili2, ili5);
      }
    }
    ili1 = ad2ili(IL_IADD, ili1, ili2);
  } else {
    if (DTYG(DTYPEG(sdsc)) == DT_INT) {
      ili3 = ad1ili(IL_IKMV, ili3);
      if (ptrexpand && ili5) {
        if (ili4)
          ili4 = ad1ili(IL_IKMV, ili4);
        ili5 = ad1ili(IL_IKMV, ili5);
      }
    }
    if (!XBIT(58, 0x40000000)) {
      /* with section stride/offset */
      if (ptrexpand && ili5) {
        ili2 = ad2ili(IL_KMUL, ili2, ili4);
        ili2 = ad2ili(IL_KADD, ili2, ili5);
      }
      ili2 = ad2ili(IL_KMUL, ili2, ili3);
    } else {
      /* no section stride/offset */
      ili2 = ad2ili(IL_KMUL, ili2, ili3);
      if (ptrexpand && ili5) {
        ili2 = ad2ili(IL_KADD, ili2, ili5);
      }
    }
    ili1 = ad2ili(IL_KADD, ili1, ili2);
  }
  return ili1;
}

int
get_sdsc_element(int sdsc, int indx, int membase, int membase_nme)
{
  int acon, ili;
  int scale, elmsz;
  if (CLASSG(sdsc)) {
/* Special case for type descriptors and -Mlarge_arrays
 * or -mcmodel=medium. We can't compute the descriptor element size
 * from the element dtype since we store the derived type dtype record
 * that's associated with this type descriptor in DTY(dtype+1). So,
 *  we assume DT_INT (or stb.il) by default and DT_INT8 (or stb.k1) for
 * -Mlarge_arrays and -mcmodel=medium.
 */
    if (XBIT(68, 0x1))
      scale = scale_of(DTYPEG(stb.k1), &elmsz);
    else
      scale = scale_of(DTYPEG(stb.i1), &elmsz);
  } else
    scale = scale_of(DTYG(DTYPEG(sdsc)),
                     &elmsz); /* element size of sdsc is integer */

  if (membase)
    acon = ad3ili(IL_AADD, membase,
                  ad_aconi(ADDRESSG(sdsc) + elmsz * (indx - 1)), scale);

  else {
    if (SCG(sdsc) == SC_CMBLK && IS_THREAD_TP(sdsc)) {
      /*
       * BASE is of a member which is in a threadprivate common.
       * generate an indirection using the threadprivate common's
       * vector and then add the offset of this member. The
       * indirection will be of the form:
       *    vector[_mp_lcpu3()]
       */
      int nm;
      int adr;
      ref_threadprivate(sdsc, &adr, &nm);
      acon = adr;
    } else if (IS_THREAD_TP(sdsc)) {
      /*
       * BASE is a threadprivate variable; generate an indirection using
       * the threadprivate's vector.  The indirection will be of the form:
       *    vector[_mp_lcpu3()]
       */
      int nm;
      int adr;
      ref_threadprivate_var(sdsc, &adr, &nm, 0);
      acon = adr;
    } else if (SCG(sdsc) == SC_BASED) {
      int anme;
      if (!MIDNUMG(sdsc)) {
        interr("based section descriptor has no pointer", sdsc, 4);
      }
      acon = mk_address(MIDNUMG(sdsc));
      anme = addnme(NT_VAR, sdsc, 0, (INT)0);
      acon = ad2ili(IL_LDA, acon, anme);
    } else {
      acon = mk_address(sdsc);
      if (SCG(sdsc) == SC_DUMMY
          && (!flg.smp || (flg.smp && is_llvm_local_private(sdsc))) &&
          !(gbl.internal > 1 && INTERNREFG(sdsc))
              ) {
        int asym, anme;
        asym = mk_argasym(sdsc);
        anme = addnme(NT_VAR, asym, 0, (INT)0);
        acon = ad2ili(IL_LDA, acon, anme);
        ADDRCAND(acon, anme);
      }
    }
    acon = ad3ili(IL_AADD, acon, ad_aconi(elmsz * (indx - 1)), scale);
  }
  if (elmsz == 8)
    ili = ad3ili(IL_LDKR, acon, compute_nme(sdsc, indx, membase_nme), MSZ_I8);
  else
    ili = ad3ili(IL_LD, acon, compute_nme(sdsc, indx, membase_nme), MSZ_WORD);
  return ili;
}

static void
create_sdsc_subscr(int nmex, int sptr, int nsubs, int *subs, int dtype,
                   int ilix, int sdscilix)
{
  int i, fi, sdsc, nme;
  int ili1, ili2, ili3, ili4, ili5;
  int base = 0;
  int basenm = 0, basesym;
  ADSC *adp; /* array descriptor */
  int sym = 0, sub;
  ILM *ilmp1;
  int any_kr;
  int ptrexpand = 0;

  adp = AD_DPTR(dtype);

  /*  useful information re: the storage class of sptr:
   *     assumed shape => SC_LOCAL, pointer => SC_BASED,
   *	   allocatable => SC_BASED , automatic => SC_DUMMY
   */

  sdsc = AD_SDSC(adp);
  assert(sdsc != 0, "create_sdsc_subscr: sdsc is zero", sdsc, 3);
  PTRSAFEP(sdsc, 1);

  /* this code duplicates much of what is done in compute_subscr(),
   * filling in the the subscr fields, except for subscr.zbase and
   * subscr.offset, which are different here due to the late
   * linearization of assumed shape and pointer arrays.
   */

  subscr.nsubs = nsubs;
  subscr.zbase = 0;
#if DEBUG
  assert(subscr.nsubs <= (sizeof(subscr.sub) / sizeof(int)),
         "create_sdsc_subscr:nsubs exceeded", subscr.nsubs, 3);
#endif
  subscr.eldt = DTY(dtype + 1); /* element data type */

  if (subscr.eldt != DT_ASSCHAR && subscr.eldt != DT_ASSNCHAR &&
      subscr.eldt != DT_DEFERCHAR && subscr.eldt != DT_DEFERNCHAR) {
    INT val;
    subscr.elmsz = ad_icon(size_of(subscr.eldt));
    subscr.scale = scale_of(subscr.eldt, &val);
    subscr.elmscz = ad_icon(val);
  } else if (subscr.eldt == DT_DEFERCHAR || subscr.eldt == DT_DEFERNCHAR) {
    /* defered-size character; size is in symtab */
    int bytes;

    sym = sptr;
    if (ORIGDUMMYG(sym) && !XBIT(57, 0x80000)) {
      /* still using pghpf_ptr_in/out */
      sym = ORIGDUMMYG(sym);
    }
#if DEBUG
    assert(STYPEG(sym) == ST_ARRAY,
           "compute_sdsc_subscr: DEFERCH sym not array", sym, 3);
#endif
    /* generate load of elem size */
    if (STYPEG(sym) == ST_MEMBER) {
      bytes = exp_get_sdsc_len(sym, ilix, NME_NM(nmex));
    } else
      bytes = exp_get_sdsc_len(sym, 0, 0);
    if (subscr.eldt == DT_DEFERNCHAR) { /* assumed size kanji dummy */
      if (XBIT(68, 0x20)) {
        bytes = ad2ili(IL_KMUL, bytes, ad_kcon(0, 2));
      } else
      {
        bytes = ad2ili(IL_IMUL, bytes, ad_icon(2L));
      }
    }
    subscr.elmscz = subscr.elmsz = bytes;
    subscr.scale = 0;
  } else {
    /* assumed-size character; size is in symtab */
    int bytes;

    sym = sptr;
    if (ORIGDUMMYG(sym) && !XBIT(57, 0x80000)) {
      /* still using pghpf_ptr_in/out */
      sym = ORIGDUMMYG(sym);
    }
#if DEBUG
    assert(STYPEG(sym) == ST_ARRAY, "compute_sdsc_subscr: ASSCH sym not array",
           sym, 3);
#endif
    /* generate load of elem size */
    bytes = charlen(sym);
    if (subscr.eldt == DT_ASSNCHAR) /* assumed size kanji dummy */
      bytes = ad2ili(IL_IMUL, bytes, ad_icon(2L));
    subscr.elmscz = subscr.elmsz = bytes;
    subscr.scale = 0;
  }

  subscr.basenm = nmex;
  basesym = 0;
  if (NME_TYPE(subscr.basenm) == NT_IND) {
    int tmpnme = NME_NM(subscr.basenm);
    int tmpsym = NME_SYM(tmpnme);
    if (gbl.outlined && PARREFG(tmpsym) && !is_llvm_local_private(tmpsym))
      subscr.basenm = tmpnme;
  }
  if (subscr.basenm && NME_TYPE(subscr.basenm) == NT_VAR) {
    basenm = nmex;
    basesym = NME_SYM(subscr.basenm);
    /*
     * -Mcray=pointer could be in effect at this point; however,
     * if the pointee is a POINTER, the extra calculations are
     * still needed for
     *    real,target   :: arr(100)
     *    real, pointer :: p(:)
     *    p => arr(11:20)
     * With -Mcray=pointer, still need to check to see if ptrexpand
     * must be set.
     * Note that checks are similar to the PLD case, with the addition
     * of the NOCONFLICT check performed in expand.c (for facerec).
     */
    if (!XBIT(125, 0x400) && basesym && XBIT(58, 0x8000000) &&
        POINTERG(basesym) && !NOCONFLICTG(basesym))
      ptrexpand = 1;
  } else {
    /* ### */
    if (basesym && XBIT(58, 0x8000000) && POINTERG(basesym))
      ptrexpand = 1;
    if (nmex && NME_TYPE(nmex) == NT_MEM) {
      basenm = NME_NM(nmex);
    }
  }

  /* record subscripts in subscr.sub[i] */
  any_kr = 0;
  if (XBIT(125, 0x20000))
    any_kr = 1;
  for (i = 0; i < subscr.nsubs; ++i) {
    sub = subs[i];
    subscr.sub[i] = sub;
    if (IL_RES(ILI_OPC(sub)) == ILIA_KR)
      any_kr = 1;
  }
  if (any_kr) {
    for (i = 0; i < subscr.nsubs; ++i) {
      subscr.sub[i] = ikmove(subscr.sub[i]);
    }
  }

  subscr.base = ilix;
  base = 0;

  /* is the sdsc of type ST_MEMBER? */
  if (STYPEG(sdsc) == ST_MEMBER) {
    /* find the base ILM and NME */
    base = sdscilix;
    assert(base, "compute_sdsc_subscr: base is NULL", base, 3);
  }

  /* compute the static descriptor linearized version of this
   * array reference.
   */
  if (!SDSCS1G(sdsc)) {
    if (!any_kr)
      ili1 = ad_icon(0);
    else
      ili1 = ad1ili(IL_KCON, stb.k0);
    fi = 0;
  } else {
    ili1 = subscr.sub[0];
    fi = 1;
  }
  for (i = fi; i < subscr.nsubs; ++i) {
    /* let DIM_x(i) be DESC_HDR_LEN + i*DESC_DIM_LEN + DESC_DIM_x
     *  subscript term = subscr.sub[i] * sd[DIM_LMULT(i)]
     * if ptrexpand is set, the term is more complex:
     * subscript term =
     *  (subscr.sub[i] * sd[DIM_SSTRIDE(i)] + sd[DIM_SOFFSET(i)]) *
     * sd[DIM_LMULT(i)]
     */
    ili2 = subscr.sub[i];
    ili4 = 0;
    ili5 = 0;
    if (XBIT(57, 0x10000) && basesym &&
        ((SCG(basesym) == SC_DUMMY && !POINTERG(basesym))
#ifdef INLNARRG
         || (INLNARRG(basesym))
#endif
             )) {
      int m, nme;
      m = AD_MLPYR(adp, i);
      ili3 = mk_address(m);
      nme = addnme(NT_VAR, m, 0, (INT)0);
      if (DTYPEG(m) == DT_INT8)
        ili3 = ad3ili(IL_LDKR, ili3, nme, MSZ_I8);
      else
        ili3 = ad3ili(IL_LD, ili3, nme, MSZ_WORD);
      /* ### probably need to check this for ptrexpand, for inlined routines */
    } else {
      int j;
      if (ptrexpand) {
        if (!XBIT(58, 0x40000000)) {
          /* with section stride/offset */
          j = DESC_HDR_LEN + i * DESC_DIM_LEN + DESC_DIM_SSTRIDE;
          ili4 = get_sdsc_element(sdsc, j, base, basenm);
          j = DESC_HDR_LEN + i * DESC_DIM_LEN + DESC_DIM_SOFFSET;
          ili5 = get_sdsc_element(sdsc, j, base, basenm);
        }
      }
      /* the (i+1)st dimension subscript ili is located at subscr.sub[i] */
      j = DESC_HDR_LEN + i * DESC_DIM_LEN + DESC_DIM_LMULT;
      ili3 = get_sdsc_element(sdsc, j, base, basenm);
    }
    if (!any_kr) {
      if (!XBIT(58, 0x40000000)) {
        /* with section stride/offset */
        if (ptrexpand && ili5) {
          ili2 = ad2ili(IL_IMUL, ili2, ili4);
          ili2 = ad2ili(IL_IADD, ili2, ili5);
        }
        ili2 = ad2ili(IL_IMUL, ili2, ili3);
      } else {
        /* no section stride/offset */
        ili2 = ad2ili(IL_IMUL, ili2, ili3);
        if (ptrexpand && ili5) {
          ili2 = ad2ili(IL_IADD, ili2, ili5);
        }
      }
      ili1 = ad2ili(IL_IADD, ili1, ili2);
    } else {
      ili3 = ikmove(ili3);
      if (ptrexpand && ili5) {
        if (ili4)
          ili4 = ikmove(ili4);
        ili5 = ikmove(ili5);
      }
      if (!XBIT(58, 0x40000000)) {
        /* with section stride/offset */
        if (ptrexpand && ili5) {
          ili2 = ad2ili(IL_KMUL, ili2, ili4);
          ili2 = ad2ili(IL_KADD, ili2, ili5);
        }
        ili2 = ad2ili(IL_KMUL, ili2, ili3);
      } else {
        /* no section stride/offset */
        ili2 = ad2ili(IL_KMUL, ili2, ili3);
        if (ptrexpand && ili5) {
          ili2 = ad2ili(IL_KADD, ili2, ili5);
        }
      }
      ili1 = ad2ili(IL_KADD, ili1, ili2);
    }
  }
  /* offset is in ili1 */
  if (XBIT(57, 0x10000) && basesym &&
      ((SCG(basesym) == SC_DUMMY && !POINTERG(basesym))
#ifdef INLNARRG
       || (INLNARRG(basesym))
#endif
           )) {
    int zbase, nme;
    if (any_kr)
      ili1 = ikmove(ili1);
    /* the front end has folded the offset computation
     * for assumed-shape dummies into the ZBASE field */
    zbase = AD_ZBASE(adp);
    ili3 = mk_address(zbase);
    nme = addnme(NT_VAR, zbase, 0, (INT)0);
    if (DTYPEG(zbase) == DT_INT8)
      ili3 = ad3ili(IL_LDKR, ili3, nme, MSZ_I8);
    else {
      ili3 = ad3ili(IL_LD, ili3, nme, MSZ_WORD);
      if (any_kr)
        ili3 = ikmove(ili3);
    }
    if (!any_kr) {
      ili1 = ad2ili(IL_IADD, ili1, ili3);
    } else {
      ili1 = ad2ili(IL_KADD, ili1, ili3);
    }
    subscr.offset = ili1;
  } else {
    /* add the lower bound - local base index offset folds in
     * amount so that zero-based references work. It's
     * located at $sd(DESC_HDR_LBASE).
     */
    ili3 = get_sdsc_element(sdsc, DESC_HDR_LBASE, base, basenm);
    if (!any_kr)
      subscr.offset = ad2ili(IL_IADD, ili1, ili3);
    else {
      ili3 = ikmove(ili3);
      subscr.offset = ad2ili(IL_KADD, ili1, ili3);
    }
  }
  if (!SDSCS1G(sdsc) && !CONTIGATTRG(basesym) && !XBIT(28, 0x20)) {
/*
 * A pointer array may not be contiguous, so using the 'element'
 * size as the final multiplier is insufficient.
 * Define the multiplier to be the 'byte length' as stored in the
 * descriptor; this is the length between elements of the array
 * and is located at $sd(DESC_HDR_BYTE_LEN).
 */
#ifdef SDSCCONTIGG
    if (!SDSCCONTIGG(sdsc))
#endif
    {
      subscr.scale = 0;
      subscr.elmscz = get_sdsc_element(sdsc, DESC_HDR_BYTE_LEN, base, basenm);
      if (any_kr)
        subscr.elmscz = ikmove(subscr.elmscz);
    }
    if (subscr.zbase == 0) {
      if (any_kr)
        subscr.zbase = ad_kconi(1);
      else
        subscr.zbase = ad_icon(1);
    }
  } else if (subscr.zbase == 0) {
    if (any_kr)
      subscr.zbase = ad_kconi(1);
    else
      subscr.zbase = ad_icon(1);
  } else {
    subscr.zbase = ad2ili(IL_IADD, subscr.zbase, ad_icon(1));
    if (any_kr)
      subscr.zbase = ikmove(subscr.zbase);
  }

}

/**
 * the current ilm is an INLELEM which is generated for the subscripted
 * references of the dummy arrays of a subprogram which has been inlined.
 * This ilm is used for cases where the name of the actual array cannot
 * be substituted (e.g., the dimensions change).  The goal of this ilm
 * is to generate a subscript expression where the subscripts used for the
 * dummy array are folded into the first subscript of the actual array;
 * doing this allows the name's entry of the actual to be used.
 * Note that it's assumed that the data types of the actual & dummy arrays
 * match.
 *
 * This routine is recursive, where the subscripts are evaluated beginning
 * with the base reference (ELEMENT, BASE, or MEMBER ilms).  Because of
 * multi-level inlining, it's possible to have a "list" of INLELEM ilms
 * which ultimately locates the base.
 */
static void
inlarr(int curilm, int odtype, LOGICAL bigobj)
{
  ILM *ilmp;
  int nsubs; /* # subscripts */
  ADSC *adp; /* array descriptor */
  int dtype; /* array data type */
  int zbase; /* zbase sym/ili ptr */
  int i;
  int sym;
  int sub, sub_1;
  int mplyr;
  int offset;
  int ili2;
  ISZ_T coffset;
  int nme;
  int tmp;
  LOGICAL any_kr;
  int sdsc, base, basenm;
#if DEBUG
  FILE *dbgfil;
#endif

  ilmp = (ILM *)(ilmb.ilm_base + curilm);

  switch (ILM_OPC(ilmp)) {

  case IM_INLELEM:
    dtype = ILM_OPND(ilmp, 3);
    inlarr((int)ILM_OPND(ilmp, 2), dtype, bigobj); /* compute subscr struct */
    nsubs = ILM_OPND(ilmp, 1);
    adp = AD_DPTR(dtype);
#if DEBUG
    if (DBGBIT(49, 0x4000)) {
      dbgfil = stderr;
      if (gbl.dbgfil)
        dbgfil = gbl.dbgfil;
      fprintf(dbgfil, "INLELEM, %d=dtype\n", dtype);
      dumpdtype(dtype);
    }
#endif

    /* fold  together the zero-base offsets of the actual and dummy */
    zbase = genload(AD_ZBASE(adp), bigobj); /* ili for zero-based offset */
    zbase = ad2ili(bigobj ? IL_KADD : IL_IADD, zbase, subscr.zbase);
#if DEBUG
    if (DBGBIT(49, 0x4000)) {
      fprintf(dbgfil, "INLELEM, %d=initial zbase\n", zbase);
      dilitre(zbase);
    }
#endif
    /*
     * calculate offset; first subscript begins with the ili of the
     * first subscript in subscr
     */
    coffset = 0;
    any_kr = bigobj;
    if (XBIT(125, 0x20000))
      any_kr = TRUE;
    /*
     * scan the subscripts of the dummy, record them and sum up the
     * products of the subscripts and their multipliers.  All subscripts
     * of the inlined reference are folded into the first subscript as
     * just an offset expression:  (sum of  s * m) - zbase, where s is
     * the subscript, m is the multiplier, zbase is the zero-base offset.
     */
    for (i = 0; i < subscr.nsubs; ++i) {
      sub = ILI_OF(ILM_OPND(ilmp, 4 + i)); /* subscript ili */
      subscr.sub[i] = sub;
      if (IL_RES(ILI_OPC(sub)) == ILIA_KR)
        any_kr = 1;
    }
    if (any_kr && !bigobj) {
      for (i = 0; i < subscr.nsubs; ++i) {
        subscr.sub[i] = ikmove(subscr.sub[i]);
      }
    }
    offset = sel_icnst(0, any_kr);

    /*
     * if the first/left-most subscript is a constant, the initial value
     * of the constant offset is the subscript's value and the first
     * subscript must be set to 0.
     */
    sub_1 = subscr.sub[0];
    if (!XBIT(125, 0x4000) && IL_TYPE(ILI_OPC(sub_1)) == ILTY_CONS) {
      coffset = get_isz_cval(ILI_OPND(sub_1, 1));
      subscr.sub[0] = offset; /* the zero */
    }

    for (i = 0; i < nsubs; ++i) {
      sub = subscr.sub[i];                       /* subscript ili */
      mplyr = genload(AD_MLPYR(adp, i), bigobj); /* ili for multiplier */
      if (any_kr)
        mplyr = ikmove(mplyr);
      tmp = ad2ili(any_kr ? IL_KMUL : IL_IMUL, sub, mplyr); /* sub * m */
      sub_1 = ad2ili(any_kr ? IL_KADD : IL_IADD, sub_1, tmp);
      /* offset += sub * mplyr */
      if (ILI_OPC(mplyr) == IL_ICON) {
        if ((ILI_OPC(sub) == IL_IADD) &&
            ILI_OPC(ili2 = ILI_OPND(sub, 2)) == IL_ICON) {
          /*
           * subcript is of the form i + c, where c is a constant. the
           * value c*mlpyr is accumulated and i becomes sub.
           */
          coffset += CONVAL2G(ILI_OPND(ili2, 1)) * CONVAL2G(ILI_OPND(mplyr, 1));
          sub = ILI_OPND(sub, 1);
        } else if ((ILI_OPC(sub) == IL_ISUB) &&
                   ILI_OPC(ili2 = ILI_OPND(sub, 2)) == IL_ICON) {
          /*
           * subcript is of the form i - c, where c is a constant. the
           * value c*mlpyr is accumulated and i becomes sub.
           */
          coffset -= CONVAL2G(ILI_OPND(ili2, 1)) * CONVAL2G(ILI_OPND(mplyr, 1));
          sub = ILI_OPND(sub, 1);
        } else if ((ILI_OPC(sub) == IL_KADD) &&
                 ILI_OPC(ili2 = ILI_OPND(sub, 2)) == IL_KCON) {
          /*
           * subcript is of the form i + c, where c is a constant. the
           * value c*mlpyr is accumulated and i becomes sub.
           */
          coffset +=
              get_isz_cval(ILI_OPND(ili2, 1)) * CONVAL2G(ILI_OPND(mplyr, 1));
          sub = ILI_OPND(sub, 1);
        } else if ((ILI_OPC(sub) == IL_KSUB) &&
                   ILI_OPC(ili2 = ILI_OPND(sub, 2)) == IL_KCON) {
          /*
           * subcript is of the form i - c, where c is a constant. the
           * value c*mlpyr is accumulated and i becomes sub.
           */
          coffset -=
              get_isz_cval(ILI_OPND(ili2, 1)) * CONVAL2G(ILI_OPND(mplyr, 1));
          sub = ILI_OPND(sub, 1);
        }
      } else if (ILI_OPC(mplyr) == IL_KCON) {
        if ((ILI_OPC(sub) == IL_KADD) &&
            ILI_OPC(ili2 = ILI_OPND(sub, 2)) == IL_KCON) {
          /*
           * subcript is of the form i + c, where c is a constant. the
           * value c*mlpyr is accumulated and i becomes sub.
           */
          coffset +=
              ad_val_of(ILI_OPND(ili2, 1)) * ad_val_of(ILI_OPND(mplyr, 1));
          sub = ILI_OPND(sub, 1);
        } else if ((ILI_OPC(sub) == IL_KSUB) &&
                   ILI_OPC(ili2 = ILI_OPND(sub, 2)) == IL_KCON) {
          /*
           * subcript is of the form i - c, where c is a constant. the
           * value c*mlpyr is accumulated and i becomes sub.
           */
          coffset -=
              ad_val_of(ILI_OPND(ili2, 1)) * ad_val_of(ILI_OPND(mplyr, 1));
          sub = ILI_OPND(sub, 1);
        }
      }
      ili2 = ad2ili(any_kr ? IL_KMUL : IL_IMUL, sub, mplyr);
      offset = ad2ili(any_kr ? IL_KADD : IL_IADD, offset, ili2);
    }
#if DEBUG
    if (DBGBIT(49, 0x4000)) {
      fprintf(dbgfil, "INLELEM, %d=offset, %d=coffset, %d=sub_1\n", offset,
              (int)coffset, sub_1);
      dilitre(offset);
      dilitre(sub_1);
    }
#endif
    /*
     * update the zero-based offset, the subscript-offset expression,
     * and the first subscript
     */
    if (coffset)
      zbase =
          ad2ili(any_kr ? IL_KSUB : IL_ISUB, zbase, sel_icnst(coffset, any_kr));
    subscr.zbase = zbase;
    subscr.offset = ad2ili(any_kr ? IL_KADD : IL_IADD, offset,
                           sel_iconv(subscr.offset, any_kr));
    tmp = genload(AD_ZBASE(adp), any_kr); /* ili for zero-based offset */
    sub_1 = ad2ili(any_kr ? IL_KSUB : IL_ISUB, sub_1, sel_iconv(tmp, any_kr));
    subscr.sub[0] = sub_1;
#if DEBUG
    if (DBGBIT(49, 0x4000)) {
      fprintf(dbgfil, "INLELEM, %d=final zbase, %d=final offset, %d=sub[1]\n",
              zbase, subscr.offset, sub_1);
      dilitre(zbase);
      dilitre(subscr.offset);
      dilitre(sub_1);
    }
#endif
    break;
  case IM_ELEMENT:
    dtype = ILM_OPND(ilmp, 3);
    adp = AD_DPTR(dtype);
    if (!XBIT(52, 4) && AD_SDSC(adp)) {
      /* Assumed shape and pointer arrays have not been previously
       * linearized in terms of their sdsc. Do that now if necessary.
       */
      compute_sdsc_subscr(ilmp);
    } else
      compute_subscr(ilmp, bigobj);
    break;
  case IM_BASE:
    sym = ILM_OPND(ilmp, 1);
    goto base_sym;
  case IM_PLD:
    sym = ILM_OPND(ilmp, 2);
    goto base_sym;
  case IM_MEMBER:
    sym = ILM_OPND(ilmp, 2);
  base_sym:
    /*
     * for a symbol-based reference (i.e., not an ELEMENT), extract all
     * information from the symbol.  Since we know subscripts are necessary,
     * create a subscripted reference whose subscripts are the lower bounds
     * of the dimensions.
     */
    dtype = DTYPEG(sym);
#if DEBUG
    assert(DTY(dtype) == TY_ARRAY, "inlarr:BASE/MEMBER-not TY_ARRAY", sym, 3);
#endif
    adp = AD_DPTR(dtype);
    sdsc = AD_SDSC(adp);
    PTRSAFEP(sdsc, 1);
    base = basenm = 0;
    if (STYPEG(sdsc) == ST_MEMBER) {
      ILM *basep;
      /* find the base ILM and NME */
      basep = (ILM *)(ilmb.ilm_base + ILM_OPND(ilmp, 2));
      assert(ILM_OPC(basep) == IM_PLD, "inlarr: not PLD", ILM_OPND(ilmp, 2), 3);
      basep = (ILM *)(ilmb.ilm_base + ILM_OPND(basep, 1));
      assert(ILM_OPC(basep) == IM_MEMBER, "inlarr: not MEMBER",
             ILM_OPND(ilmp, 1), 3);
      base = ILM_OPND(basep, 1);
      basenm = NME_OF(base);
      base = ILI_OF(base);
      assert(base, "inlarr: base is NULL", base, 3);
    }
#if DEBUG
    if (DBGBIT(49, 0x4000)) {
      dbgfil = stderr;
      if (gbl.dbgfil)
        dbgfil = gbl.dbgfil;
      fprintf(dbgfil, "INLSYM, %d=dtype\n", dtype);
      dumpdtype(dtype);
    }
#endif
    subscr.eldt = DTY(dtype + 1);
    if (subscr.eldt != DT_ASSCHAR && subscr.eldt != DT_ASSNCHAR &&
        subscr.eldt != DT_DEFERCHAR && subscr.eldt != DT_DEFERNCHAR) {
      ISZ_T val;
      int so;
      so = size_of(subscr.eldt);
      subscr.elmsz = sel_icnst(so, bigobj);
      subscr.scale = Scale_Of(subscr.eldt, &val);
      subscr.elmscz = sel_icnst(val, bigobj);
    }
    else if (subscr.eldt == DT_DEFERCHAR || subscr.eldt == DT_DEFERNCHAR) {
      /* deferred-size character; size is in sym */
      /* generate load of elem size */
      i = exp_get_sdsc_len(sym, base, basenm);
      if (subscr.eldt == DT_DEFERNCHAR) /* kanji-convert to byte units */
        i = ad2ili(IL_IMUL, i, ad_icon(2L));
      subscr.elmscz = subscr.elmsz = i;
      subscr.scale = 0;
    }
    else {
      /* assumed-size character; size is in sym */
      /* generate load of elem size */
      i = charlen(sym);
      if (subscr.eldt == DT_ASSNCHAR) /* kanji - convert to byte units */
        i = ad2ili(IL_IMUL, i, ad_icon(2L));
      subscr.elmscz = subscr.elmsz = i;
      subscr.scale = 0;
    }
    subscr.basenm = NME_OF(curilm);
    subscr.base = ILI_OF(curilm);
    subscr.nsubs = nsubs = AD_NUMDIM(adp);
    /* calculate offset */
    offset = sel_icnst(0, bigobj);
    if (!XBIT(52, 4) && sdsc) {
      if (!SDSCS1G(sdsc) && !XBIT(28, 0x20)) {
        subscr.zbase = sel_icnst(0, bigobj);
      } else {
        subscr.zbase = sel_icnst(0, bigobj);
        for (i = 0; i < nsubs; ++i) {
          int v;
          v = DESC_HDR_LEN + i * DESC_DIM_LEN + DESC_DIM_LOWER;
          sub = get_sdsc_element(sdsc, v, base, basenm);
          subscr.sub[i] = sub;
        }
      }
      offset = sel_icnst(0, bigobj);
#if DEBUG
      if (DBGBIT(49, 0x4000)) {
        fprintf(dbgfil, "INLSYM, %d=sdsc offset\n", offset);
        dilitre(offset);
      }
#endif
    } else if (CCSYMG(sym) && odtype) {
      ADSC *oadp; /* array descriptor */
      oadp = AD_DPTR(odtype);
      /* use the bounds from the original datatype */
      for (i = 0; i < nsubs; ++i) {
        sub = genload(AD_LWBD(oadp, i), bigobj); /* lwb is subscript */
        subscr.sub[i] = sub;
      }
      subscr.zbase = sel_icnst(0, bigobj);
      offset = sel_icnst(0, bigobj);
#if DEBUG
      if (DBGBIT(49, 0x4000)) {
        fprintf(dbgfil, "INLSYM, %d=ccsym zbase, %d=offset\n", subscr.zbase,
                offset);
        dilitre(subscr.zbase);
        dilitre(offset);
      }
#endif
    } else
    {
      subscr.zbase = genload(AD_ZBASE(adp), bigobj);
      for (i = 0; i < nsubs; ++i) {

        sub = genload(AD_LWBD(adp, i), bigobj); /* lwb is subscript */
        subscr.sub[i] = sub;
        mplyr = genload(AD_MLPYR(adp, i), bigobj); /* ili for multiplier */
        /* offset += sub * mplyr */
        offset = ad2ili(bigobj ? IL_KADD : IL_IADD, offset,
                        ad2ili(bigobj ? IL_KMUL : IL_IMUL, sub, mplyr));
      }
    }
    subscr.offset = offset;
    break;

  default:
    interr("inlarr:bad ilmopc", ILM_OPC(ilmp), 3);
  }

}

static int
finish_array(LOGICAL bigobj, LOGICAL inl_flg)
{
  int nme, i, sub, ili1, ili2, ili3, base;
  LOGICAL constant_zbase;
  int over_subscr;
  nme = subscr.basenm;
  over_subscr = 0;
  if (NME_TYPE(subscr.basenm) == NT_ARR) {
    /* over-subscripted; more subsripts than rank */
    over_subscr = 1;
  }
  NME_OVS(nme) = over_subscr;
  for (i = 0; i < subscr.nsubs; ++i) {
    sub = subscr.sub[i];
    if (IL_TYPE(ILI_OPC(sub)) == ILTY_CONS)
      nme =
          add_arrnme(NT_ARR, 0, nme, ad_val_of(ILI_OPND(sub, 1)), sub, inl_flg);
    else
      nme = add_arrnme(NT_ARR, -1, nme, (INT)0, sub, inl_flg);
    NME_OVS(nme) = over_subscr;
  }
  constant_zbase = FALSE;
  if (XBIT(70, 0x4000000) || (IL_TYPE(ILI_OPC(subscr.zbase)) == ILTY_CONS &&
                              IL_TYPE(ILI_OPC(subscr.elmscz)) == ILTY_CONS))
    constant_zbase = TRUE;
  if (constant_zbase) {
/* base = (array_base - (zbase - coffset) * size) <scaled by> scale */

    if (
        1 /* always big 64-bit */
        ) {
      ili1 = ikmove(subscr.zbase);
      ili2 = ikmove(subscr.elmscz);
      ili2 = ad2ili(IL_KMUL, ili1, ili2);
      ili2 = ad1ili(IL_KAMV, ili2);
    } else
    {
      ili2 = ad2ili(IL_IMUL, subscr.zbase, subscr.elmscz);
      ili2 = ad1ili(IL_IAMV, ili2);
    }
    base = ad3ili(IL_ASUB, subscr.base, ili2, subscr.scale);
  } else if (IL_TYPE(ILI_OPC(subscr.elmscz)) == ILTY_CONS) {
    if ((ILI_OPC(subscr.zbase) == IL_IADD) &&
        ILI_OPC(ili2 = ILI_OPND(subscr.zbase, 2)) == IL_ICON) {
      /*
       * zbase is of the form i + c, where c is a constant.
       * Restructure so that:
       *    zbase <- i
       *    base  <= base - c*elmsz
       * ....
       */
      /*
       */
      subscr.zbase = ILI_OPND(subscr.zbase, 1);
      ili2 = ad2ili(IL_IMUL, ili2, subscr.elmscz);
      ili2 = ad1ili(IL_IAMV, ili2);
      base = ad3ili(IL_ASUB, subscr.base, ili2, subscr.scale);
    } else if ((ILI_OPC(subscr.zbase) == IL_ISUB) &&
               ILI_OPC(ili2 = ILI_OPND(subscr.zbase, 2)) == IL_ICON) {
      /*
       * zbase is of the form i - c, where c is a constant.
       * Restructure so that:
       *    zbase <- i
       *    base  <= base + c*elmsz
       * ....
       */
      /*
       */
      subscr.zbase = ILI_OPND(subscr.zbase, 1);
      ili2 = ad2ili(IL_IMUL, ili2, subscr.elmscz);
      ili2 = ad1ili(IL_IAMV, ili2);
      base = ad3ili(IL_AADD, subscr.base, ili2, subscr.scale);
    } else if ((ILI_OPC(subscr.zbase) == IL_KADD) &&
               ILI_OPC(ili2 = ILI_OPND(subscr.zbase, 2)) == IL_KCON) {
      /*
       * zbase is of the form i + c, where c is a constant.
       * Restructure so that:
       *    zbase <- i
       *    base  <= base - c*elmsz
       * ....
       */
      /*
       */
      subscr.zbase = ILI_OPND(subscr.zbase, 1);
      ili2 = ad2ili(IL_KMUL, ili2, subscr.elmscz);
      ili2 = ad1ili(IL_KAMV, ili2);
      base = ad3ili(IL_ASUB, subscr.base, ili2, subscr.scale);
    } else if ((ILI_OPC(subscr.zbase) == IL_KSUB) &&
               ILI_OPC(ili2 = ILI_OPND(subscr.zbase, 2)) == IL_KCON) {
      /*
       * zbase is of the form i - c, where c is a constant.
       * Restructure so that:
       *    zbase <- i
       *    base  <= base + c*elmsz
       * ....
       */
      /*
       */
      subscr.zbase = ILI_OPND(subscr.zbase, 1);
      ili2 = ad2ili(IL_KMUL, ili2, subscr.elmscz);
      ili2 = ad1ili(IL_KAMV, ili2);
      base = ad3ili(IL_AADD, subscr.base, ili2, subscr.scale);
    } else {
      base = subscr.base;
    }
  } else {
    base = subscr.base;
  }

/*-
 * compute the final address of the reference.  Generate:
 *  (0) isub  offset  zbase		!constant_zbase
 *  (1) imul  offset  size(ili1)
 *  (2) damv  (1)
 *  (3) aadd  base    (2)      scale
 */
  if (IL_RES(ILI_OPC(subscr.offset)) == ILIA_KR || bigobj) {
    ili2 = ikmove(subscr.elmscz);
    if (constant_zbase) {
      ili1 = ikmove(subscr.offset);
    } else {
      ili1 = ad2ili(IL_KSUB, ikmove(subscr.offset), ikmove(subscr.zbase));
    }
    ili2 = ad2ili(IL_KMUL, ili1, ili2);
    ili2 = ad1ili(IL_KAMV, ili2);
  } else {
    if (constant_zbase) {
      ili1 = subscr.offset;
    } else {
      ili1 = kimove(subscr.zbase);
      ili1 = ad2ili(IL_ISUB, subscr.offset, ili1);
    }
    ili2 = ad2ili(IL_IMUL, ili1, subscr.elmscz);
    ili2 = ad1ili(IL_IAMV, ili2);
  }

  ili3 = ad3ili(IL_AADD, base, ili2, subscr.scale);
  subscr.finalnme = nme;
  return ili3;
} /* finish_array */

void
exp_array(ILM_OP opc, ILM *ilmp, int curilm)
{
  int ili1;
  int ili3;
  int nme;
  LOGICAL inl_flg, bigobj;
  int dtype;
  ADSC *adp;

#if DEBUG
  assert(opc == IM_ELEMENT || opc == IM_INLELEM, "exp_array: opc not ELEMENT",
         opc, 3);
#endif

  if (XBIT(125, 0x10000)) {
    int subs[7], i;
    int arrilm, dtype, sym;
    ILM *ilma;
    arrilm = ILM_OPND(ilmp, 2);
    dtype = ILM_OPND(ilmp, 3);
    ilma = (ILM *)(ilmb.ilm_base + arrilm);
    if (ILM_OPC(ilma) == IM_PLD) {
      /* rewritten arguments */
      sym = ILM_OPND(ilma, 2);
      if (ORIGDUMMYG(sym)) {
        sym = ORIGDUMMYG(sym);
      }
    } else {
#if DEBUG
      assert(ILM_OPC(ilma) == IM_BASE || ILM_OPC(ilma) == IM_ELEMENT,
             "exp_array: ASSCH/DEFERCH array not base", arrilm, 3);
#endif
      sym = ILM_OPND(ilma, 1); /* symbol pointer */
    }
    subscr.nsubs = ILM_OPND(ilmp, 1);
    for (i = 0; i < subscr.nsubs; ++i) {
      subs[i] = ILI_OF(ILM_OPND(ilmp, 4 + i)); /* subscript ili */
    }
    if (opc == IM_ELEMENT) {
      inl_flg = FALSE;
    } else {
      inl_flg = TRUE;
    }
    ILI_OF(curilm) = create_array_ref(NME_OF(arrilm), sym, dtype, subscr.nsubs,
                                      subs, ILI_OF(arrilm), 0, inl_flg, &nme);
    NME_OF(curilm) = nme;
    if (DTY(subscr.eldt) == TY_CHAR || DTY(subscr.eldt) == TY_NCHAR) {
      ILM_RESTYPE(curilm) = ILM_ISCHAR;
      if (DTY(subscr.eldt) == TY_NCHAR) /* kanji char type ... */
        /*  value represented by subscr.elmsz is twice too large: */
        ILM_CLEN(curilm) = ad2ili(IL_IDIV, subscr.elmsz, ad_icon(2L));
      else
        ILM_CLEN(curilm) = subscr.elmsz;

      if (DTY(subscr.eldt + 1) == 0)
        ILM_MXLEN(curilm) = 0;
      else
        ILM_MXLEN(curilm) = ILM_CLEN(curilm); /*subscr.elmsz;*/
    }
    return;
  }

  bigobj = XBIT(68, 0x1);
  /* ELEMENT nsubs array-lval dtype subs+ */
  /* INLEMEN nsubs array-lval dtype subs+ */

  if (opc == IM_ELEMENT) {
    dtype = ILM_OPND(ilmp, 3);
    adp = AD_DPTR(dtype);
    if (!XBIT(52, 4) && AD_SDSC(adp)) {
      /* Assumed shape and pointer arrays have not been previously
       * linearized in terms of their sdsc. Do that now if necessary.
       */
      compute_sdsc_subscr(ilmp);
    } else
      compute_subscr(ilmp, bigobj);
    inl_flg = FALSE;
  } else {
    inlarr(curilm, 0, bigobj);
    inl_flg = TRUE;
  }

  ili3 = finish_array(bigobj, inl_flg);
  nme = subscr.finalnme;
  if (DTY(subscr.eldt) == TY_CHAR || DTY(subscr.eldt) == TY_NCHAR) {
    ILM_RESTYPE(curilm) = ILM_ISCHAR;
    ili1 = kimove(subscr.elmsz);
    if (DTY(subscr.eldt) == TY_NCHAR) /* kanji char type ... */
      /*  value represented by subscr.elmsz is twice too large: */
      ILM_CLEN(curilm) = ad2ili(IL_IDIV, ili1, ad_icon(2L));
    else
      ILM_CLEN(curilm) = ili1;

    if (DTY(subscr.eldt + 1) == 0)
      ILM_MXLEN(curilm) = 0;
    else
      ILM_MXLEN(curilm) = ILM_CLEN(curilm); /*subscr.elmsz;*/
  }

  NME_OF(curilm) = nme;
  ILI_OF(curilm) = ili3;
}

/*
 * a routine to create an array reference given the array
 * and subscripts
 */
void
create_array_subscr(int nmex, int sym, int dtype, int nsubs, int *subs,
                    int ilix)
{
  ADSC *adp; /* array descriptor */
  int zbase; /* zbase sym/ili ptr */
  int i;
  ILM *ilmp1;
  int sub;
  int mplyr;
  int offset;
  int ili2;
  ISZ_T coffset;
  int any_kr;
  int sub_1;
  LOGICAL bigobj = FALSE;

  subscr.nsubs = nsubs;
#if DEBUG
  assert(subscr.nsubs <= (sizeof(subscr.sub) / sizeof(int)),
         "create_array_subscr:nsubs exceeded", subscr.nsubs, 3);
#endif
  if (XBIT(68, 0x1))
    bigobj = TRUE;
  adp = AD_DPTR(dtype);
  zbase = genload(AD_ZBASE(adp), bigobj); /* ili for zero-based offset */
  subscr.eldt = DTY(dtype + 1);           /* element data type */

  /*-
   * use scale_of to get the multiplier -- this is in two forms:
   * val    = number of units to scale
   * scale  = scaling factor of the subscript
   */

  if (subscr.eldt != DT_ASSCHAR && subscr.eldt != DT_ASSNCHAR &&
      subscr.eldt != DT_DEFERCHAR && subscr.eldt != DT_DEFERNCHAR) {
    INT val;
    subscr.elmsz = ad_icon(size_of(subscr.eldt));
    subscr.scale = scale_of(subscr.eldt, &val);
    subscr.elmscz = ad_icon(val);
  }
  else if (subscr.eldt == DT_DEFERCHAR || subscr.eldt == DT_DEFERNCHAR) {
    /* defered-size character; size is in symtab */
    int bytes;
#if DEBUG
    assert((STYPEG(sym) == ST_ARRAY ||
            (STYPEG(sym) == ST_MEMBER && subscr.eldt == DT_DEFERCHAR)),
           "create_array_subscr: DEFERCH sym not array", sym, 3);
#endif
    /* generate load of elem size */
    if (STYPEG(sym) == ST_MEMBER) {
      bytes = exp_get_sdsc_len(sym, ilix, NME_OF(nmex));
    } else
      bytes = exp_get_sdsc_len(sym, 0, 0);
    if (subscr.eldt == DT_DEFERNCHAR) /* assumed size kanji dummy */
      bytes = ad2ili(IL_IMUL, bytes, ad_icon(2L));
    subscr.elmscz = subscr.elmsz = bytes;
    subscr.scale = 0;
  }
  else {
    /* assumed-size character; size is in symtab */
    int bytes;
#if DEBUG
    assert(STYPEG(sym) == ST_ARRAY, "create_array_subscr: ASSCH sym not array",
           sym, 3);
#endif
    /* generate load of elem size */
    bytes = charlen(sym);
    if (subscr.eldt == DT_ASSNCHAR) /* assumed size kanji dummy */
      bytes = ad2ili(IL_IMUL, bytes, ad_icon(2L));
    subscr.elmscz = subscr.elmsz = bytes;
    subscr.scale = 0;
  }

  /* calculate offset */
  coffset = 0;
  subscr.basenm = nmex;
  any_kr = 0;
  if (XBIT(125, 0x20000))
    any_kr = 1;
  for (i = 0; i < subscr.nsubs; ++i) {
    sub = subs[i]; /* subscript ili */
    subscr.sub[i] = sub;
    if (!bigobj && IL_RES(ILI_OPC(sub)) == ILIA_KR)
      any_kr = 1;
  }
  if (any_kr) {
    for (i = 0; i < subscr.nsubs; ++i) {
      subscr.sub[i] = ikmove(subscr.sub[i]);
    }
  }
  offset = sel_icnst(0, any_kr);

  /*
   * if the first/left-most subscript is a constant, the initial value
   * of the constant offset is the subscript's value and the first
   * subscript must be set to 0.
   */
  sub_1 = subscr.sub[0];
  if (!XBIT(125, 0x4000) && IL_TYPE(ILI_OPC(sub_1)) == ILTY_CONS) {
    coffset = get_isz_cval(ILI_OPND(sub_1, 1));
    subscr.sub[0] = offset; /* the zero */
  }

  for (i = 0; i < subscr.nsubs; ++i) {
    sub = subscr.sub[i]; /* subscript ili */
    if (!bigobj) {
      mplyr = genload(AD_MLPYR(adp, i), FALSE); /* ili for multiplier */
      /* offset += sub * mplyr */
      if (ILI_OPC(mplyr) == IL_ICON) {
        if ((ILI_OPC(sub) == IL_IADD) &&
            ILI_OPC(ili2 = ILI_OPND(sub, 2)) == IL_ICON) {
          /*
           * subcript is of the form i + c, where c is a constant. the
           * value c*mlpyr is accumulated and i becomes sub.
           */
          coffset += CONVAL2G(ILI_OPND(ili2, 1)) * CONVAL2G(ILI_OPND(mplyr, 1));
          sub = ILI_OPND(sub, 1);
        } else if ((ILI_OPC(sub) == IL_ISUB) &&
                   ILI_OPC(ili2 = ILI_OPND(sub, 2)) == IL_ICON) {
          /*
           * subscript is of the form i - c, where c is a constant. the
           * value c*mlpyr is accumulated and i becomes sub.
           */
          coffset -= CONVAL2G(ILI_OPND(ili2, 1)) * CONVAL2G(ILI_OPND(mplyr, 1));
          sub = ILI_OPND(sub, 1);
        } else if ((ILI_OPC(sub) == IL_KADD) &&
                 ILI_OPC(ili2 = ILI_OPND(sub, 2)) == IL_KCON) {
          /*
           * subcript is of the form i + c, where c is a constant. the
           * value c*mlpyr is accumulated and i becomes sub.
           */
          coffset +=
              get_isz_cval(ILI_OPND(ili2, 1)) * CONVAL2G(ILI_OPND(mplyr, 1));
          sub = ILI_OPND(sub, 1);
        } else if ((ILI_OPC(sub) == IL_KSUB) &&
                   ILI_OPC(ili2 = ILI_OPND(sub, 2)) == IL_KCON) {
          /*
           * subscript is of the form i - c, where c is a constant. the
           * value c*mlpyr is accumulated and i becomes sub.
           */
          coffset -=
              get_isz_cval(ILI_OPND(ili2, 1)) * CONVAL2G(ILI_OPND(mplyr, 1));
          sub = ILI_OPND(sub, 1);
        }
      }
      if (!any_kr) {
        ili2 = ad2ili(IL_IMUL, sub, mplyr);
        offset = ad2ili(IL_IADD, offset, ili2);
      } else {
        ili2 = ad1ili(IL_IKMV, mplyr);
        ili2 = ad2ili(IL_KMUL, sub, ili2);
        if (IL_TYPE(ILI_OPC(ili2)) == ILTY_CONS &&
            IL_TYPE(ILI_OPC(sub)) != ILTY_CONS) {
          subscr.sub[i] = ili2;
        }
        offset = ad2ili(IL_KADD, offset, ili2);
      }
    } else {
      mplyr = genload(AD_MLPYR(adp, i), TRUE); /* ili for multiplier */
      if (ILI_OPC(mplyr) == IL_KCON) {
        if ((ILI_OPC(sub) == IL_KADD) &&
            ILI_OPC(ili2 = ILI_OPND(sub, 2)) == IL_KCON) {
          /*
           * subcript is of the form i + c, where c is a constant. the
           * value c*mlpyr is accumulated and i becomes sub.
           */
          coffset +=
              ad_val_of(ILI_OPND(ili2, 1)) * ad_val_of(ILI_OPND(mplyr, 1));
          sub = ILI_OPND(sub, 1);
        } else if ((ILI_OPC(sub) == IL_KSUB) &&
                   ILI_OPC(ili2 = ILI_OPND(sub, 2)) == IL_KCON) {
          /*
           * subscript is of the form i - c, where c is a constant. the
           * value c*mlpyr is accumulated and i becomes sub.
           */
          coffset -=
              ad_val_of(ILI_OPND(ili2, 1)) * ad_val_of(ILI_OPND(mplyr, 1));
          sub = ILI_OPND(sub, 1);
        }
      }
      ili2 = ad2ili(IL_KMUL, sub, mplyr);
      if (IL_TYPE(ILI_OPC(ili2)) == ILTY_CONS &&
          IL_TYPE(ILI_OPC(sub)) != ILTY_CONS) {
        subscr.sub[i] = ili2;
      }
      offset = ad2ili(IL_KADD, offset, ili2);
    }
  }
  /*
   * Eventually, offset will multiplied by the element size.  Check the
   * offset for the pattern 'i + c' or 'i - c', if the size is a constant.
   * The constant part can be folded into coffset; note that this is
   * without the multiply since the caller of compute_subscr() will perform
   * the multiply by the element size.
   */
  mplyr = subscr.elmscz; /* ili for multiplier */
  if (IL_TYPE(ILI_OPC(mplyr)) == ILTY_CONS) {
    if ((ILI_OPC(offset) == IL_IADD) &&
        ILI_OPC(ili2 = ILI_OPND(offset, 2)) == IL_ICON) {
      /*
       * offset is of the form i + c, where c is a constant. the
       * value c is accumulated and i becomes offset.
       */
      coffset += CONVAL2G(ILI_OPND(ili2, 1));
      offset = ILI_OPND(offset, 1);
    } else if ((ILI_OPC(offset) == IL_ISUB) &&
               ILI_OPC(ili2 = ILI_OPND(offset, 2)) == IL_ICON) {
      /*
       * offset is of the form i - c, where c is a constant. the
       * value c is accumulated and i becomes offset.
       */
      coffset -= CONVAL2G(ILI_OPND(ili2, 1));
      offset = ILI_OPND(offset, 1);
    } else if ((ILI_OPC(offset) == IL_KADD) &&
             ILI_OPC(ili2 = ILI_OPND(offset, 2)) == IL_KCON) {
      /*
       * offset is of the form i + c, where c is a constant. the
       * value c is accumulated and i becomes offset.
       */
      coffset += get_isz_cval(ILI_OPND(ili2, 1));
      offset = ILI_OPND(offset, 1);
    } else if ((ILI_OPC(offset) == IL_KSUB) &&
               ILI_OPC(ili2 = ILI_OPND(offset, 2)) == IL_KCON) {
      /*
       * offset is of the form i - c, where c is a constant. the
       * value c is accumulated and i becomes offset.
       */
      coffset -= get_isz_cval(ILI_OPND(ili2, 1));
      offset = ILI_OPND(offset, 1);
    }
  }

  /* base = (array_base - (zbase - coffset) * size) <scaled by> scale */

  if (coffset) {
    if (!bigobj)
      zbase = ad2ili(IL_ISUB, zbase, ad_icon(coffset));
    else
      zbase = ad2ili(IL_KSUB, zbase, ad_kconi(coffset));
  }
  subscr.zbase = zbase;
  subscr.offset = offset;
  subscr.base = ilix;
  subscr.sub[0] = sub_1;

} /* create_array_subscr */

int
create_array_ref(int nmex, int sptr, DTYPE dtype, int nsubs, int *subs,
                 int ilix, int sdscilix, int inline_flag, int *pnme)
{
  int base;
  int ili1;
  int ili2;
  int ili3;
  int nme;
  int i;
  int sub;
  LOGICAL bigobj = FALSE, usek = FALSE;
  ADSC *adp;
  LOGICAL constant_zbase;

  adp = AD_DPTR(dtype);
  if (!XBIT(52, 4) && AD_SDSC(adp)) {
    /* Assumed shape and pointer arrays have not been previously
     * linearized in terms of their sdsc. Do that now if necessary.
     */
    create_sdsc_subscr(nmex, sptr, nsubs, subs, dtype, ilix, sdscilix);
  } else
  {
    create_array_subscr(nmex, sptr, dtype, nsubs, subs, ilix);
  }

  nme = nmex;
  for (i = 0; i < subscr.nsubs; ++i) {
    sub = subscr.sub[i];
    if (IL_TYPE(ILI_OPC(sub)) == ILTY_CONS) {
      nme = add_arrnme(NT_ARR, 0, nme, ad_val_of(ILI_OPND(sub, 1)), sub,
                       inline_flag);
    } else {
      nme = add_arrnme(NT_ARR, -1, nme, (INT)0, sub, inline_flag);
    }
  }

  if (XBIT(68, 0x1))
    bigobj = TRUE;
  usek = TRUE;
  constant_zbase = FALSE;
  if (!bigobj &&
      (XBIT(70, 0x4000000) || (IL_TYPE(ILI_OPC(subscr.zbase)) == ILTY_CONS &&
                               IL_TYPE(ILI_OPC(subscr.elmscz)) == ILTY_CONS)))
    constant_zbase = TRUE;
  if (!bigobj && !constant_zbase) {
    base = subscr.base;
  } else {
/* base = (array_base - (zbase - coffset) * size) <scaled by> scale */
    if (bigobj || usek || IL_RES(ILI_OPC(subscr.zbase)) == ILIA_KR) {
      ili1 = ikmove(subscr.zbase);
      ili2 = ikmove(subscr.elmscz);
      ili2 = ad2ili(IL_KMUL, ili1, ili2);
      ili2 = ad1ili(IL_KAMV, ili2);
    } else {
      ili2 = ad2ili(IL_IMUL, subscr.zbase, subscr.elmscz);
      ili2 = ad1ili(IL_IAMV, ili2);
    }
    base = ad3ili(IL_ASUB, subscr.base, ili2, subscr.scale);
  }

/*-
 * compute the final address of the reference.  Generate:
 *  (0) isub  offset  zbase		!constant_zbase && !bigobj
 *  (1) imul  offset  size(ili1)
 *  (2) damv  (1)
 *  (3) aadd  base    (2)      scale
 */
  if (bigobj) {
    ili2 = ad2ili(IL_KMUL, subscr.offset, ikmove(subscr.elmscz));
    ili2 = ad1ili(IL_KAMV, ili2);
  } else if (IL_RES(ILI_OPC(subscr.offset)) == ILIA_KR) {
    ili1 = subscr.offset;
    if (!constant_zbase) {
      ili1 = ad2ili(IL_KSUB, ili1, ikmove(subscr.zbase));
    }
    ili2 = ad2ili(IL_KMUL, ili1, ikmove(subscr.elmscz));
    ili2 = ad1ili(IL_KAMV, ili2);
  } else
  {
    ili1 = subscr.offset;
    if (!constant_zbase) {
      ili1 = ad2ili(IL_ISUB, ili1, kimove(subscr.zbase));
    }
    ili2 = ad2ili(IL_IMUL, ili1, subscr.elmscz);
    ili2 = ad1ili(IL_IAMV, ili2);
  }

  ili1 = ad3ili(IL_AADD, base, ili2, subscr.scale);

  if (pnme)
    *pnme = nme;
  return ili1;
} /* create_array_ref */

/***************************************************************/

static LOGICAL
simple_ili(int ilix)
{
  int opc;

  opc = ILI_OPC(ilix);
  if (IL_TYPE(opc) == ILTY_CONS)
    return TRUE;
  if (IL_TYPE(opc) == ILTY_LOAD && !func_in(ilix))
    return TRUE;
  return FALSE;
}

void
exp_bran(ILM_OP opc, ILM *ilmp, int curilm)
{
  static struct {
    short jmpop;  /* aif jump op */
    short cseop;  /* aif cse op */
    short dtype;  /* data type */
    short stop;   /* store op */
    short ldop;   /* load op */
    short cmpop;  /* compare with 0 op */
    short subop;  /* subtract op */
    short cjmpop; /* compare and jump op */
    short msz;    /* msz for load/store */
  } aif[4] = {
    {IL_ICJMPZ, IL_CSEIR, DT_INT, IL_ST, IL_LD, IL_ICMPZ, IL_ISUB, IL_ICJMP,
     MSZ_WORD},
    {IL_FCJMPZ, IL_CSESP, DT_REAL, IL_STSP, IL_LDSP, IL_FCMPZ, IL_FSUB,
     IL_FCJMP, MSZ_F4},
    {IL_DCJMPZ, IL_CSEDP, DT_DBLE, IL_STDP, IL_LDDP, IL_DCMPZ, IL_DSUB,
     IL_DCJMP, MSZ_F8},
    {IL_KCJMPZ, IL_CSEKR, DT_INT8, IL_STKR, IL_LDKR, IL_KCMPZ, IL_KSUB,
     IL_KCJMP, MSZ_I8},
  };
  int i;    /* temp */
  int ilix; /* ILI index */
  int sym1, sym2, sym3, type;
  int sym, save, ililnk, nme;
  int op1;
  ILM *ilmpx;

#define BR_TRUE(t, i, c, s) \
  ad3ili(expb.logcjmp, ad2ili(aif[t].cmpop, i, c), CC_NE, s)

#define CHK_113(i)

  switch (opc) {
  case IM_CGOTO: /* computed goto */
    exp_cgoto(ilmp, curilm);
    break;

  case IM_AGOTO: /* assigned goto */
    exp_agoto(ilmp, curilm);
    break;

  case IM_KAIF: /* integer*8 arithmetic IF */
    type = 3;
    goto comaif;
  case IM_IAIF: /* integer arithmetic IF */
    type = 0;
    goto comaif;
  case IM_RAIF: /* real arithmetic IF */
    type = 1;
    goto comaif;
  case IM_DAIF: /* double arithmetic IF */
    type = 2;
  comaif:
    /* arithmetic if processing */
    ilix = ILM_RESULT(ILM_OPND(ilmp, 1));
    sym1 = ILM_OPND(ilmp, 2);
    CHK_113(sym1);
    sym2 = ILM_OPND(ilmp, 3);
    CHK_113(sym2);
    sym3 = ILM_OPND(ilmp, 4);
    CHK_113(sym3);
    if (sym1 == sym2) {
      RFCNTD(sym1);
      if (sym1 == sym3) { /* all are equal */
        RFCNTD(sym1);
        ilix = ad1ili(IL_JMP, sym1);
      } else {
        /* if <= goto sym1 */
        ilix = BR_TRUE(type, ilix, CC_LE, sym1);
        if (ilix)
          chk_block(ilix);
        ilix = ad1ili(IL_JMP, sym3);
      }
    } else if (sym1 == sym3) {
      /* if != goto sym1 */
      RFCNTD(sym1);
      ilix = BR_TRUE(type, ilix, CC_NE, sym1);
      if (ilix)
        chk_block(ilix);
      ilix = ad1ili(IL_JMP, sym2);
    } else if (sym2 == sym3) {
      /* if >= goto sym2 */
      RFCNTD(sym2);
      ilix = BR_TRUE(type, ilix, CC_GE, sym2);
      if (ilix)
        chk_block(ilix);
      ilix = ad1ili(IL_JMP, sym1);
    } else { /* all are different */
      if (flg.opt == 1) {
        /* Just add multiple branches of the if expression which is
         * asserted to be a common subexpression via the CSE ili.
         */
        save = ilix;
        /* if < goto sym1 */
        ilix = ad3ili(aif[type].jmpop, ilix, CC_LT, sym1);
        if (ilix)
          chk_block(ilix);
        ilix = ad1ili(aif[type].cseop, save);
        /* if = goto sym2 */
        ilix = ad3ili(aif[type].jmpop, ilix, CC_EQ, sym2);
        if (ilix)
          chk_block(ilix);
        ilix = ad1ili(IL_JMP, sym3);
      } else {
        /* For the I386, always create multiple blocks; asserted cse
         * opportunities are problematic across branches because of the
         * floating point stack. The code generator may not pop the
         * stack for the first conditional.
         * For other targets, make multiple blocks for opt 0 & opt >= 2.
         * in general, generate:
         *    tmp = expr;
         *    if (tmp  < 0) goto lab1;
         *    if (tmp == 0) goto lab2;
         *    goto lab3;
         */
        int load;
        int op1, op2;

        /* special case:
         *    if (x - y) l1, l2, l3  becomes
         *    if (x .lt. y) goto l1
         *    if (x .eq. y) goto l2
         *    goto l3
         * where
         *    x & y are "simple"
         */
        if (ILI_OPC(ilix) == aif[type].subop &&
            simple_ili(op1 = ILI_OPND(ilix, 1)) &&
            simple_ili(op2 = ILI_OPND(ilix, 2))) {
          /* if x < y goto sym1 */
          ilix = ad4ili(aif[type].cjmpop, op1, op2, CC_LT, sym1);
          if (ilix)
            chk_block(ilix);
          /* if x = y goto sym2 */
          ilix = ad4ili(aif[type].cjmpop, op1, op2, CC_EQ, sym2);
          if (ilix)
            chk_block(ilix);
          ilix = ad1ili(IL_JMP, sym3);
          if (ilix)
            chk_block(ilix);
          break;
        }
        if (simple_ili(ilix))
          /* don't need to temp store if arith if expr is simple */
          load = ilix;
        else {
          sym = mkrtemp_sc(ilix, expb.sc);
          nme = addnme(NT_VAR, sym, 0, (INT)0);
          DTYPEP(sym, aif[type].dtype);
          ililnk = ad_acon(sym, (INT)0);
          ilix = ad4ili(aif[type].stop, ilix, ililnk, nme, aif[type].msz);
          load = ad3ili(aif[type].ldop, ililnk, nme, aif[type].msz);
          chk_block(ilix);
        }
        /* if < goto sym1 */
        ilix = ad3ili(aif[type].jmpop, load, CC_LT, sym1);
        if (ilix)
          chk_block(ilix);
        /* if = goto sym2 */
        ilix = ad3ili(aif[type].jmpop, load, CC_EQ, sym2);
        if (ilix)
          chk_block(ilix);
        ilix = ad1ili(IL_JMP, sym3);
      }
    }
    if (ilix)
      chk_block(ilix);
    break;

  case IM_BRF:
    /*
     * .OP ICJMPZ null p1 eq v2
     * .OP LCJMPZ null p1 eq v2
     */
    sym1 = CC_EQ;
    goto logcjmp_;

  case IM_BRT:
    /*
     * .OP ICJMPZ null p1 ne v2
     * .OP LCJMPZ null p1 ne v2
     */
    sym1 = CC_NE;
  logcjmp_:
    sym = ILM_OPND(ilmp, 2);
    if (CCSYMG(sym) == 0) {
      /* refd but not defd */
      CHK_113(sym);
    }
    op1 = ILM_OPND(ilmp, 1);
    ilix = ILM_RESULT(op1);
    ilmpx = (ILM *)(ilmb.ilm_base + op1);
    switch (ILM_OPC(ilmpx)) {
    case IM_EQ8:
    case IM_NE8:
    case IM_LT8:
    case IM_GE8:
    case IM_LE8:
    case IM_GT8:
      if (ILI_OPC(ilix) == IL_IKMV)
        ilix = ILI_OPND(ilix, 1);
      break;
    default:
      break;
    }
    if (IL_RES(ILI_OPC(ilix)) == ILIA_KR) {
      if (XBIT(125, 0x8)) {
        /* -Munixlogical */
        ilix = ad2ili(IL_KCMPZ, ilix, sym1);
        sym1 = CC_NE;
      }
    }
    if ((ilix = ad3ili(expb.logcjmp, ilix, sym1, sym)) != 0)
      chk_block(ilix);
    break;

  default:                              /* this code is same as for C */
    i = ILM_OPND(ilmp, ilms[opc].oprs); /* get label */
    if (CCSYMG(i) == 0) {
      /* refd but not defd */
      CHK_113(i);
    }
    if ((ilix = exp_mac(opc, ilmp, curilm)) != 0)
      chk_block(ilix);
    break;
  }
}

/***************************************************************/

void
exp_misc(ILM_OP opc, ILM *ilmp, int curilm)
{
  int tmp;
  int ilix, listilix;
  int nme;
  int lpcnt;
  int sym;
  char lbl[32];
  int s, i;
  int pragmatype, pragmascope, pragmanargs, pragmaarg, pragmasym, devarg,
      argili;
  int parentnmex, parentilix;
  static int hostsptr = 0, devsptr = 0;
  static int blocknest, gridnest, kernelnest;
  int ilmx;
  ILM *ilmpx;

  switch (opc) {
  case IM_NOP: /* skip to next ILM  */
    break;

  case IM_BOS:
    expb.ilm_words += expb.nilms - BOS_SIZE;
    expb.curlin = ILM_OPND(ilmp, 1);
    if (expb.curlin) {
      gbl.lineno = expb.curlin;

      /* per flyspray 15632, we want to get the line number correctly
         for higher optimization. Blocks are merged into ENLAB
         block until there is a branch.  We want to get a line
         number of the next block if the current block does not
         have ilt.
       */
      if (expb.curilt == 0 && BIH_ENLAB(expb.curbih))
        BIH_LINENO(expb.curbih) = expb.curlin;
    }

    expb.arglcnt.next = expb.arglcnt.start;

    if (expb.flags.bits.noblock) {
      /*
       * no bih exists - create one with no ilts and set its line
       * number field
       */
      cr_block();
      /*
       * if no entry header has been written yet, then this BOS
       * is the first one for the subprogram.  create the entry
       * header for this subprogram; passing 0 to begin_entry/exp_header
       * indicates that the entry symbol (an unnamed program,
       * program, subroutine, or function) is retrieved from
       * gbl.currsub
       */
      if (expb.flags.bits.noheader) {
        begin_entry(0);
        expb.flags.bits.noheader = 0;
      }
    } else if (flg.opt == 0) {

      /*
       * since the opt level is zero, the current block is written out
       * provided that the current block is not empty, or already has a
       * line number or label.  A new one is created with no ilts with
       * its line number field set.
       */
      if (expb.curilt != 0 || BIH_LINENO(expb.curbih) != 0 ||
          BIH_LABEL(expb.curbih) != 0) {
        wr_block();
        cr_block();
      } else {
        BIH_LINENO(expb.curbih) = expb.curlin;
        expb.curlin = 0;
      }
    } else if (expb.ilm_words > expb.ilm_thresh) {
      /* prevent merge of this block at opt >= 2 */
      BIH_NOMERGE(expb.curbih) = 1;
      if (expb.curilt || expb.flags.bits.waitlbl) {
        flsh_block();
        if (expb.flags.bits.noblock)
          cr_block();
      }
    }
    mkrtemp_init();
    hostsptr = 0;
    devsptr = 0;
    break;

  case IM_ENTRY:
    /* process an entry defined by the ENTRY statement */
    begin_entry((int)ILM_OPND(ilmp, 1));
    break;

  case IM_ENLAB:
#if !defined(TARGET_OSX)
    sprintf(lbl, "..EN%d_%d", gbl.func_count, entry_sptr);
#else
    sprintf(lbl, "L.EN%d_%d", gbl.func_count, entry_sptr);
#endif
    s = getsym(lbl, strlen(lbl));
    STYPEP(s, ST_LABEL);
    RFCNTP(s, 1);
    exp_label(s);
    BIH_ENLAB(expb.curbih) = 1;
    CCSYMP(s, 1);

    break;

  case IM_LABEL:
    exp_label((int)ILM_OPND(ilmp, 1));
    break;

  case IM_ESTMT:
    exp_estmt((int)ILI_OF(ILM_OPND(ilmp, 1)));
    break;

  case IM_ARET:
    tmp = ILM_RESULT(ILM_OPND(ilmp, 1));
    store_aret(tmp);
    goto ret_shared;

  case IM_RET:
    if (gbl.arets) {
      tmp = ad_icon((INT)0);
      store_aret(tmp);
    }
  /*
   * generate a jump to the return label which is common to the
   * function
   */
  ret_shared:
    if (expb.retlbl == 0) {
      /*
       * this is the first return ILM seen for this function:
       */
      expb.retlbl = getccsym('R', expb.retcnt++, ST_LABEL);
    }
    RFCNTI(expb.retlbl);
    chk_block(ad1ili(IL_JMP, expb.retlbl));
    break;

  case IM_ENDF:
    exp_end(ilmp, curilm, TRUE);
    break;

  case IM_END:
    exp_end(ilmp, curilm, FALSE);
    break;

  case IM_BYVAL:
    ilmx = ILM_OPND(ilmp, 1); /* operand being passed */
    ilmpx = (ILM *)(ilmb.ilm_base + ilmx);
    if (ILM_OPC(ilmpx) == IM_DPVAL) {
      ilmx = ILM_OPND(ilmpx, 1); /* operand of the %val() */
      ILM_OPND(ilmp, 1) = ilmx;
    }
    /* now defer it */
    break;
  case IM_DPSCON:
  case IM_DPVAL:
  case IM_DPREF:
  case IM_DPREF8:
  case IM_DPNULL:
    /* defer these */
    break;
#ifdef IM_DOBEGNZ
  case IM_DOBEGNZ:
    lpcnt = ILM_RESULT(ILM_OPND(ilmp, 1)); /* fetch loop count */
    if (expb.isguarded <= 0) {
      lpcnt = ILM_RESULT(ILM_OPND(ilmp, 4));
      tmp = ad3ili(IL_ICJMPZ, lpcnt, CC_NE, (int)ILM_OPND(ilmp, 2));
      if (tmp) {
        chk_block(tmp);
        expb.isguarded++;
      }
    } else {
      expb.isguarded++;
    }
    if (expb.isguarded == 1) {
      BIH_GUARDER(expb.curbih) = 1;
    } else if (expb.isguarded) {
      BIH_GUARDEE(expb.curbih) = 1;
      sym = ILM_OPND(ilmp, 2);
      RFCNTD(sym);
    }
    expb.curlin = gbl.lineno; /* ensure next ilm (LABEL) gets line #*/
    break;
#endif

  case IM_DOBEG:
    lpcnt = ILM_RESULT(ILM_OPND(ilmp, 1)); /* fetch loop count */
                                           /*
                                            * For zero-trip loops, test the loop count and generate a
                                            * branch to the zero-trip label it's less than or equal to
                                            * zero.  "Check" the block, but watch out for branches that
                                            * are no-op'd.  Note that we don't emit a cse of the loop
                                            * count; a load is better suited for tracking the store's uses.
                                            */
    if (!flg.onetrip) {
      sym = ILM_OPND(ilmp, 3); /* address of count var */
      if (IL_TYPE(ILI_OPC(lpcnt)) != ILTY_CONS) {
        ilix = mk_address(sym);
        nme = addnme(NT_VAR, sym, 0, (INT)0);
        if (DTYPEG(sym) == DT_INT8)
          lpcnt = ad3ili(IL_LDKR, ilix, nme, MSZ_I8);
        else
        {
          lpcnt = ad3ili(IL_LD, ilix, nme, MSZ_WORD);
        }
        ADDRCAND(lpcnt, nme);
      }
      if (DTYPEG(sym) == DT_INT8)
        tmp = ad3ili(IL_KCJMPZ, lpcnt, CC_LE, (int)ILM_OPND(ilmp, 2));
      else
      {
        tmp = ad3ili(IL_ICJMPZ, lpcnt, CC_LE, (int)ILM_OPND(ilmp, 2));
      }
      if (tmp)
        chk_block(tmp);
    }
    expb.curlin = gbl.lineno; /* ensure next ilm (LABEL) gets line #*/
    break;

#ifdef IM_DOENDNZ
  case IM_DOENDNZ:
    if (expb.isguarded)
      expb.isguarded--;
    if (BIH_LABEL(expb.curbih) && RFCNTG(BIH_LABEL(expb.curbih)) == 0) {
      ILIBLKP(BIH_LABEL(expb.curbih), 0);
      BIH_LABEL(expb.curbih) = 0;
    }

#endif
  case IM_DOEND:
    sym = ILM_OPND(ilmp, 2); /* for address of count variable */
    ilix = mk_address(sym);
    nme = addnme(NT_VAR, sym, 0, (INT)0);
/*
 * generate the decrement of the loop count variable
 */
    if (DTYPEG(sym) == DT_INT8) {
      lpcnt = ad3ili(IL_LDKR, ilix, nme, MSZ_I8);
      ADDRCAND(lpcnt, nme);
      lpcnt = ad2ili(IL_KSUB, lpcnt, ad1ili(IL_KCON, stb.k1));
      tmp = ad4ili(IL_STKR, lpcnt, ilix, nme, MSZ_I8);
      ADDRCAND(tmp, nme);
      chk_block(tmp);
      /*
       * generate compare and branch ILI against zero which branches
       * to the top of the loop if still greater than zero.
       * Also, if at opt 2 and the loop is a zero-trip loop, set
       * the zero-trip flag of the block (BIH) defined by the loop
       * top label.
       */
      /* assertion: should be safe with respect to optimizations to use
       * a load of the loop count variable instead of a cse of the rhs
       * of the store; if not, change ilix to ad_cse(lpcnt).
       */
      tmp = ad3ili(IL_KCJMPZ, ad3ili(IL_LDKR, ilix, nme, MSZ_I8), CC_GT,
                   (int)ILM_OPND(ilmp, 1));
    } else
    {
      lpcnt = ad3ili(IL_LD, ilix, nme, MSZ_WORD);
      ADDRCAND(lpcnt, nme);
      lpcnt = ad2ili(IL_ISUB, lpcnt, ad1ili(IL_ICON, stb.i1));
      tmp = ad4ili(IL_ST, lpcnt, ilix, nme, MSZ_WORD);
      ADDRCAND(tmp, nme);
      chk_block(tmp);
      /*
       * generate compare and branch ILI against zero which branches
       * to the top of the loop if still greater than zero.
       * Also, if at opt 2 and the loop is a zero-trip loop, set
       * the zero-trip flag of the block (BIH) defined by the loop
       * top label.
       */
      /* assertion: should be safe with respect to optimizations to use
       * a load of the loop count variable instead of a cse of the rhs
       * of the store; if not, change ilix to ad_cse(lpcnt).
       */
      tmp = ad3ili(IL_ICJMPZ, ad3ili(IL_LD, ilix, nme, MSZ_WORD), CC_GT,
                   (int)ILM_OPND(ilmp, 1));
    }
    chk_block(tmp);
    if (!flg.onetrip && flg.opt >= 2 && opc != IM_DOENDNZ)
      BIH_ZTRP(ILIBLKG(ILM_OPND(ilmp, 1))) = 1;
    break;

  case IM_ADJARR:
    sym = ILM_OPND(ilmp, 1);
#if DEBUG
    assert(STYPEG(sym) == ST_ENTRY, "exp_misc: not ST_ENTRY in ilm", curilm, 3);
#endif
    if (AFTENTG(sym)) {
      tmp = ad1ili(IL_JMP, (int)ILM_OPND(ilmp, 2));
      chk_block(tmp);
      if (sym == gbl.currsub)
        /* for ENTRYs, "branch around" label is used as "return" */
        exp_label((int)ILM_OPND(ilmp, 3));
    }
    break;

  case IM_VFENTER:
    exp_label((int)ILM_OPND(ilmp, 1));  /* label vf "function" */
    ilix = ad1ili(IL_VFENTER, vf_addr); /* enter "function" */
    chk_block(ilix);
    break;
  case IM_VFRET:
    ilix = ILI_OF(ILM_OPND(ilmp, 1));        /* return value */
    ilix = ad2ili(IL_VFEXIT, vf_addr, ilix); /* leave "function" */
    chk_block(ilix);
    break;

  case IM_CMSIZE:
    sym = ILM_OPND(ilmp, 1); /* common block symbol */
#if DEBUG
    assert(STYPEG(sym) == ST_CMBLK, "exp_misc: CMSIZE not cmblk", sym, 3);
#endif
    ilix = ad_kconi(SIZEG(sym));
    ILM_RESULT(curilm) = ilix;
    break;

#ifdef IM_PARG
  case IM_PARG:
    /* defer to exp_rte */
    break;
#endif

  case IM_PREFETCH:
    ilix = ILI_OF(ILM_OPND(ilmp, 1)); /* address */
    nme = NME_OF(ILM_OPND(ilmp, 1));
    if (XBIT(39, 0x4000) && TEST_MACH(MACH_AMD_HAMMER)) {
      ilix = ad3ili(IL_PREFETCHT0, ilix, 0, nme);
    } else if (TEST_MACH(MACH_AMD_HAMMER)) {
      ilix = ad3ili(IL_PREFETCHNTA, ilix, 0, NME_UNK);
    } else if (TEST_MACH(MACH_AMD)) {
      ilix = ad3ili(IL_PREFETCH, ilix, 0, nme); /* Athlon */
    } else {
      ilix = ad3ili(IL_PREFETCHNTA, ilix, 0, nme); /* PIII+ sse */
    }
    chk_block(ilix);
    break;
  case IM_FARG:
    ILM_CLEN(curilm) = ILM_CLEN(ILM_OPND(ilmp, 1));
    ILM_RESULT(curilm) = ILM_RESULT(ILM_OPND(ilmp, 1));
    ILM_RESTYPE(curilm) = ILM_RESTYPE(ILM_OPND(ilmp, 1));
    break;
  case IM_FARGF:
    ILM_CLEN(curilm) = ILM_CLEN(ILM_OPND(ilmp, 1));
    ILM_RESULT(curilm) = ILM_RESULT(ILM_OPND(ilmp, 1));
    ILM_RESTYPE(curilm) = ILM_RESTYPE(ILM_OPND(ilmp, 1));
    break;
  case IM_BBND:
    break;
#if defined(IM_FILE)
  case IM_FILE:
    /* PGF90 only */
    if (!XBIT(6, 0x40000) && fihb.currfindex != ILM_OPND(ilmp, 2)) {
      /* start a new block */
      wr_block();
      cr_block();
    }
    if (fihb.nextfindex != ILM_OPND(ilmp, 2) ||
        fihb.nextftag < ILM_OPND(ilmp, 3)) {
      fihb.nextfindex = ILM_OPND(ilmp, 2);
      fihb.nextftag = ILM_OPND(ilmp, 3);
      if (ILT_NEXT(0) == 0) {
        /* no ILTs yet */
        fihb.currftag = fihb.nextftag;
        fihb.currfindex = fihb.nextfindex;
        gbl.findex = fihb.nextfindex;
      }
    }
    break;
#endif
  case IM_PRAGMASYM:
  case IM_PRAGMASLIST:
    pragmanargs = ILM_OPND(ilmp, 1);
    pragmatype = ILM_OPND(ilmp, 2);
    pragmascope = ILM_OPND(ilmp, 3);
    switch (pragmatype) {
    }
    break;
  case IM_PRAGMASYMEXPR:
  case IM_PRAGMASELIST:
    pragmanargs = ILM_OPND(ilmp, 1);
    pragmatype = ILM_OPND(ilmp, 2);
    pragmascope = ILM_OPND(ilmp, 3);
    pragmaarg = ILM_OPND(ilmp, 4);
    pragmasym = 0;
    parentilix = 0;
    parentnmex = 0;
    devarg = 0;
    argili = 0;
    if (opc == IM_PRAGMASELIST) {
      ILM *ilmp1;
      int arg, depth;
      /* pragmaarg is an ILM pointer to the IM_BASE of the symbol */
      arg = pragmaarg;
      ilmp1 = (ILM *)(ilmb.ilm_base + arg);
      while (ILM_OPC(ilmp1) == IM_ELEMENT) {
        /* can come from inlining */
        arg = ILM_OPND(ilmp1, 2);
        ilmp1 = (ILM *)(ilmb.ilm_base + arg);
      }
      argili = ILI_OF(arg);
      switch (ILM_OPC(ilmp1)) {
      case IM_PLD:
      case IM_MEMBER:
        pragmasym = ILM_OPND(ilmp1, 2);
        break;
      case IM_BASE:
        pragmasym = ILM_OPND(ilmp1, 1);
        break;
      default:
        if (IM_TYPE(ILM_OPC(ilmp1)) == IMTY_CONS)
          return; /* substituted by inlining? */
        interr("pragma: bad ilmopc", ILM_OPC(ilmp1), 3);
        pragmasym = 0;
      }
      depth = 0;
      while (arg > 1) {
        ILM *argilm = (ILM *)(ilmb.ilm_base + arg);
        switch (ILM_OPC(argilm)) {
        case IM_PLD:
          if (depth == 0) {
            arg = ILM_OPND(argilm, 1);
          } else {
            parentilix = ILI_OF(arg);
            parentnmex = NME_OF(arg);
            arg = 0;
          }
          break;
        case IM_MEMBER:
          ++depth;
          if (depth == 1) {
            arg = ILM_OPND(argilm, 1);
          } else {
            parentilix = ILI_OF(arg);
            parentnmex = NME_OF(arg);
            arg = 0;
          }
          break;
        case IM_ELEMENT:
          parentilix = ILI_OF(arg);
          parentnmex = NME_OF(arg);
          arg = 0;
          break;
        case IM_BASE:
          parentilix = ILI_OF(arg);
          parentnmex = NME_OF(arg);
          arg = 0;
          break;
        }
      }
    }
    if (pragmasym == hostsptr)
      devarg = devsptr;
    break;
  case IM_PRAGMAEXPR:
    pragmanargs = ILM_OPND(ilmp, 1);
    pragmatype = ILM_OPND(ilmp, 2);
    pragmascope = ILM_OPND(ilmp, 3);
    pragmaarg = ILM_OPND(ilmp, 4);
    switch (pragmatype) {
    case PR_ACCVECTOR:
      break;
    case PR_ACCGANG:
      break;
    case PR_ACCGANGDIM:
    break;
    case PR_ACCGANGCHUNK:
    break;
    case PR_ACCWORKER:
      break;
    case PR_ACCAUTO:
      break;
    case PR_ACCPARALLEL:
      break;
    case PR_ACCSEQ:
      break;
    case PR_ACCHOST:
      break;
    case PR_ACCDEVICEID:
      break;
    case PR_ACCIF:
    break;
    case PR_ACCASYNC:
    break;
    case PR_ACCNUMWORKERS:
    break;
    case PR_ACCNUMGANGS:
    break;
    case PR_ACCNUMGANGS2:
    break;
    case PR_ACCNUMGANGS3:
    break;
    case PR_ACCVLENGTH:
    break;
    case PR_ACCSEQUNROLL:
      break;
    case PR_ACCPARUNROLL:
      break;
    case PR_ACCVECUNROLL:
      break;
    case PR_ACCUNROLL:
      break;
    case PR_KERNEL_BLOCK:
      break;
    case PR_KERNEL_GRID:
      break;
    case PR_KERNEL_NEST:
      break;
    case PR_KERNEL_STREAM:
      break;
    case PR_KERNEL_DEVICE:
      break;
    case PR_ACCWAITARG:
      break;
    }
    break;
  case IM_PRAGMAGEN:
    pragmanargs = ILM_OPND(ilmp, 1);
    pragmatype = ILM_OPND(ilmp, 2);
    pragmascope = ILM_OPND(ilmp, 3);
    pragmaarg = ILM_OPND(ilmp, 4);
    switch (pragmatype) {
    case PR_ACCEL:
      break;
    case PR_ENDACCEL:
      break;
    case PR_ACCKERNELS:
      break;
    case PR_ACCENDKERNELS:
      break;
    case PR_ACCPARCONSTRUCT:
      break;
    case PR_ACCENDPARCONSTRUCT:
      break;
    case PR_ACCSCALARREG:
      break;
    case PR_ACCENDSCALARREG:
      break;
    case PR_ACCSERIAL:
      break;
    case PR_ACCENDSERIAL:
      break;
    case PR_ACCDATAREG:
      break;
    case PR_ACCIMPDATAREG:
      break;
    case PR_ACCIMPDATAREGX:
      break;
    case PR_ACCENDDATAREG:
      break;
    case PR_ACCENDIMPDATAREG:
      break;
    case PR_ACCENTERDATA:
      break;
    case PR_ACCEXITDATA:
      break;
    case PR_ACCFINALEXITDATA:
      break;
    case PR_ACCBEGINDIR:
      break;
    case PR_ACCELLP:
      break;
    case PR_ACCKLOOP:
      break;
    case PR_ACCPLOOP:
      break;
    case PR_ACCUPDATE:
      break;
    case PR_ACCSHORTLOOP:
      break;
    case PR_ACCKERNEL:
      break;
    case PR_ACCINDEPENDENT:
      break;
    case PR_ACCWAIT:
      break;
    case PR_ACCNOWAIT:
      break;
    case PR_KERNELBEGIN:
      break;
    case PR_KERNEL:
      break;
    case PR_ENDKERNEL:
      break;
    case PR_ACCWAITDIR:
      break;
    case PR_ACCREDUCTOP:
      accreduct_op = ILM_OPND(ilmp, 4);
      break;
    case PR_ACCCACHEDIR:
      break;
    case PR_ACCCACHEREADONLY:
      break;
    case PR_ACCHOSTDATA:
      if (ACC_DATAMOVEMENT_DISABLED)
        break;
      break;
    case PR_ACCENDHOSTDATA:
      if (ACC_DATAMOVEMENT_DISABLED)
        break;
      break;
    case PR_ACCCOLLAPSE:
      break;
    case PR_ACCFORCECOLLAPSE:
      break;
    case PR_ACCDEFNONE:
      break;
    case PR_ACCDEFPRESENT:
      break;
    default:
      break;
    }
    break;
#ifdef IM_ALLOCA
  case IM_DEALLOCA:
    if (bihb.parfg || bihb.taskfg || ILM_OPND(ilmp, 4) == 1) {
      /*  void RTE_auto_dealloc($p) */
      s = ILM_OPND(ilmp, 3);
      ilix = ILI_OF(ILM_OPND(ilmp, 1));
      tmp = ad1ili(IL_NULL, 0);
#if defined(TARGET_X8664)
      tmp = ad3ili(IL_DAAR, ilix, ARG_IR(0), tmp);
#else
      tmp = ad3ili(IL_ARGAR, ilix, tmp, 0);
#endif
      ilix = ad2ili(IL_JSR, s, tmp);
      chk_block(ilix);
    }
    break;
#endif

#ifdef IM_BEGINATOMIC
  case IM_BEGINATOMIC: {
    wr_block();
    cr_block();
    set_is_in_atomic(1);
    set_atomic_store_created(0);
  } break;
#endif

#ifdef IM_BEGINATOMICCAPTURE
  case IM_BEGINATOMICCAPTURE: {
    wr_block();
    cr_block();
    set_is_in_atomic_capture(1);
    set_atomic_store_created(0);
    set_atomic_capture_created(0);
    set_capture_read_ili(0);
    set_capture_update_ili(0);
  } break;
#endif

#ifdef IM_BEGINATOMICREAD
  case IM_BEGINATOMICREAD: {
    wr_block();
    cr_block();
    set_is_in_atomic_read(1);
    set_atomic_store_created(0);
  } break;
#endif

#ifdef IM_BEGINATOMICWRITE
  case IM_BEGINATOMICWRITE: {
    wr_block();
    cr_block();
    set_is_in_atomic_write(1);
    set_atomic_store_created(0);
  } break;
#endif

#ifdef IM_ENDATOMIC
  case IM_ENDATOMIC: {
    if (get_is_in_atomic_capture()) {
      if (get_capture_read_ili() == 0 || get_capture_update_ili() == 0 ||
          !get_atomic_capture_created()) {
        error(155, 3, gbl.lineno, "Invalid/Incomplete atomic capture.", CNULL);
      }
      set_is_in_atomic_capture(0);
    } else {
      if (!get_atomic_store_created()) {
        error(155, 3, gbl.lineno, "Invalid atomic region.", CNULL);
      }
      set_is_in_atomic(0);
      set_is_in_atomic_read(0);
      set_is_in_atomic_write(0);
    }
  } break;
#endif

  default:
    interr("exp_misc:ilm not cased", opc, 3);
  }
}

/** \brief Shared function for calling target specific exp_header */
static void
begin_entry(int esym)
{
  int tmp;

  exp_header(esym);
  if (esym == 0 && gbl.multiversion > 1)
    setfile(1, SYMNAME(gbl.currsub), ilmb.globalilmstart);
  if (!gbl.outlined)
    ccff_open_unit();
  if (esym == 0)
    entry_sptr = gbl.currsub;
  else
    entry_sptr = esym;
  if (gbl.vfrets) { /* subprogram contains <expr> in FORMATs */
    if (esym == 0) {
      /* first time for subprogram */
      tmp = getccsym('Q', expb.gentmps++, ST_VAR);
      SCP(tmp, SC_STATIC);
      DTYPEP(tmp, DT_DCMPLX); /* need at least 3 words */
      vf_addr = mk_address(tmp);
    }
    tmp = ad1ili(IL_FPSAVE, vf_addr); /* have sched save fp */
    chk_block(tmp);
  }
  if (gbl.arets && esym == 0) {
    expb.aret_tmp = getccsym('Q', expb.gentmps++, ST_VAR);
    SCP(expb.aret_tmp, SC_AUTO);
    DTYPEP(expb.aret_tmp, DT_INT);
  }
  if (gbl.denorm) {
    int addr, mask;
    int sym, arg;
    if (esym == 0) {
      expb.mxcsr_tmp = getccsym('Q', expb.gentmps++, ST_VAR);
      SCP(expb.mxcsr_tmp, SC_AUTO);
      DTYPEP(expb.mxcsr_tmp, DT_INT);
      ADDRTKNP(expb.mxcsr_tmp, 1);
    }
    /*
     *  __pgi_mask_mxcsr(int mask, int *psv)
     */
    mask = ad_icon(0xffff7fbf); /* clear bit 15 (FZ) & bit 6 (DAZ) */
    addr = ad_acon(expb.mxcsr_tmp, 0);
    sym = mkfunc("__pgi_mask_mxcsr");
    arg = ad1ili(IL_NULL, 0);
#if defined(TARGET_X8664)
    arg = ad3ili(IL_DAIR, mask, ARG_IR(0), arg);
    arg = ad3ili(IL_DAAR, addr, ARG_IR(1), arg);
#else
    arg = ad3ili(IL_ARGAR, addr, arg, 0);
    arg = ad2ili(IL_ARGIR, mask, arg);
#endif
    tmp = ad2ili(IL_JSR, sym, arg);
    iltb.callfg = 1;
    chk_block(tmp);
  }

}

void
exp_restore_mxcsr(void)
{
  if (gbl.denorm) {
    int addr, nme, tmp;
    int sym, arg;
    addr = ad_acon(expb.mxcsr_tmp, (INT)0);
    nme = addnme(NT_VAR, expb.mxcsr_tmp, 0, (INT)0);
    tmp = ad3ili(IL_LD, addr, nme, MSZ_WORD);
    /*
     *  __pgi_restore_mxcsr(int sv)
     */
    sym = mkfunc("__pgi_restore_mxcsr");
    arg = ad1ili(IL_NULL, 0);
    arg = ad3ili(IL_ARGIR, tmp, arg, 0);
    tmp = ad2ili(IL_JSR, sym, arg);
    iltb.callfg = 1;
    chk_block(tmp);
  }
}

static void
store_aret(int val)
{
  int addr;
  int nme;
  int tmp;

  addr = ad_acon(expb.aret_tmp, (INT)0);
  nme = addnme(NT_VAR, expb.aret_tmp, 0, (INT)0);
  tmp = ad4ili(IL_ST, val, addr, nme, MSZ_WORD);
  ADDRCAND(tmp, nme);
  chk_block(tmp);
}

int
exp_get_sdsc_len(int s, int base, int basenm)
{
  int sdsc;
  int len, scale, elmsz;
  int ili, acon;
  sdsc = SDSCG(s);
  PTRSAFEP(sdsc, 1);
#if DEBUG
  assert((DDTG(DTYPEG(s)) == DT_ASSCHAR || DDTG(DTYPEG(s)) == DT_ASSNCHAR ||
          DDTG(DTYPEG(s)) == DT_DEFERCHAR || DDTG(DTYPEG(s)) == DT_DEFERNCHAR),
         "exp_get_sdsc_len expects deferred or assumed length character type",
         s, 3);
#endif

  /* the DESC_HDR_BYTE_LEN is 32-bit in the descriptor if not compiled with
   * -i8/Mlarge_arrays
   * make sure it is 64-bit
   */
  len = get_sdsc_element(sdsc, DESC_HDR_BYTE_LEN, base, basenm);
  if (XBIT(68, 0x20) && IL_RES(ILI_OPC(len)) != ILIA_KR) {
    len = ad1ili(IL_IKMV, len);
  } else
  {
    len = kimove(get_sdsc_element(sdsc, DESC_HDR_BYTE_LEN, base, basenm));
  }
  return len;
}

SPTR
frte_func(SPTR (*pf)(const char *), const char *root)
{
  char bf[32];
  char *p;
  SPTR sym;

  p = bf;
  strcpy(p, root);
#if DEBUG
  assert((int)strlen(bf) <= 31, "frte_func:exceed bf", sizeof(bf), 3);
#endif
  sym = (*pf)(bf);
  return sym;
}
