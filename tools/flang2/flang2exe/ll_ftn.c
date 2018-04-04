/*
 * Copyright (c) 2016-2018, NVIDIA CORPORATION.  All rights reserved.
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
   FIXME - document what this is
 */

#include "gbldefs.h"
#include "error.h"
#include "global.h"
#include "symtab.h"
#include "ili.h"
#include "machreg.h"
#include "dinit.h"
#include "cg.h"
#include "x86.h"
#include "fih.h"
#include "pd.h"
#include "llutil.h"
#include "ll_structure.h"
#include <stdlib.h>
#include "expand.h"
#include "llassem.h"
#include "cgllvm.h"

/* debug switches:
   -Mq,11,16 dump ili right before ILI -> LLVM translation
   -Mq,12,16 provides dinit info, ilt trace, and some basic preprocessing info
   -Mq,12,32 provides complete flow debug info through the LLVM routines
*/

#define DBGTRON DBGBIT(12, 0x20)
#define DBGTRACEIN(str) DBGXTRACEIN(DBGTRON, 1, str)
#define DBGTRACEOUT(str) DBGXTRACEOUT(DBGTRON, 1, str)
#define DBGDUMPLLTYPE(str, llt) DBGXDUMPLLTYPE(DBGTRON, 1, str, llt)
#define DBGTRACE5(str, p1, p2, p3, p4, p5) \
  DBGXTRACE5(DBGTRON, 1, str, p1, p2, p3, p4, p5)

#define MAXARGLEN 256

#define LLVM_SHORTTERM_AREA 14

/* -~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~- */
/* local types */

typedef struct char_len {
  int sptr;
  struct char_len *next;
} sclen;

/* -~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~- */
/* local prototypes */

static void stb_process_iface_chlen(int);

/* -~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~- */
/* variables */

int master_sptr = 0;
static ISZ_T f90_equiv_sz = 0;
static LL_Type *equiv_type = NULL;
static char *equiv_var = NULL;

/* -~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~-~=*=~- */
/* implementation */

static int
get_func_altili(int ilix)
{
  if (ILI_ALT(ilix) && ILI_OPC(ILI_ALT(ilix)) == IL_GJSR)
    return ILI_ALT(ilix);
  return 0;
}

/* return argument dtype in IL GJSR , expect ili derived from IL_GJSR */
static int
get_altili_dtype(int param_ili)
{
  if (ILI_OPC(param_ili) != IL_NULL)
    return ILI_OPND(param_ili, 3);
  return 0;
}

LOGICAL
is_fastcall(int ilix)
{
  switch (ILI_OPC(ilix)) {
  case IL_QJSR: /* sym lnk */
  case IL_JSR:  /* sym lnk */
  case IL_JSRA: /* arlnk lnk stc  , arlnk is the address of function */
    switch (ILI_OPC(ILI_OPND(ilix, 2))) {
    /* mth_i_ ..  routines? */
    case IL_DADP: /* dplnk dp lnk */
    case IL_DASP: /* splnk sp lnk */
    case IL_DACS: /* cslnk cs lnk */
    case IL_DACD: /* cdlnk cd lnk */
      return TRUE;
    }
    break;
  default:
    break;
  }
  return FALSE;
}

void
stb_process_routine_parameters(void)
{
  int fsptr;

  ll_process_routine_parameters(gbl.currsub);
  /* Process Entry */
  for (fsptr = SYMLKG(gbl.currsub); fsptr > NOSYM; fsptr = SYMLKG(fsptr)) {
    stb_process_iface_chlen(fsptr); /* fix up char len dummy args */
    ll_process_routine_parameters(fsptr);
  }
}

char *
get_llvm_ifacenm(int sptr)
{
  char *nm = (char *)getitem(LLVM_LONGTERM_AREA, MAXARGLEN);
  strcpy(nm, get_llvm_name(sptr));
#if DEBUG
  assert((strlen(get_llvm_name(gbl.currsub)) + strlen(get_llvm_name(sptr)) + 4) <
             MAXARGLEN,
         "get_llvm_ifacenm: name too long", sptr, 4);
#endif
  return nm;
}

/* Given an sptr, return the iface if it exists, or 0 otherwise */
int
get_iface_sptr(int sptr)
{
  const int dtype = DTYPEG(sptr);
  if (DTY(dtype) == TY_PTR && DTY(DTY(dtype + 1)) == TY_PROC)
    return DTY(DTY(dtype + 1) + 2);
  return 0;
}

/* Returns the Fortran representation of a function name, taking into account if
 * the function is an interface.
 *
 * CAUTION XXX: This returns a pointer from get_llvm_name, which returns a stack
 * address.
 */
static const char *
get_ftn_func_name(int func_sptr, LOGICAL *has_iface)
{
  *has_iface = FALSE;
  if (func_sptr != gbl.currsub) {
    if (!gbl.currsub)
      return NULL;
    if (SCG(func_sptr) == SC_EXTERN || INMODULEG(func_sptr) ||
        OUTLINEDG(func_sptr) || ((STYPEG(func_sptr) == ST_ENTRY) &&
                                 has_multiple_entries(gbl.currsub))) {
      return get_llvm_name(func_sptr); /* module subroutine */
    }
    /* interface name to be hashed has the format:
     * <get_llvm_name(gbl.currsub)>_$_<get_llvm_name(func_sptr)>
     */
    *has_iface = TRUE;
    return get_llvm_ifacenm(func_sptr);
  } else if ((gbl.internal == 1) && (gbl.rutype == RU_PROG)) {
    return get_main_progname();
  }
  return get_llvm_name(func_sptr);
}

void
ll_process_routine_parameters(int func_sptr)
{
  int params, sc, param_sptr, dtype, return_dtype, param_dtype;
  int gblsym, fval, clen, param_num, ref_dtype;
  LL_ABI_Info *abi;
  sclen *t_len, *c_len = NULL;
  LOGICAL update;
  LOGICAL iface = FALSE;
  const char *nm;
  LL_Type *ref_dummy;
  LOGICAL hiddenarg = TRUE;
  int display_temp = 0;

  if (func_sptr < 1)
    return;
  /* If we already processed this and the func_sptr is for a differnt function
   * being compiled, then return early. Else, we need to update the sptrs in
   * the AG table for the LL_ABI.
   */
  nm = get_ftn_func_name(func_sptr, &iface);
  assert(nm, "get_ftn_func_name(): Could not find name", func_sptr, 0);
  gblsym = find_ag(nm);
  update = ((gblsym &&
             (gbl.currsub == func_sptr || get_master_sptr() == func_sptr)) ||
            STYPEG(func_sptr) == ST_ENTRY);
  if (gblsym && !update && is_llvmag_entry(gblsym))
    return;

  if (!gblsym) {
    gblsym = iface
      ? get_llvm_funcptr_ag(func_sptr, (char *)nm) // FIXME castaway const?
      : get_ag(func_sptr);
  }

  if (!update && (abi = ll_proto_get_abi(ll_proto_key(func_sptr))) &&
      abi->nargs)
    return;

  /* It is possible that we have ag but it is not ST_ENTRY */
  if (STYPEG(func_sptr) == ST_ENTRY)
    set_llvmag_entry(gblsym);

  /* At this point, we have a valid gblsym, perhaps already processed.  We
   * still need to update the AG table sptr entries if the func_sptr being
   * processed is this function.
   */
  clen = 0;
  c_len = NULL;
  t_len = NULL;

  /* Store return type (if we are overriding get_return_dtype()) */
  if (gbl.arets && (!CFUNCG(func_sptr))) {
    return_dtype = DT_INT;
    set_ag_return_lltype(gblsym, make_lltype_from_dtype(return_dtype));
  } else {
    return_dtype = get_return_type(func_sptr);
  }
  sc = SCG(func_sptr);

  DBGTRACEIN("")
  DBGTRACE5("#function \"%s\" (%s), sptr %d returning dtype=%d(%s)",
            get_llvm_name(func_sptr), stb.scnames[sc], func_sptr, return_dtype,
            stb.tynames[DTY(return_dtype)])

  params = PARAMCTG(func_sptr);
  fval = FVALG(func_sptr);
  clen = 0;
  c_len = NULL;
  param_num = 0;

  /* Create a dummy LL_Type for use when passing by ref.
   * This will either be a i32* or i64*.
   */
  ref_dtype = generic_dummy_dtype();
  ref_dummy = make_generic_dummy_lltype();

  /* If an internal function */
  if ((gbl.internal > 1 && STYPEG(func_sptr) == ST_ENTRY) &&
      !OUTLINEDG(func_sptr)) { 
    /* get the display variable. This will be the last argument. */ 
    display_temp = aux.curr_entry->display;
    if (aux.curr_entry->display) {
      display_temp = aux.curr_entry->display;
      DTYPEP(display_temp, ref_dtype); /* fake type */
    } else {
      display_temp = getccsym('S', gbl.currsub, ST_VAR);
      /* we won't make type as at the time we generate the prototype, we don't
       * know
       * what members it has.
       */
      SCP(display_temp, SC_DUMMY);
      DTYPEP(display_temp, ref_dtype); /* fake type */
    }
  }

  if (fval) {
    LOGICAL nchar = FALSE;
    param_dtype = DTYPEG(fval);
    dtype = DTY(param_dtype);

    if (DT_ISCMPLX(param_dtype)) {
      if (XBIT(70, 0x40000000) && (CFUNCG(func_sptr) || CMPLXFUNC_C)) {
        if ((POINTERG(fval) || ALLOCATTRG(fval)) && SCG(MIDNUMG(fval)) == SC_DUMMY)
          hiddenarg = TRUE;
        else
          hiddenarg = FALSE;
      }
    } else if (CFUNCG(func_sptr) && DTY(param_dtype) == TY_STRUCT) {
        hiddenarg = FALSE;
    }

    nchar = (DTYG(param_dtype) == TY_NCHAR ||
             (dtype == TY_PTR && DTY(dtype + 1) == DT_NCHAR));
    if (DTYG(param_dtype) == TY_CHAR ||
        (dtype == TY_PTR && DTY(dtype + 1) == DT_CHAR) || nchar) {
      /* If func_sptr has return type(that is not 0), len is put right after
       * return fval
       * else len is put as normal argument - the end of all arguments.
       */
      addag_llvm_argdtlist(gblsym, param_num, fval, ref_dummy);
      ++param_num;

      clen = CLENG(fval);
      if (!clen) {
        clen = getdumlen();
        CLENP(fval, clen);
      } else if (SCG(clen) == SC_LOCAL) {
        clen = getdumlen();
        CLENP(fval, clen);
      }
      if (PARREFG(fval))
        PARREFP(clen, 1);
      if (DTYPEG(func_sptr)) {
        /* fixed size length, put size immediately after return value
         */
        addag_llvm_argdtlist(gblsym, param_num, clen,
                             make_lltype_from_dtype(DTYPEG(clen)));
        ++param_num;
      } else {
        if (c_len) {
          t_len->next = (sclen *)getitem(LLVM_SHORTTERM_AREA, sizeof(sclen));
          t_len = t_len->next;
        } else {
          c_len = (sclen *)getitem(LLVM_SHORTTERM_AREA, sizeof(sclen));
          t_len = c_len;
        }
        t_len->sptr = clen;
        t_len->next = NULL;
      }
    } else if (TY_ARRAY == DTY(param_dtype) || 
               (TY_STRUCT == DTY(param_dtype) && !CFUNCG(func_sptr))||
               (((SCG(fval) == SC_BASED) || (SCG(fval) == SC_DUMMY)) && POINTERG(fval)) ||
               (((SCG(fval) == SC_BASED) || (SCG(fval) == SC_DUMMY)) && ALLOCATTRG(fval)) ||
               ((hiddenarg) && is_struct_kind(param_dtype, TRUE, TRUE))) {

      if (!is_iso_cptr(param_dtype)) {
        if (MIDNUMG(fval) && SCG(MIDNUMG(fval)) == SC_DUMMY)
          fval = MIDNUMG(fval);
        addag_llvm_argdtlist(gblsym, param_num, fval, ref_dummy);
        ++param_num;
        clen = 1;
      }
    }
  }

  if (params) {
    int *dpdscp = (int *)(aux.dpdsc_base + DPDSCG(func_sptr));

    /* Get a temporary abi so that we can call our abi classifiers */
    abi = ll_abi_alloc(cpu_llvm_module, params);
    abi->is_fortran = TRUE;

    while (params--) {
      param_sptr = *dpdscp++;
      if (param_sptr) {
        if (param_sptr == FVALG(func_sptr))
          continue;
        clen = 1;
        param_dtype = DTYPEG(param_sptr);
        if (DTY(param_dtype) == TY_STRUCT && is_iso_cptr(param_dtype)) {
          param_dtype = DT_ADDR;
        }
        /* For string, need to ut length */
        if (!PASSBYVALG(param_sptr) &&
            (DTYG(param_dtype) == TY_CHAR || DTYG(param_dtype) == TY_NCHAR)) {
          int len = CLENG(param_sptr);
          if ((len <= NOSYM) || (SCG(len) == SC_NONE) || 
              (SCG(len) == SC_LOCAL)) {
            len = getdumlen();
            CLENP(param_sptr, len);
          }
          if (PARREFG(param_sptr))
            PARREFP(len, 1);
          PASSBYVALP(len, 1);
          if (len) {
            if (c_len) {
              t_len->next =
                  (sclen *)getitem(LLVM_SHORTTERM_AREA, sizeof(sclen));
              t_len = t_len->next;
            } else {
              c_len = (sclen *)getitem(LLVM_SHORTTERM_AREA, sizeof(sclen));
              t_len = c_len;
            }
            t_len->sptr = len;
            t_len->next = NULL;
          }
        }

        if (!PASSBYVALG(param_sptr)) { /* If pass by reference... */
          LL_Type *llt;
          if (OUTLINEDG(func_sptr))
            llt = make_ptr_lltype(make_lltype_from_dtype(DTYPEG(param_sptr)));
          else
            llt = ref_dummy;
          addag_llvm_argdtlist(gblsym, param_num, param_sptr, llt);
          ++param_num;
        } else { /* Else, pass by value */
          LL_Type *type;
          LL_ABI_ArgInfo arg = {0};
          if (is_iso_cptr(DTYPEG(param_sptr)))
            type = ref_dummy;
          else {
            if ((DTY(param_dtype) == TY_CHAR || DTY(param_dtype) == TY_NCHAR) &&
                (DTY(param_dtype + 1) == 1)) {
                type = make_lltype_from_dtype(DT_BINT);
            } else {
              ll_abi_classify_arg_dtype(abi, &arg, param_dtype);
              ll_abi_complete_arg_info(abi, &arg, param_dtype);
              type = make_lltype_from_abi_arg(&arg);
            }
          }
          addag_llvm_argdtlist(gblsym, param_num, param_sptr, type);
          ++param_num;
        }
      }
    }

    /* This was just a temporary state to call the classifiers with */
    ll_abi_free(abi);

    /* print clen */
    t_len = c_len;
    while (t_len) {
      param_dtype = DTYPEG(t_len->sptr);
      addag_llvm_argdtlist(gblsym, param_num, t_len->sptr,
                           make_lltype_from_dtype(param_dtype));
      ++param_num;
      t_len = t_len->next;
    }
  }

  if (display_temp != 0) {
    /* place display_temp as last argument */
    addag_llvm_argdtlist(gblsym, param_num, display_temp, ref_dummy);
    ++param_num;
  }


  if (iface) {
    set_llvm_iface_oldname(gblsym, get_llvm_name(func_sptr));
  }

  add_ag_typename(gblsym, (char *)char_type(return_dtype, 0));
  if (gbl.arets && (!CFUNCG(func_sptr)))
    set_ag_lltype(gblsym, make_lltype_from_dtype(DT_INT));

  /* If we got this far, then we have established an argdtlist, perhaps it is
   * null with no params, and that is still valid.
   */
  set_ag_argdtlist_is_valid(gblsym);

  /* Add the abi */
  abi = process_ll_abi_func_ftn(func_sptr, TRUE);
  ll_proto_add_sptr(func_sptr, abi);

  if (flg.smp && OUTLINEDG(func_sptr) && gbl.internal > 1) {
    ll_shallow_copy_uplevel(gbl.currsub, func_sptr);
  }

  freearea(LLVM_SHORTTERM_AREA);

  DBGTRACEOUT("")
} /* ll_process_routine_parameters */

/*
 * same return value as strcmp(str, pattern); pattern is a lower case
 * string and str may contain upper case characters.
 */
static int
sem_strcmp(char *str, char *pattern)
{
  char *p1, *p2;
  int ch;

  p1 = str;
  p2 = pattern;
  do {
    ch = *p1;
    if (ch >= 'A' && ch <= 'Z')
      ch += ('a' - 'A'); /* to lower case */
    if (ch != *p2)
      return (ch - *p2);
    if (ch == '\0')
      return 0;
    p1++;
    p2++;
  } while (1);
}

int
is_iso_cptr(int d_dtype)
{
  int tag;
  if (DTY(d_dtype) == TY_ARRAY)
    d_dtype = DTY(d_dtype + 1);

  if (DTY(d_dtype) != TY_STRUCT)
    return 0;

  tag = DTY(d_dtype + 3);

  if (ISOCTYPEG(tag))
    return d_dtype;

  return 0;
}

/**
   \brief Get the return \c DTYPE of the function, \p func_sptr.
   \param func_sptr  Symbol id of function to examine
 */
int
get_return_type(int func_sptr)
{
  int fval, dtype;

  if ((SCG(func_sptr) == SC_DUMMY) && MIDNUMG(func_sptr))
    func_sptr = MIDNUMG(func_sptr);

  fval = FVALG(func_sptr);
  if (fval) {
    if (POINTERG(fval) || ALLOCATTRG(fval))
      return 0;
    dtype = DTYPEG(fval);
  } else {
    dtype = DTYPEG(func_sptr);
  }
  if (POINTERG(func_sptr) || ALLOCATTRG(func_sptr))
    return 0;
  switch (DTY(dtype)) {
  case TY_CHAR:
  case TY_NCHAR:
  case TY_ARRAY:
    return 0;
  case TY_STRUCT:
  case TY_UNION:
    if (is_iso_cptr(dtype))
      return DT_ADDR;
    if (CFUNCG(func_sptr))
      break;
    return 0;
  case TY_CMPLX:
  case TY_DCMPLX:
    if (CFUNCG(func_sptr) || CMPLXFUNC_C)
      break;
    return 0;
  default:
    break;
  }
  return dtype;
}

void
assign_array_lltype(int dtype, int size, int sptr)
{
  LLTYPE(sptr) = make_array_lltype(size, make_lltype_from_dtype(dtype));
}

void
write_llvm_lltype(int sptr)
{
  write_type(LLTYPE(sptr));
}

static void
stb_process_iface_chlen(int sptr)
{
  int i;
  int e = sptr;
  int dpdsc = DPDSCG(e);
  int paramct = PARAMCTG(e);

  for (i = 0; i < paramct; ++i) {
    int param = aux.dpdsc_base[dpdsc + i];
    int dtype = DDTG(DTYPEG(param));
    if (dtype == DT_DEFERCHAR || dtype == DT_DEFERNCHAR) {
      if (!CLENG(param)) {
        int clen = getdumlen();
        CLENP(param, clen);
        if (PARREFG(param))
          PARREFP(clen, 1);
      }
    } else if (dtype == DT_ASSCHAR || dtype == DT_ASSNCHAR) {
      if (!CLENG(param)) {
        int clen = getdumlen();
        CLENP(param, clen);
        if (PARREFG(param))
          PARREFP(clen, 1);
      }
    }
  }
}

static int
llvm_args_valid(int func_sptr)
{
  /* This is a workaround  - there maybe a place in the front end that we don't
   * process module routine arguments - if that is the case don't put it in ag
   * table.
   * it will replace the correct one because we can have same routine multiple
   * times
   * in ilm file by use associate.
   */
  int valid = 1;
  int argcnt = PARAMCTG(func_sptr);
  int fval = FVALG(func_sptr);
  int dtype;

  if (!fval)
    return valid;

  if (CFUNCG(func_sptr))
    return valid;

  if (argcnt) {
    int *dpdscp = (int *)(aux.dpdsc_base + DPDSCG(func_sptr));
    if (fval == *dpdscp)
      return valid;

    dtype = get_return_type(func_sptr);
    if (dtype == 0 && DTYPEG(fval) != 0)
      return 0;
  }

  return valid;
}

void
fix_llvm_fptriface(void)
{
/* Process function interface and store in ag table - need to do when process
   stb file
   because
   0.  This function needs to be called in main even without code.
   1.  All function info must be in ag table already so that vft processing can
   get
       correct function signature.
   2.  For inlining(i.e., ieee03), Currently when we read symbol from inlining
   ilm
       we have no information about that symbol at all, we then put incorrect
   info
       in ag table.  If we process the stb file, we normally have interface
       information at that time, so correct function info is stored in ag table
       first.   When we subsequently inline this function, we would get correct
       info from ag table.
 */

  int dtype, dt, sptr, iface;
  char *ifacenm;

  if (!gbl.currsub)
    return;

  if (!gbl.stbfil)
    return; /* do it when process stb file */

  for (sptr = stb.firstusym; sptr < stb.stg_avail; sptr++) {
    if (SCG(sptr) == SC_BASED)
      continue;
    dtype = DTYPEG(sptr);

    if (SCG(sptr) == SC_EXTERN && STYPEG(sptr) == ST_PROC && INMODULEG(sptr)) {

      /* If routine is in same module as current routine then it is module
         subroutine - should already process for this module.
       */
      if (INMODULEG(gbl.currsub) == INMODULEG(sptr))
        continue;

      stb_process_iface_chlen(sptr); /* fix up char len dummy args */
      ll_process_routine_parameters(sptr);
      continue;
    }
    if (SCG(sptr) == SC_EXTERN && STYPEG(sptr) == ST_PROC) {
      if (CFUNCG(sptr) || PARAMCTG(sptr) ||
          (CMPLXFUNC_C && DTYPEG(sptr) && DT_ISCMPLX(DTYPEG(sptr)))) {
        ifacenm = get_llvm_ifacenm(sptr);
        llvm_funcptr_store(sptr, ifacenm);
        stb_process_iface_chlen(sptr); /* fix up char len dummy args */
        ll_process_routine_parameters(sptr);
        continue;
      }
    }
    if (DTY(dtype) != TY_PTR)
      continue;
    if ((iface = get_iface_sptr(sptr))) {
      ifacenm = get_llvm_ifacenm(iface);
      llvm_funcptr_store(sptr, ifacenm);
      stb_process_iface_chlen(iface); /* fix up char len dummy args */
      ll_process_routine_parameters(iface);
    }
  }
}

void
store_llvm_localfptr(void)
{
/* Store interface function name in fptr_local table.  This table is done per
   routine.
   It stores the name that will be used to search for function signature of what
   it
   points to.  The interface name is in the form of
   <getname(gbl.currsub)>_$_<getname(iface)>,
   which is done in get_llvm_ifacenm().
 */

  int dtype, dt, sptr, iface;
  char *ifacenm;

  if (!gbl.currsub)
    return;

  if (gbl.stbfil)
    return;
}

/* Handle equivalence on stack:
   Collect the size (gbl.locaddr) and create a new array of i8 with size of
   gbl.locaddr.
   In gen_llvm_expr() - use equiv_type instead.
   Its address is the total size + ADDRESSG field(which is negative value).
   ADDRESSG is always negative for SC_LOCAL+SOCPTR.
   lowest_quiv_addr is the lowest address - for native compiler this is the
   offset from
   stack.
 */

void
get_local_overlap_size(void)
{
  char *name;
  ISZ_T align_mask = 15; /* assume maximum alignment is 16 */
  /* create a new variable with [i8 x gbl.locaddr] - note that gbl.locaddr may
   * change later when we process more local variable(s).
   */
  if (gbl.locaddr && !gbl.outlined) {
    f90_equiv_sz = ALIGN(gbl.locaddr, align_mask);
    equiv_type =
        make_array_lltype(f90_equiv_sz, make_lltype_from_dtype(DT_BINT));
    name = get_llvm_name(gbl.currsub);
    equiv_var = (char *)getitem(LLVM_LONGTERM_AREA, strlen(name) + 20);
    sprintf(equiv_var, "%%%s_%s%d", name, "_$eq_", gbl.currsub);
  }
}

char *
get_local_overlap_var(void)
{
  return equiv_var;
}

LL_Type *
get_local_overlap_vartype(void)
{
  return equiv_type;
}

void
write_local_overlap(void)
{
  if (!equiv_var)
    return;

  print_token("\t");
  print_token(equiv_var);
  print_token(" = alloca ");
  write_type(equiv_type);
  print_token(", align 4\n");
}

void
reset_equiv_var(void)
{
  equiv_var = NULL;
  equiv_type = NULL;
}

void
reset_master_sptr(void)
{
  master_sptr = 0;
}

int
get_master_sptr(void)
{
  return master_sptr;
}

ISZ_T
get_socptr_offset(int sptr)
{
  return f90_equiv_sz + (ADDRESSG(sptr));
}

static char *
get_master_entry_name(int sptr)
{
  static char nm[MAXARGLEN];
  sprintf(nm, "%s%s", "_master___", get_llvm_name(sptr));
  return nm;
}

static int
make_new_funcsptr(int oldsptr)
{
  char *nm = get_master_entry_name(oldsptr);
  int sptr = getsym(nm, strlen(nm));
  DTYPEP(sptr, DTYPEG(oldsptr));
  STYPEP(sptr, STYPEG(oldsptr));
  SCP(sptr, SCG(oldsptr));
  CCSYMP(sptr, CCSYMG(oldsptr));
  SYMLKP(sptr, NOSYM);
  CREFP(sptr, CREFG(oldsptr));
#ifdef CUDAP
  CUDAP(sptr, CUDAG(oldsptr));
#endif
  PASSBYVALP(sptr, PASSBYVALG(oldsptr));
  PASSBYREFP(sptr, PASSBYREFG(oldsptr));
  ADDRESSP(sptr, 0);
  FVALP(sptr, FVALG(oldsptr));
  ADJARRP(sptr, ADJARRG(oldsptr));
  DCLDP(sptr, DCLDG(oldsptr));
  INMODULEP(sptr, INMODULEG(oldsptr));
  VTOFFP(sptr, VTOFFG(oldsptr));
  INVOBJP(sptr, INVOBJG(oldsptr));
  INVOBJINCP(sptr, INVOBJINCG(oldsptr));
  FUNCLINEP(sptr, FUNCLINEG(oldsptr));
  CLASSP(sptr, CLASSG(oldsptr));
  DPDSCP(sptr, DPDSCG(oldsptr));
  sym_is_refd(sptr);

  return sptr;
}

/* This function collect all arguments from all Entry include main routine ,
 * removing the duplicates, put into new dpdsc.
 *
 * Returns the master_sptr value
 */
int
get_entries_argnum(void)
{
  int param_cnt, max_cnt, i, param_sptr, *dpdscp, opt, master_dpdsc;
  int sptr = gbl.currsub;
  int fval = FVALG(gbl.currsub);
  int fvaldt = 0;
  int found = 0;
  char name[100];

  if (SYMLKG(sptr) <= NOSYM) /* no Entry */
    return 0;

  /* Create a new sym and gblsym for master */
  master_sptr = make_new_funcsptr(gbl.currsub);

  /* Argument from main routine */
  param_cnt = PARAMCTG(sptr);
  dpdscp = (int *)(aux.dpdsc_base + DPDSCG(sptr));
  master_dpdsc = aux.dpdsc_avl;

  /* Add first argument, the entry_option */
  i = 0;
  sprintf(name, "%s%d", "__master_entry_choice", stb.stg_avail);
  opt = addnewsym(name);
  SCG(opt) = SC_DUMMY;
  DTYPEP(opt, DT_INT);
  STYPEP(opt, ST_VAR);
  PASSBYVALP(opt, 1);
  sym_is_refd(opt);
  max_cnt = 1;
  if (!aux.dpdsc_avl)
    aux.dpdsc_avl++;
  master_dpdsc = aux.dpdsc_avl;
  aux.dpdsc_avl += max_cnt;
  aux.dpdsc_avl = 1;
  NEED(aux.dpdsc_avl, aux.dpdsc_base, int, aux.dpdsc_size,
       aux.dpdsc_size + max_cnt + 100);
  aux.dpdsc_base[master_dpdsc] = opt;
  i = 1;

  /* Add second arg if the following is true */
  if (fval && SCG(fval) != SC_DUMMY) {
    sprintf(name, "%s%d", "__master_entry_rslt", stb.stg_avail);
    opt = addnewsym(name);
    max_cnt++;
    SCG(opt) = SC_DUMMY;
    DTYPEP(opt, DTYPEG(fval));
    STYPEP(opt, ST_VAR);
    sym_is_refd(opt);
    aux.dpdsc_avl += max_cnt;
    aux.dpdsc_base[master_dpdsc + 1] = opt;
    i = 2;
  }

  /* Add all of the known dummies */
  if (param_cnt) {
    max_cnt += param_cnt;
    aux.dpdsc_avl += param_cnt;
    NEED(aux.dpdsc_avl + 1, aux.dpdsc_base, int, aux.dpdsc_size,
         aux.dpdsc_size + param_cnt + 100);

    while (param_cnt--) {
      param_sptr = *dpdscp++;
      aux.dpdsc_base[master_dpdsc + i] = param_sptr;
      ++i;
    }
  }

  /* add argument of entry that is not already in the list */
  for (sptr = SYMLKG(sptr); sptr > NOSYM; sptr = SYMLKG(sptr)) {
    if (sptr == gbl.currsub)
      continue;

    param_cnt = PARAMCTG(sptr);

    if (param_cnt) {
      dpdscp = (int *)(aux.dpdsc_base + DPDSCG(sptr));
      while (param_cnt--) {
        param_sptr = *dpdscp++;
        found = 0;
        for (i = 0; i < max_cnt; i++) {
          if (param_sptr == aux.dpdsc_base[master_dpdsc + i]) {
            found = 1;
            break;
          }
        }
        if (!found) { /* not yet in the list, add to list */
          aux.dpdsc_avl++;
          NEED(aux.dpdsc_avl + 1, aux.dpdsc_base, int, aux.dpdsc_size,
               aux.dpdsc_size + param_cnt + 100);
          aux.dpdsc_base[master_dpdsc + max_cnt] = param_sptr;
          max_cnt++;
        }
      }
    }
  }

  PARAMCTP(master_sptr, max_cnt);
  if (max_cnt) /* should always be true */
    DPDSCP(master_sptr, master_dpdsc);
  DTYPEP(master_sptr, 0); /* subroutine */
  FVALP(master_sptr, 0);

  /* Update the ag entry for master_sptr to have these newly added args */
  ll_process_routine_parameters(master_sptr);
  return master_sptr;
}

static void
_declare_sptr_as_local(int sptr, int flag)
{
  print_token("\t");
  print_token("%");
  print_token(get_llvm_name(sptr));
  print_token(" = alloca ");
  if (flag || PASSBYVALG(sptr))
    write_type(make_lltype_from_dtype(DTYPEG(sptr)));
  else
    write_type(make_lltype_from_dtype(generic_dummy_dtype()));
  print_nl();
}

/* This function will declare all dummy variables from all entries as 
 * local variables if it is not dummy argument of the current Entry. 
 * Then we can pass them to master routine with the right type.
 * Therefore, it must be called after gen_entries_argnum so that we can 
 * compare it against the list.
 */
static void
write_dummy_as_local_in_entry(int sptr)
{
  int param_cnt, i, param_sptr, found, marg_sptr, master_param;
  int *dpdscp, *master_dp;

  param_cnt = PARAMCTG(sptr);
  if (param_cnt) {
    master_dp = (int *)(aux.dpdsc_base + DPDSCG(master_sptr));
    master_param = PARAMCTG(master_sptr);
    for (i = 0; i < master_param; i++, master_dp++) {
      found = 0;
      marg_sptr = *master_dp;
      dpdscp = (int *)(aux.dpdsc_base + DPDSCG(sptr));
      while (param_cnt--) {
        param_sptr = *dpdscp++;
        if (param_sptr == marg_sptr) { /* in current entry dummy arg */
          found = 1;
          break;
        } else if (marg_sptr == FVALG(sptr)) {
          found = 1;
          break;
        }
      }
      if (found == 0) {
        _declare_sptr_as_local(marg_sptr, 0);
      }
      param_cnt = PARAMCTG(sptr);
    }
  } else {
    /* declare all as local variables*/
    master_dp = (int *)(aux.dpdsc_base + DPDSCG(master_sptr));
    for (i = 0; i < PARAMCTG(master_sptr); i++) {
      param_sptr = *master_dp++;
      _declare_sptr_as_local(param_sptr, 0);
    }
  }

  if (FVALG(sptr) && SCG(FVALG(sptr)) != SC_DUMMY) {
    _declare_sptr_as_local(FVALG(sptr), 1);
  }
}

/**
   \brief Write out all Entry's as a separate routine

   Each entry will call a master/common routine (MCR).  The first argument to
   the MCR will determine which label(Entry) control will jump to upon entry
   into the MCR.  If the MCR is a function, the next argument will be the
   function's return value.  The next argument(s) will be all non-duplicate
   aggregate arguments for all entries.  The MCR will always effectively be a
   subroutine.
 */
void
print_entry_subroutine(LL_Module *module)
{
  int sptr = gbl.entries;
  int iter = 0;
  char num[16];
  int i, dtype, param_dtype, clen, fval, rettype;
  int chararg = 0;
  char *nm;
  int *dpdscp;
  TMPS *tmp, *atmp;
  LL_ABI_Info *abi;
  LL_Type *dummy_type;
  hashset_t formals; /* List of formal params for each entry trampoline */

  if (SYMLKG(sptr) <= NOSYM)
    return;

  if (master_sptr == 0) return;

  /* For use when representing formal parameters */
  dummy_type = make_generic_dummy_lltype();

  /* For each entry trampoline */
  formals = hashset_alloc(hash_functions_direct);
  for (; sptr > NOSYM; sptr = SYMLKG(sptr)) {
    tmp = NULL;
    atmp = NULL;
    reset_expr_id(); /* reset a temp runner */

    /* Convenience hash for fast formal paramter identifying */
    hashset_clear(formals);
    abi = process_ll_abi_func_ftn(sptr, TRUE);

    ll_proto_add_sptr(sptr, abi);
    ll_proto_set_defined_body(ll_proto_key(sptr), TRUE);

    /*
     * HACK XXX FIXME: We do not call process_formal_arguments()
     * on any of the routines generated by the print_token commands below.
     * This means process_sptr will not be called for any CCSYM arguments
     * and we need to do that so that there exists an SNAME for those.
     */
    for (i = 1; i <= abi->nargs; ++i) {
      int arg_sptr = abi->arg[i].sptr;
      if (!SNAME(arg_sptr) && CCSYMG(arg_sptr))
        process_sptr(arg_sptr);
      hashset_insert(formals, INT2HKEY(arg_sptr));
    }
    build_routine_and_parameter_entries(sptr, abi, NULL);

    write_dummy_as_local_in_entry(sptr);

    fval = FVALG(sptr);
    if (fval) {
      rettype = DTYPEG(fval);
    } else if (gbl.arets) {
      rettype = DT_INT;
    } else {
      rettype = 0;
    }
    if (fval && SCG(fval) != SC_DUMMY) {
      /* Bitcast fval which is local variable to i8*.
       * We will pass this fval to master routine.
       */
      tmp = make_tmps();
      tmp->id = 0;
      print_token("\t");
      print_tmp_name(tmp);
      print_token(" = bitcast ");
      write_type(make_ptr_lltype(make_lltype_from_dtype(rettype)));
      print_space(1);
      print_token(SNAME(fval));
      print_token(" to ");
      write_type(dummy_type);
      print_space(1);
      print_nl();
    }

    /* call the master */
    if (gbl.arets) {
      atmp = make_tmps();
      print_token("\t");
      print_tmp_name(atmp);
      print_token(" = call ");
      write_type(make_lltype_from_dtype(DT_INT));
      print_token(" @");
    } else {
      print_token("\tcall void @");
    }
    print_token(get_llvm_name(master_sptr));
    print_token("(");

    /* First argument is choice=? */
    write_type(make_lltype_from_dtype(DT_INT));
    snprintf(num, sizeof(num), " %d", iter++);
    print_token(num);

    /* if function, the second argument is the return value. The third argument
       can also be a return value if the return value is a dummy argument
       (happens when types are different). */
    if (tmp) {
      /* pass the tmp about */
      print_token(", ");
      write_type(dummy_type);
      print_space(1);
      print_tmp_name(tmp);
    } else if (fval && SCG(fval) != SC_DUMMY && fval != FVALG(gbl.currsub)) {
      /* If it is a dummy, it should already in the master dpdsc.  */
      print_token(", ");
      write_type(dummy_type);
      print_space(1);
      print_token(SNAME(fval));
      param_dtype = DTYPEG(fval);
      dtype = DTY(param_dtype);
      if (DTYG(param_dtype) == TY_CHAR || DTYG(param_dtype) == TY_NCHAR ||
          (dtype == TY_PTR && DTY(dtype + 1) == DT_CHAR) ||
          (dtype == TY_PTR && DTY(dtype + 1) == DT_NCHAR)) {
        if (DTYPEG(sptr)) {
          clen = CLENG(sptr);
          if (!clen) {
            clen = getdumlen();
            CLENP(sptr, clen);
          }
          print_token(", ");
          write_type(make_lltype_from_dtype(DTYPEG(sptr)));
          print_token(SNAME(clen));
        } else {
          ++chararg;
        }
      }
    }

    dpdscp = (int *)(aux.dpdsc_base + DPDSCG(master_sptr));
    for (i = 0; i < PARAMCTG(master_sptr); i++) {
      int sym = *dpdscp++;
      if (i == 0)
        continue; /* skip choice */
      if (tmp && i == 1)
        continue; /* skip return value */
      print_token(", ");
      if (PASSBYVALG(sym))
        write_type(LLTYPE(sym));
      else
        write_type(dummy_type);
      print_space(1);
      print_token(SNAME(sym));
    }
    /* second loop - check for char arg */
    /* print char len here */
    if (chararg) {
      clen = CLENG(fval);
      print_token(", ");
      write_type(make_lltype_from_dtype(DTYPEG(clen)));
      print_token(" ");
      print_token(SNAME(clen));
    }

    /* check for char arg */
    dpdscp = (int *)(aux.dpdsc_base + DPDSCG(master_sptr));
    for (i = 0; i < PARAMCTG(master_sptr); i++) {
      int sym = *dpdscp++;
      if (i == 0) /* Skip choice */
        continue;
      if (tmp && i == 1)
        continue; /* Skip non-character, return value */
      if (DTYG(DTYPEG(sym)) == TY_CHAR || DTYG(DTYPEG(sym)) == TY_NCHAR) {
        clen = CLENG(sym);
        print_token(", ");
        write_type(make_lltype_from_dtype(DTYPEG(clen)));
        if (clen && hashset_lookup(formals, INT2HKEY(clen))) {
          print_token(SNAME(clen));
        } else {
          print_token(" 0"); /* Default to 0 */
        }
      }
    }

    print_token(")\n\t");

    if (tmp) {
      /* load return value and return it */
      LL_Type *return_ll_type;

      if (!DT_ISCMPLX(rettype) || !CMPLXFUNC_C) {
        return_ll_type = make_lltype_from_dtype(rettype);

        /* %1 = load i32, i32* %cp1_300, align 4 */
        tmp = make_tmps();
        print_tmp_name(tmp);
        print_token(" = load ");
        if (ll_feature_explicit_gep_load_type(&module->ir)) {
          /* Print load type */
          write_type(return_ll_type);
          print_token(", ");
        }
        write_type(make_ptr_lltype(return_ll_type));
        print_space(1);
        print_token(SNAME(fval));
        print_token(", align 4");
        print_nl();
      } else {
        /* complex entry, default C return conventions */
        TMPS *addrtmp;
        return_ll_type = make_lltype_from_abi_arg(&abi->arg[0]);

        /* %1 = bitcast <{float, float}>* %cp1_300 to double* */
        addrtmp = make_tmps();
        print_tmp_name(addrtmp);
        print_token(" = bitcast ");
        write_type(make_ptr_lltype(make_lltype_from_dtype(rettype)));
        print_space(1);
        print_token(SNAME(fval));
        print_token(" to ");
        write_type(make_ptr_lltype(return_ll_type));
        print_nl();

        /* %2 = load double, double* %1, align 4 */
        tmp = make_tmps();
        print_token("\t");
        print_tmp_name(tmp);
        print_token(" = load ");
        /* Print load type */
        write_type(return_ll_type);
        print_token(", ");
        write_type(make_ptr_lltype(return_ll_type));
        print_space(1);
        print_tmp_name(addrtmp);
        print_token(", align 4\n");
      }
      if (abi->extend_abi_return) {
        print_token("\t%.rt = sext ");
        write_type(return_ll_type);
        print_space(1);
        print_tmp_name(tmp);
        print_token(" to ");
        write_type(make_lltype_from_dtype(DT_INT));
        print_nl();
      }
      print_token("\tret ");
      write_type(abi->extend_abi_return ? make_lltype_from_dtype(DT_INT) :
                 return_ll_type);
      print_space(1);
      if (abi->extend_abi_return) {
        print_token("%.rt");
      } else {
        print_tmp_name(tmp);
      }
    } else if (atmp) {
      print_token("ret ");
      write_type(make_lltype_from_dtype(DT_INT));
      print_space(1);
      print_tmp_name(atmp);
    } else {
      print_token("ret void"); /* make sure it return correct type */
    }
    print_nl();
    /* vi matching { */
    print_token("}");
    print_nl();
  }

  hashset_free(formals);
}

LOGICAL
has_multiple_entries(int sptr)
{
  return (SYMLKG(sptr) > NOSYM);
}

void
write_master_entry_routine(void)
{
  LL_ABI_Info *a = process_ll_abi_func_ftn(master_sptr, TRUE);
  build_routine_and_parameter_entries(master_sptr, a, NULL);
}

char *
get_entret_arg_name(void)
{
  int *dpdscp = (int *)(aux.dpdsc_base + DPDSCG(master_sptr));
  dpdscp++;
  return get_llvm_name(*dpdscp);
}

int
mk_charlen_address(int sptr)
{
  int mem, ili, nme, off;
  INT zoff;

  mem = get_sptr_uplevel_address(sptr); /* next one is the address of its len */
  zoff = ADDRESSG(mem);

/* match in load_uplevel_addresses. */
  zoff += 8;
  nme = addnme(NT_VAR, aux.curr_entry->display, 0, (INT)0);
  ili = ad_acon(aux.curr_entry->display, (INT)0);

  off = 0;
  ili = ad2ili(IL_LDA, ili, nme); /* load display struct */
  if (zoff) {
    off = ad_aconi(zoff);
    ili = ad3ili(IL_AADD, ili, off, 0); /* add offset of sptr to display */
  }

  return ili;
}

LL_Type *
get_ftn_lltype(int sptr)
{
  int dtype, gblsym;
  char *name;
  char tname[250];
  LL_Type *llt;
  LL_Type *rslt = NULL;

  if (LLTYPE(sptr))
    return llt;

  switch (SCG(sptr)) {
  case SC_STATIC:
    llt = get_ftn_static_lltype(sptr);
    rslt = llt;
    break;
  case SC_CMBLK:
    llt = get_ftn_cmblk_lltype(sptr);
    rslt = llt;
    break;
  case SC_EXTERN:
    llt = get_ftn_extern_lltype(sptr);
    rslt = llt;
    break;
  default:
    process_sptr(sptr);
    llt = LLTYPE(sptr);
    rslt = llt;
    break;
  }
  return rslt;
}

