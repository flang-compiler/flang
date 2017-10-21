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
 * \brief outliner.c - extract regions into subroutines; add uplevel references
 * as arguments
 *
 */

#include "gbldefs.h"
#include "error.h"
#include "global.h"
#include "symtab.h"
#include "semant.h"
#include "ilmtp.h"
#include "ilm.h"
#include "ili.h"
#include "expand.h"
#include "kmpcutil.h"
#include "outliner.h"
#include "machreg.h"
#include "mp.h"
#include "ll_structure.h"
#include "llmputil.h"
#include "llutil.h"
#include "cgllvm.h"
#ifndef _WIN32
#include <unistd.h>
#endif
#include "regutil.h"

#define MAX_PARFILE_LEN 15

extern LL_Module *cpu_llvm_module; /* To create ABI info for outlined funcs */
FILE *par_file1 = NULL;
FILE *par_file2 = NULL;
FILE *par_curfile = NULL;           /* current tempfile for ilm rewrite */
static FILE *savedILMFil = NULL;
static char parFileNm1[MAX_PARFILE_LEN]; /* temp ilms file: pgipar1XXXXXX */
static char parFileNm2[MAX_PARFILE_LEN]; /* temp ilms file: pgipar2XXXXXX */
static LOGICAL hasILMRewrite = 0;  /* if set, tempfile is not empty. */
static LOGICAL isRewritingILM = 0; /* if set, write ilm to tempfile */
static int funcCnt = 1;            /* keep track how many outlined region */
static int llvmUniqueSym;          /* keep sptr of unique symbol */
static int uplevelSym = 0;
static int gtid;
static LOGICAL writeTaskdup = FALSE;/* if set, write IL_NOP to TASKDUP_FILE */
static int pos;

/* store taskdup ILMs */
static struct taskdupSt {
  ILM_T* file;
  int sz;
  int avl;
}taskdup;

#define TASKDUP_FILE taskdup.file
#define TASKDUP_SZ   taskdup.sz
#define TASKDUP_AVL  taskdup.avl
static void allocTaskdup(int);

/* Forward decls */
extern char *get_ag_name(int);
static void resetThreadprivate();
#ifdef DEBUG
void _dumpilms(ILM_T*, int );
void dumpsingleilm(ILM_T*, int);
#endif

#define DT_VOID_NONE DT_NONE

#define MXIDLEN 250

/* Dump the values being stored in the uplevel argument */
static void
dumpUplevel(int uplevel_sptr)
{
  int i;
  FILE *fp = gbl.dbgfil ? gbl.dbgfil : stdout;

  fprintf(fp, "********* UPLEVEL Struct *********\n");
  for (i = DTY(DTYPEG(uplevel_sptr) + 1); i > NOSYM; i = SYMLKG(i))
    fprintf(fp, "==> %s %s\n", SYMNAME(i), stb.tynames[DTY(DTYPEG(i))]);
  fprintf(fp, "**********\n\n");
}

/* Dump the list of variables for the parallel regions specified by 'sptr'.
 * These
 * variables should be used to make the uplevel struct when making a call to
 * this outlined region.
 *
 */
void
dump_parsyms(int sptr)
{
  int i;
  const LLUplevel *up;
  FILE *fp = gbl.dbgfil ? gbl.dbgfil : stdout;

  assert(STYPEG(sptr) == ST_BLOCK, "Invalid parallel region sptr", sptr, 4);

  up = llmp_get_uplevel(sptr);
  fprintf(fp, "\n********** OUTLINING: Parallel Region "
              "%d (%d shared variables) **********\n",
          sptr, up->vals_count);

  for (i = 0; i < up->vals_count; ++i) {
    const int var = up->vals[i];
    fprintf(fp, "==> %d) %d (%s) (stype:%d, sc:%d)\n", i + 1, var, SYMNAME(var),
            STYPEG(var), SCG(var));
  }
}

static int
genNullArg()
{
  int con, ili;
  INT tmp[2];

  tmp[0] = 0;
  tmp[1] = 0;
  con = getcon(tmp, DT_INT);
  ili = ad1ili(IL_ACON, con);
  return ili;
}

/* Returns a dtype for arguments referenced by stblk_sptr */
int
ll_make_uplevel_type(int stblk_sptr)
{
  int i, j, sz, dtype, nmems, psyms_idx, count, presptr;
  const LLUplevel *up;
  KMPC_ST_TYPE *meminfo = NULL;

  up = llmp_get_uplevel(stblk_sptr);
  count = nmems = up->vals_count;

  if (gbl.internal >= 1)
    nmems = nmems + 1;

  /* Special case: No members (return null ptr) */
  if (!nmems)
    return DT_CPTR;

  /* Add members */
  meminfo = calloc(nmems, sizeof(KMPC_ST_TYPE));
  sz = 0;
  i = 0;
  if (gbl.internal >= 1) {
    meminfo[i].name = strdup(SYMNAME(aux.curr_entry->display));
    meminfo[i].dtype = DT_CPTR;
    meminfo[i].byval = FALSE;
    meminfo[i].psptr = aux.curr_entry->display;
    sz += size_of(DT_CPTR);
    i++;
  }
  presptr = 0;
  for (j = 0; j < count; ++j) {
    int sptr = up->vals[j];
    meminfo[i].name = strdup(SYMNAME(sptr));
    meminfo[i].dtype = DT_CPTR;
    meminfo[i].byval = FALSE;
    meminfo[i].psptr = sptr;
    sz += size_of(DT_CPTR);
    ++i;
  }

  dtype = ll_make_kmpc_struct_type(nmems, NULL, meminfo);

  /* Cleanup */
  for (i = 0; i < nmems; ++i)
    free(meminfo[i].name);
  free(meminfo);
  meminfo = NULL;

  return dtype;
}

/**
   This symbol is used only for its name, if none is found, a unique name is
   generated.
 */
int
llvm_get_unique_sym(void)
{
  return llvmUniqueSym;
}

/**
   Do not call twice for same paralllel region, \c funcCnt is incremented
 */
char *
ll_get_outlined_funcname(int fileno, int lineno)
{
  static char *nm = NULL;
  static unsigned nmLen = 0;
  const unsigned maxDigits = 8 * sizeof(int) / 3;
  unsigned nmSize = (3 * maxDigits) + 4;
  char *sptrnm;
  int unique_sym = llvm_get_unique_sym();
  int r;
  sptrnm = get_ag_name(unique_sym);
  nmSize += strlen(sptrnm);
  if (nmLen < nmSize) {
    if (nm)
      free(nm);
    nm = malloc(nmSize);
    nmLen = nmSize;
  }
  /* side-effect: global funcCnt incremented */
  r = snprintf(nm, nmSize, "%s_%dF%dL%d", sptrnm, funcCnt++, fileno, lineno);
  assert(r < nmSize, "buffer overrun", r, ERR_Fatal);
  return nm;
}

/**
   \p argili is in order
 */
int
ll_make_outlined_garg(int nargs, int *argili, int *arg_dtypes)
{
  int i, gargl = ad1ili(IL_NULL, 0);
  if (arg_dtypes != NULL) {
    for (i = nargs - 1; i >= 0; --i) {
      if (argili[i]) /* Null if this is a varargs ellipsis */ {
        if (arg_dtypes[i] == 0)
          gargl = ad4ili(IL_GARG, argili[i], gargl, DT_CPTR, 0);
        else
          gargl = ad4ili(IL_GARG, argili[i], gargl, arg_dtypes[i], 0);
      }
    }
  } else {
    for (i = nargs - 1; i >= 0; --i)
      if (argili[i]) /* Null if this is a varargs ellipsis */
        gargl = ad4ili(IL_GARG, argili[i], gargl, DT_CPTR, 0);
  }
  return gargl;
}

int
ll_make_outlined_gjsr(int func_sptr, int nargs, int arg1, int arg2, int arg3)
{
  int gjsr;
  int garg;
  int arglist[10];

  arglist[0] = arg1;
  arglist[1] = arg2;
  arglist[2] = arg3;

  garg = ll_make_outlined_garg(3, arglist, NULL);
  gjsr = ad3ili(IL_GJSR, func_sptr, garg, 0);

  return gjsr;
}

int
ll_ad_outlined_func2(int result_opc, int call_opc, int sptr, int nargs,
                     int *args)
{
  int i, arg, rg, argl, ilix;
  int *argsp = args;

  rg = 0;
  argl = ad1ili(IL_NULL, 0);
  for (i = 0; i < nargs; i++) {
    int arg = *argsp++;
    if (!arg) /* If varargs ellipses */
      continue;
    switch (IL_RES(ILI_OPC(arg))) {
    case ILIA_AR:
      argl = ad3ili(IL_ARGAR, arg, argl, 0);
      rg++;
      break;
    case ILIA_IR:
      argl = ad3ili(IL_ARGIR, arg, argl, 0);
      rg++;
      break;
    case ILIA_SP:
      argl = ad3ili(IL_ARGSP, arg, argl, 0);
      rg++;
      break;
    case ILIA_DP:
      argl = ad3ili(IL_ARGDP, arg, argl, 0);
      rg += 2;
      break;
    case ILIA_KR:
      argl = ad3ili(IL_ARGKR, arg, argl, 0);
      rg += 2;
      break;
    default:
      interr("ll_ad_outlined_func2: illegal arg", arg, 3);
      break;
    }
  }

  ilix = ad2ili(call_opc, sptr, argl);
  if (result_opc)
    ilix = genretvalue(ilix, result_opc);

  return ilix;
}

/* right now, the last argument is the uplevel struct */
int
ll_get_shared_arg(int func_sptr)
{
  int paramct, dpdscp, sym;

  paramct = PARAMCTG(func_sptr);
  dpdscp = DPDSCG(func_sptr);

  while (paramct--) {
    sym = aux.dpdsc_base[dpdscp++];
    if (ISTASKDUPG(func_sptr) && paramct == 2)
      break;
  }
  return sym;
}

void
ll_make_ftn_outlined_params(int func_sptr, int paramct, int *argtype)
{
  int count = 0;
  int sym, dtype;
  char name[MXIDLEN + 2];
  int dpdscp = aux.dpdsc_avl;

  PARAMCTP(func_sptr, paramct);
  DPDSCP(func_sptr, dpdscp);
  aux.dpdsc_avl += paramct;
  NEED(aux.dpdsc_avl, aux.dpdsc_base, int, aux.dpdsc_size,
       aux.dpdsc_size + paramct + 100);

  while (paramct--) {
    sprintf(name, "%sArg%d", SYMNAME(func_sptr), count++);
    sym = getsymbol(name);
    SCP(sym, SC_DUMMY);
    if (*argtype == DT_CPTR) { /* either i8* or actual type( pass by value). */
      DTYPEP(sym, DT_INT8);
    } else {
      DTYPEP(sym, *argtype);
      PASSBYVALP(sym, 1);
    }
    argtype++;
    STYPEP(sym, ST_VAR);
    aux.dpdsc_base[dpdscp++] = sym;
  }
}

/* This is a near duplicate of ll_make_ftn_outlined_params but handles by value
 * for fortran.
 */
static void
llMakeFtnOutlinedSignature(int func_sptr, int n_params,
                               const KMPC_ST_TYPE *params)
{
  int i, sym;
  char name[MXIDLEN + 2];
  int count = 0;
  int dpdscp = aux.dpdsc_avl;

  PARAMCTP(func_sptr, n_params);
  DPDSCP(func_sptr, dpdscp);
  aux.dpdsc_avl += n_params;
  NEED(aux.dpdsc_avl, aux.dpdsc_base, int, aux.dpdsc_size,
       aux.dpdsc_size + n_params + 100);

  for (i = 0; i < n_params; ++i) {
    int dtype = params[i].dtype;
    const LOGICAL byval = params[i].byval;

    sprintf(name, "%sArg%d", SYMNAME(func_sptr), count++);
    sym = getsymbol(name);
    SCP(sym, SC_DUMMY);

    if (dtype == DT_CPTR) {
      dtype = DT_INT8;
    }

    DTYPEP(sym, dtype);
    STYPEP(sym, ST_VAR);
    PASSBYVALP(sym, byval);
    aux.dpdsc_base[dpdscp++] = sym;
  }
}

/* Update ACC information such that our OpenACC code generator will be aware of
 * this routine.
 *
 * fnsptr: Function sptr
 */
void
update_acc_with_fn(int fnsptr)
{
}

static int
llGetSym(char *name, int dtype)
{
  int gtid;
  if (!name)
    return 0;
  gtid = getsymbol(name);
  DTYPEP(gtid, dtype);
  SCP(gtid, SC_AUTO);
  ENCLFUNCP(gtid, GBL_CURRFUNC);
  STYPEP(gtid, ST_VAR);
  CCSYMP(gtid,
         1); /* to prevent llassem.c setting it to SC_STATIC for Fortran */
  return gtid;
}

static int
findBihForGtid()
{
  int bih;
  bih = BIH_NEXT(BIHNUMG(GBL_CURRFUNC));
  if (bih == expb.curbih)
    return 0;
  return bih;
}

/* Return the ili representing the global thread id:
 * This value is generated from:
 * 1) Calling the kmpc api directly: kmpc_global_thread_num
 * 2) Using the 1st formal parameter if this is a microtask (i.e., outlined
 * function called by kmpc_fork_call).
 * 3) Using the 1st parameter if this is a task.
 *    where 'this' is gbl.curr_func.
 *
 * * If this is a task, the 1st formal param represents the gtid: i32 gtid.
 * * If this is an outlined func, the 1st formal represents gtid: i32* gtid.
 */
int
ll_get_gtid_val_ili(void)
{
  int ili, nme;
  char *name;

  if (!gtid) {
    name = malloc(strlen(getsname(GBL_CURRFUNC)) + 10);
    sprintf(name, "%s%s", "__gtid_", getsname(GBL_CURRFUNC));
    gtid = llGetSym(name, DT_INT);
    sym_is_refd(gtid);
    free(name);
  }
  ili = ad_acon(gtid, 0);
  nme = addnme(NT_VAR, gtid, 0, 0);
  ili = ad3ili(IL_LD, ili, nme, MSZ_WORD);
  return ili;
}

int
ll_get_gtid_addr_ili(void)
{
  int ili, nme;
  char *name;

  if (!gtid) {
    name = malloc(strlen(getsname(GBL_CURRFUNC)) + 10);
    sprintf(name, "%s%s", "__gtid_", getsname(GBL_CURRFUNC));
    gtid = llGetSym(name, DT_INT);
    sym_is_refd(gtid);
    free(name);
  }
  ili = ad_acon(gtid, 0);
  return ili;
}

static int
llLoadGtid(void)
{
  int ili, nme, rhs;
  int gtid = ll_get_gtid();

  if (!gtid)
    return 0;

  if (gbl.outlined) {
    int arg = ll_get_hostprog_arg(GBL_CURRFUNC, 1);
    int nme = addnme(NT_VAR, arg, 0, 0);
    int ili = ad_acon(arg, 0);
    if (!TASKFNG(GBL_CURRFUNC)) {
      ili = mk_address(arg);
      nme = addnme(NT_VAR, arg, 0, (INT)0);
      arg = mk_argasym(arg);
      ili = ad2ili(IL_LDA, ili, addnme(NT_VAR, arg, 0, (INT)0));
    }
    rhs = ad3ili(IL_LD, ili, nme, MSZ_WORD);
  } else {
    rhs = ll_make_kmpc_global_thread_num();
  }
  ili = ad_acon(gtid, 0);
  nme = addnme(NT_VAR, gtid, 0, 0);
  ili = ad4ili(IL_ST, rhs, ili, nme, MSZ_WORD);
  ASSNP(gtid, 1);

  return ili;
}

int
ll_save_gtid_val(int bih)
{
  int ili;
#ifdef CUDAG
  if (CUDAG(GBL_CURRFUNC) == CUDA_GLOBAL || CUDAG(GBL_CURRFUNC) == CUDA_DEVICE)
    return 0;
#endif

  if (ll_get_gtid()) {
    if (!bih) {
      bih = expb.curbih = BIH_NEXT(BIHNUMG(GBL_CURRFUNC));
    }
    rdilts(bih); /* get block after entry */
    expb.curilt = 0;
    iltb.callfg = 1;
    ili = llLoadGtid();
    if (ili)
      chk_block(ili);
    wrilts(bih);
  }
  return 0;
}

/* Return the uplevel argument from the current function */
int
ll_get_uplevel_arg(void)
{
  int uplevel;

  if (!gbl.outlined && !ISTASKDUPG(GBL_CURRFUNC))
    return 0;

  uplevel = ll_get_shared_arg(GBL_CURRFUNC);
  return uplevel;
}

int
ll_create_task_sptr()
{
  int base = getnewccsym('z', GBL_CURRFUNC, ST_VAR);
  SCP(base, SC_AUTO);
  DTYPEP(base, DT_CPTR);
  return base;
}

int *
ll_make_sections_args(int lbSym, int ubSym, int stSym, int lastSym)
{
  static int args[9];
  int sptr;

  args[8] = genNullArg();          /* i32* ident     */
  args[7] = ll_get_gtid_val_ili();   /* i32 tid        */
  args[6] = ad_icon(KMP_SCH_STATIC); /* i32 schedule   */
  args[5] = ad_acon(lastSym, 0);     /* i32* plastiter */
  args[4] = ad_acon(lbSym, 0);       /* i32* plower    */
  args[3] = ad_acon(ubSym, 0);       /* i32* pupper    */
  args[2] = ad_acon(stSym, 0);       /* i32* pstridr   */
  args[1] = ad_icon(1);              /* i32 incr       */
  args[0] = ad_icon(0);              /* i32 chunk      */
  ADDRTKNP(lbSym, 1);
  ADDRTKNP(ubSym, 1);
  ADDRTKNP(stSym, 1);
  ADDRTKNP(lastSym, 1);
  return args;
}

/* Create the prototype for an outlined function or task.
 * An outlined function is:  void (int32*, int32*, ...);
 * An outlined task is:      int32 (int32, void*);
 *
 * We actually treat these as:
 * An outlined function is:  void (int32*, int32*, void*);
 * An outlined task is:      void (int32, void*); Return is ignored.
 */
static const KMPC_ST_TYPE funcSig[3] = {
    {.dtype = DT_INT, .byval = FALSE},
    {.dtype = DT_CPTR, .byval = FALSE},
    {.dtype = DT_CPTR, .byval = FALSE}, /* Pass ptr directly */
};

static const KMPC_ST_TYPE taskSig[2] = {
    {.dtype = DT_INT, .byval = TRUE},
    {.dtype = DT_CPTR, .byval = FALSE}, /* Pass ptr directly */
};

static const KMPC_ST_TYPE taskdupSig[3] = {
    {.dtype = DT_CPTR, .byval = FALSE},
    {.dtype = DT_CPTR, .byval = FALSE},
    {.dtype = DT_INT, .byval = TRUE}, 
};

static int
makeOutlinedFunc(int stblk_sptr, int scope_sptr, LOGICAL is_task, LOGICAL istaskdup)
{
  char *nm;
  LL_ABI_Info *abi;
  int func_sptr, dtype, ret_dtype, n_args, param1, param2, param3;
  int count = 0;
  const KMPC_ST_TYPE *args;

  /* Get the proper prototype dtypes */
  ret_dtype = DT_VOID_NONE;
  if (is_task) {
    args = taskSig;
    n_args = 2;
  } else if (istaskdup) {
    args = taskdupSig;
    n_args =  3;
  } else {
    args = funcSig;
    n_args = 3;
  }

  if (DBGBIT(45, 0x8) && stblk_sptr)
    dump_parsyms(stblk_sptr);

  /* Create the function sptr */
  nm = ll_get_outlined_funcname(gbl.findex, gbl.lineno);
  func_sptr = getsymbol(nm);
  TASKFNP(func_sptr, is_task);
  ISTASKDUPP(func_sptr, istaskdup);
  OUTLINEDP(func_sptr, scope_sptr);
  FUNCLINEP(func_sptr, gbl.lineno);

/* Set return type and  parameters for function dtype */
  STYPEP(func_sptr, ST_ENTRY);
  DTYPEP(func_sptr, ret_dtype);
  DEFDP(func_sptr, 1);
  SCP(func_sptr, SC_STATIC);
  llMakeFtnOutlinedSignature(func_sptr, n_args, args);
  ADDRTKNP(func_sptr, 1);
  update_acc_with_fn(func_sptr);

  return func_sptr;
}

/* Create function and parameter list for an outlined function.
 * 'stblk_sptr' references the arguments for the function to be outlined.
 */
int
ll_make_outlined_func(int stblk_sptr, int scope_sptr)
{
  return makeOutlinedFunc(stblk_sptr, scope_sptr, FALSE, FALSE);
}

/* Create function and parameter list for an outlined task.
 * 'stblk_sptr' references the arguments for the task to be outlined.
 */
int
ll_make_outlined_task(int stblk_sptr, int scope_sptr)
{
  return makeOutlinedFunc(stblk_sptr, scope_sptr, TRUE, FALSE);
}

static int
llMakeTaskdupRoutine(int task_sptr)
{
  int dupsptr;

  dupsptr = makeOutlinedFunc(0, 0, FALSE, TRUE);
  TASKDUPP(task_sptr, dupsptr);
  TASKDUPP(dupsptr, task_sptr);
  ISTASKDUPP(dupsptr, 1);
  return dupsptr;
}

#ifdef _WIN32
int truncate(const char *path, __int64 length) {
  FILE *f = fopen(   
    &path,  
    "r+"
  );
  _chsize_s(_fileno(f), length);
}
int mkstemp(char * template) {
	return 0;
}
int vasprintf(char **strp, const char *fmt, va_list ap) {
	return 0;
}
#endif

int
ll_reset_parfile(void)
{
  static FILE *orig_ilmfil = 0;
  if (!savedILMFil)
    savedILMFil = gbl.ilmfil;
  if (hasILMRewrite) {
    int i;
    if (gbl.ilmfil == par_file1) {
      gbl.ilmfil = par_file2;
      gbl.eof_flag = 0;
      par_curfile = par_file1;
      truncate(parFileNm1, 0);
      hasILMRewrite = 0;
      (void)fseek(gbl.ilmfil, 0L, 0);
      (void)fseek(par_curfile, 0L, 0);
      return 1;
    } else if (gbl.ilmfil == par_file2) {
      gbl.ilmfil = par_file1;
      gbl.eof_flag = 0;
      par_curfile = par_file2;
      truncate(parFileNm2, 0);
      hasILMRewrite = 0;
      (void)fseek(gbl.ilmfil, 0L, 0);
      (void)fseek(par_curfile, 0L, 0);
      return 1;
    } else {
      gbl.eof_flag = 0;
      orig_ilmfil = gbl.ilmfil;
      gbl.ilmfil = par_file1;
      par_curfile = par_file2;
      hasILMRewrite = 0;
      (void)fseek(gbl.ilmfil, 0L, 0);
      (void)fseek(par_curfile, 0L, 0);
      return 1;
    }
  } else {
    if (orig_ilmfil)
      gbl.ilmfil = orig_ilmfil;
    truncate(parFileNm1, 0);
    truncate(parFileNm2, 0);
    (void)fseek(par_file1, 0L, 0);
    (void)fseek(par_file2, 0L, 0);
    par_curfile = par_file1;
    reset_kmpc_ident_dtype();
    resetThreadprivate();
    return 0;
  }
  return 0;
}

static int
llGetILMLen(int ilmx)
{
  int opcx, len;
  ILM *ilmpx;

  opcx = ILM_OPC(ilmpx = (ILM *)(ilmb.ilm_base + ilmx));
  len = ilms[opcx].oprs + 1;
  if (IM_VAR(opcx))
    len += ILM_OPND(ilmpx, 1); /* include the number of
                                       * variable operands */
  return len;
}

/* collect static variable for Fortran and collect threadprivate for C/C++(need
 * early)*/
static void
llCollectSymbolInfo(ILM *ilmpx)
{
  int flen, len, opnd, sptr, opc, tpv;

  opc = ILM_OPC(ilmpx);
  flen = len = ilms[opc].oprs + 1;
  if (IM_VAR(opc)) {
    len += ILM_OPND(ilmpx, 1); /* include the variable opnds */
  }
  /* is this a variable reference */
  for (opnd = 1; opnd <= flen; ++opnd) {
    if (IM_OPRFLAG(opc, opnd) == OPR_SYM) {
      sptr = ILM_OPND(ilmpx, opnd);
      if (sptr > 0 && sptr < stb.stg_avail) {
        switch (STYPEG(sptr)) {
        case ST_VAR:
        case ST_ARRAY:
        case ST_STRUCT:
        case ST_UNION:
          switch (SCG(sptr)) {
          case SC_AUTO:
            if (!CCSYMG(sptr) && (DINITG(sptr) || SAVEG(sptr))) {
              SCP(sptr, SC_STATIC);
              sym_is_refd(sptr);
            }
            break;
          case SC_STATIC:
            if (!CCSYMG(sptr)) {
              sym_is_refd(sptr);
            }
            break;
          default:;
          }
        default:;
        }
      }
    }
  }
}

int
ll_rewrite_ilms(int lineno, int ilmx, int len)
{
  int nw, i;
  ILM *ilmpx;
  ILM_T nop = IM_NOP;

  if (writeTaskdup) {
    if (len == 0)
      len = llGetILMLen(ilmx);
    ilmpx = (ILM *)(ilmb.ilm_base + ilmx);
    if (ilmx == 0 || pos == 0 || pos < ilmx) {
      pos = ilmx;
      allocTaskdup(len);
      memcpy((TASKDUP_FILE+TASKDUP_AVL), (char*)ilmpx, len*sizeof(ILM_T));
      TASKDUP_AVL += len;
    }
  }

  if (!isRewritingILM) /* only write when this flag is set */
    return 0;

  /* if we are writing to taskdup routine, we are going to
   * write IL_NOP to outlined function.  One reason is that
   * we don't want to evaluate and set when see BMPPG/EMPPG
   */
  if (writeTaskdup) { 
    if (len == 0)
      len = llGetILMLen(ilmx);
    if (ilmx == 0) {
      ilmpx = (ILM *)(ilmb.ilm_base + ilmx);
      nw = fwrite((char *)ilmpx, sizeof(ILM_T), len, par_curfile);
#if DEBUG
#endif
    } else {
      i = ilmx;
      while (len) {
        nw = fwrite((char *)&nop, sizeof(ILM_T), 1, par_curfile);
#if DEBUG
        assert(nw, "error write to temp file in ll_rewrite_ilms", nw, 4);
#endif
        len--;
      };
    }
    return 1;
  }

  if (len == 0) {
    len = llGetILMLen(ilmx);
  }
  ilmpx = (ILM *)(ilmb.ilm_base + ilmx);
  if (!gbl.outlined)
    llCollectSymbolInfo(ilmpx);
  {
    {
      nw = fwrite((char *)ilmpx, sizeof(ILM_T), len, par_curfile);
#if DEBUG
      assert(nw, "error write to temp file in ll_rewrite_ilms", nw, 4);
#endif
    }
  }
    return 1;
}

/*
 * 0 BOS            4     1     6
 * 4 ENTRY        207           ;sub
 *
 * 0 BOS            4     1     5
 * 4 ENLAB
 */

void
ll_write_ilm_header(int outlined_sptr, int curilm)
{
  int nw, len, noplen;
  ILM_T t[6];
  ILM_T t2[6];
  ILM_T t3[4];

  if (!par_curfile)
    par_curfile = par_file1;

  t[0] = IM_BOS;
  t[1] = gbl.lineno;
  t[2] = gbl.findex;
  t[3] = 6;
  t[4] = IM_ENTRY;
  t[5] = outlined_sptr;

  t2[0] = IM_BOS;
  t2[1] = gbl.lineno;
  t2[2] = gbl.findex;
  t2[3] = 5;
  t2[4] = IM_ENLAB;
  t2[5] = 0;

  t3[0] = IM_BOS;
  t3[1] = gbl.lineno;
  t3[2] = gbl.findex;
  t3[3] = ilmb.ilmavl;

  isRewritingILM = 1;
  hasILMRewrite = 1;

  nw = fwrite((char *)t, sizeof(ILM_T), 6, par_curfile);
  nw = fwrite((char *)t2, sizeof(ILM_T), 5, par_curfile);

  len = llGetILMLen(curilm);
  noplen = curilm + len;
  len = ilmb.ilmavl - (curilm + len);
  if (len) {
    nw = fwrite((char *)t3, sizeof(ILM_T), 4, par_curfile);
    llWriteNopILM(gbl.lineno, 0, noplen - 4);
  }
#if DEBUG
#endif
}

/*
 * read outlined ilm header to get outlined function sptr so that we can set
 * gbl.currsub to it.   Fortran check gbl.currsub early in the init.
 */
static int
llReadILMHeader()
{
  int nw, outlined_sptr = 0;
  ILM_T t[6];

  if (!gbl.ilmfil)
    return 0;

  nw = fread((char *)t, sizeof(ILM_T), 6, gbl.ilmfil);

  if (nw)
    outlined_sptr = t[5];

  return outlined_sptr;
}

/*
 * 0 BOS           14     1     5
 * 4 END
 */
void
ll_write_ilm_end(void)
{
  int nw;
  ILM_T t[5];

  t[0] = IM_BOS;
  t[1] = gbl.lineno;
  t[2] = gbl.findex;
  t[3] = 5;
  t[4] = IM_END;

#if DEBUG
#endif

  nw = fwrite((char *)t, sizeof(ILM_T), 5, par_curfile);
  isRewritingILM = 0;
}

void
llWriteNopILM(int lineno, int ilmx, int len)
{
  int nw, i, tlen;
  ILM_T nop = IM_NOP;

  if (writeTaskdup) {
    tlen = len;
    if (tlen == 0)
      tlen = llGetILMLen(ilmx);
    if (tlen)
      allocTaskdup(tlen);
    while (tlen) {
      memcpy((TASKDUP_FILE+TASKDUP_AVL), (char*)&nop, sizeof(ILM_T));
      TASKDUP_AVL += 1;
      tlen--;
    };
  }

  if (!isRewritingILM) /* only write when this flag is set */
    return;

  if (len == 0)
    len = llGetILMLen(ilmx);
  i = ilmx;
  while (len) {
      nw = fwrite((char *)&nop, sizeof(ILM_T), 1, par_curfile);
#if DEBUG
      assert(nw, "error write to temp file in ll_rewrite_ilms", nw, 4);
#endif
    len--;
  };
}

void
ilm_outlined_pad_ilm(int curilm)
{
  int len;
  llWriteNopILM(-1, curilm, 0);
  len = llGetILMLen(curilm);
  len = ilmb.ilmavl - (curilm + len);
  if (len) {
    llWriteNopILM(-1, curilm, len);
  }
}

/* Create a new local uplevel variable and perform a shallow copy of the
 * original uplevel_sptr to the new uplevel sptr.
 */
static int
cloneUplevel(int uplevel_sptr, int uplevel_stblk_sptr)
{
  int ilix, dest_nme;
  static int n;
  const int uplevel_dtype = ll_make_uplevel_type(uplevel_stblk_sptr);
  const int new_uplevel = getnewccsym('D', ++n, ST_STRUCT);
  int count = llmp_get_uplevel(uplevel_stblk_sptr)->vals_count;

  SCP(new_uplevel, SC_LOCAL);
  if (gbl.internal >= 1)
    count = count + 1;
  DTYPEP(new_uplevel, uplevel_dtype);

/* rm_smove will convert SMOVEI into SMOVE.  When doing this
 * rm_smove will remove one ILI so we need to add an ili, so that it is
 * removed when rm_smove executes.
 */
  if (DTYPEG(uplevel_sptr) == DT_ADDR) {
    ilix = ad2ili(IL_LDA, ad_acon(uplevel_sptr, 0),
                  addnme(NT_VAR, uplevel_sptr, 0, 0));
  } else {
    int ili = mk_address(uplevel_sptr);
    int arg = mk_argasym(uplevel_sptr);
    ilix = ad2ili(IL_LDA, ili, addnme(NT_VAR, arg, 0, (INT)0));
  }

    /* set alignment of last argument for GPU "align 8". */
    if (DTY(uplevel_dtype) == TY_STRUCT)
      DTY(uplevel_dtype + 4) = 7;

    if (DTY(DTYPEG(new_uplevel)) == TY_STRUCT)
      DTY(DTYPEG(new_uplevel) + 4) = 7;



  /* For C we have a homed argument, a pointer to a pointer to an uplevel.
   * This will dereference the pointer, we do not need to do this for Fortran.
   */
  ilix = ad2ili(IL_LDA, ilix, 0);

/* For nested tasks: the ilix will reference the task object pointer.
 * So in that case we just loaded the task, and will need to next load the
 * uplevel stored at offset zero in that task object, that is what this load
 * does.
 * For Fortran, we store the uplevel in a temp address(more or less like homing)
 *              so we need to make sure to have another load so that when
 *              rm_smove remove one ILI, it gets to the correct address.
 */
  if (DTYPEG(uplevel_sptr) != DT_ADDR)
    if (TASKFNG(GBL_CURRFUNC)) {
      ilix = ad2ili(IL_LDA, ilix, 0);  /* task[0] */
      ilix = ad2ili(IL_LDA, ilix, 0);  /* *task[0] */
    }

  /* Copy the uplevel to the local version of the uplevel */
  dest_nme = addnme(NT_VAR, new_uplevel, 0, 0);
  ilix = ad4ili(IL_SMOVEI, ilix, ad_acon(new_uplevel, 0),
                count * TARGET_PTRSIZE, dest_nme);
  chk_block(ilix);

  return new_uplevel;
}

static int
loadCharLen(int lensym)
{
  int ilix = mk_address(lensym);
  if (DTYPEG(lensym) == DT_INT8)
    ilix = ad3ili(IL_LDKR, ilix, addnme(NT_VAR, lensym, 0, 0), MSZ_I8);
  else
    ilix = ad3ili(IL_LD, ilix, addnme(NT_VAR, lensym, 0, 0), MSZ_WORD);
  return ilix;
}

/* Generate load instructions to load just the fields of the uplevel table for
 * this function.
 * uplevel:        sptr to the uplevel table for this nest of regions.
 * base:           Base index into aux table.
 * count:          Number of sptrs to consecutively store in uplevel.
 *
 * Returns the ili for the sequence of store ilis.
 */
static int
loadUplevelArgsForRegion(int uplevel, int base, int count,
                             int uplevel_stblk_sptr)
{
  int i, addr, ilix, offset, val, nme, encl, based, lensptr;
  LOGICAL do_load, byval;
  const LLUplevel *up = count ? llmp_get_uplevel(uplevel_stblk_sptr) : NULL;

  offset = 0;
  nme = addnme(NT_VAR, uplevel, 0, 0);
  /* load display argument from host routine */
  if (gbl.internal >= 1) {
    int sptr = aux.curr_entry->display;
    if (gbl.outlined) {
      ADDRTKNP(sptr, 1);
      val = mk_address(sptr);
      val = ad2ili(IL_LDA, val, addnme(NT_VAR, sptr, 0, (INT)0));
    } else if (gbl.internal == 1) {
      ADDRTKNP(sptr, 1);
      val = mk_address(sptr);
    } else {
      sptr = mk_argasym(sptr);
      val = mk_address(sptr);
      val = ad2ili(IL_LDA, val, addnme(NT_VAR, sptr, 0, (INT)0));
    }
    addr = ad_acon(uplevel, offset);
    ilix = ad4ili(IL_STA, val, addr, nme, MSZ_PTR);
    chk_block(ilix);
    offset += size_of(DT_CPTR);
  }

  if (up)
    count = up->vals_count;

  lensptr = 0;
  byval = 0;
  for (i = 0; i < count; ++i) {
    int sptr = up->vals[i];

    based = 0;
    if (!sptr && !lensptr) {
      /* We put a placeholder in the front end for character
       * len(CLENG) after its character sptr for assumed len
       * or deferred char because CLENG may not be set
       * until later in the backend.  We shouldn't have a
       * problem for fixed len char because we
       * can get its len from DTY(dtype+1).
       */

      offset += size_of(DT_CPTR);
      continue;
    }

/* Load the uplevel pointer and get the offset where the pointer to the
 * member should be placed.
 */

    if (!lensptr &&
        (DT_ASSNCHAR == DDTG(DTYPEG(sptr)) ||
         DT_ASSCHAR == DDTG(DTYPEG(sptr)) ||
         DT_DEFERNCHAR == DDTG(DTYPEG(sptr)) ||
         DT_DEFERCHAR == DDTG(DTYPEG(sptr)) || DTY(DTYPEG(sptr)) == TY_CHAR)) {
      lensptr = CLENG(sptr);
    }

    if (lensptr && !sptr) {
      val = loadCharLen(lensptr);
      byval = 1;
      sptr = lensptr;
    } else if (SCG(sptr) == SC_DUMMY) {
      int asym = mk_argasym(sptr);
      int anme = addnme(NT_VAR, asym, 0, (INT)0);
      val = mk_address(sptr);
      val = ad2ili(IL_LDA, val, anme);

    } else if (SCG(sptr) == SC_BASED && MIDNUMG(sptr)) {
      /* for adjustable len char the $p does not have
       * clen field so we need to reference it from
       * the SC_BASED
       */
      based = sptr;
      sptr = MIDNUMG(sptr);
      val = mk_address(sptr);
#if DO_NOT_DUPLICATE_LOAD_THEN_FIX_ME
      offset += size_of(DT_CPTR);
      continue;
#endif
      if (SCG(sptr) == SC_DUMMY) {
        int asym = mk_argasym(sptr);
        int anme = addnme(NT_VAR, asym, 0, (INT)0);
        val = mk_address(sptr);
        val = ad2ili(IL_LDA, val, anme);
      }
    } else
        if (THREADG(sptr)) {
      /*
       * special handle for copyin threadprivate var - we put it in uplevel
       * structure
       * so that we get master threadprivate copy and pass down to its team.
       */
      int sym = getThreadPrivateTp(sptr);
      val = llGetThreadprivateAddr(sym);
    } else
      val = mk_address(sptr);
    nme = addnme(NT_VAR, uplevel, 0, 0);
    if (TASKFNG(GBL_CURRFUNC) && DTYPEG(uplevel) == DT_ADDR) {
      ilix = ad_acon(uplevel, 0);
      addr = ad2ili(IL_LDA, ilix, nme);
    } else {
      addr = ad_acon(uplevel, offset);
    }
    /* Skip non-openmp ST_BLOCKS stop at closest one (uplevel is set) */
    encl = ENCLFUNCG(sptr);
    if (STYPEG(encl) != ST_ENTRY && STYPEG(encl) != ST_PROC) {
      while (encl && ((STYPEG(ENCLFUNCG(encl)) != ST_ENTRY) ||
                      (STYPEG(ENCLFUNCG(encl)) != ST_PROC))) {
        if (PARUPLEVELG(encl)) /* Only OpenMP blocks use this */
          break;
        encl = ENCLFUNCG(encl);
      }
    }

    /* Private and encl is an omp block not expanded, then do not load */
    if (encl && PARUPLEVELG(encl) && SCG(sptr) == SC_PRIVATE &&
        (STYPEG(encl) == ST_BLOCK)) {
      if (!PARENCLFUNCG(encl)) {
        offset += size_of(DT_CPTR);
        continue;
      } else {
        if ((STYPEG(ENCLFUNCG(encl)) != ST_ENTRY))
        {
          offset += size_of(DT_CPTR);
          lensptr = 0;
          continue;
        }
      }
    }
    /* Determine if we should call a store */
    do_load = FALSE;
    if (THREADG(sptr))
      do_load = TRUE;
    else if (!gbl.outlined && SCG(sptr) != SC_PRIVATE) {
      do_load = TRUE; /* Non-private before outlined func - always load */
      sym_is_refd(sptr);
      if (SCG(sptr) == SC_STATIC) {
        if (based)
          ADDRTKNP(based, 1);
        else
          ADDRTKNP(sptr, 1);
        offset += size_of(DT_CPTR);
        continue;
      }
    } else if (gbl.outlined && is_llvm_local_private(sptr))
      do_load = TRUE;

    if (do_load) {
      if (based) {
        PARREFLOADP(based, 1);
        ADDRTKNP(based, 1);
      } else
      {
        PARREFLOADP(sptr, 1);
        /* prevent optimizer to remove store instruction */
        ADDRTKNP(sptr, 1);
      }
      if (lensptr && byval) {
        if (CHARLEN_64BIT) {
          val = sel_iconv(val, 1);
          ilix = ad4ili(IL_STKR, val, addr, nme, MSZ_I8);
        } else {
          val = sel_iconv(val, 0);
          ilix = ad4ili(IL_ST, val, addr, nme, MSZ_WORD);
        }
        lensptr = 0;
        byval = 0;
      } else {
        ilix = ad4ili(IL_STA, val, addr, nme, MSZ_PTR);
      }
      chk_block(ilix);
    }
    offset += size_of(DT_CPTR);
  }

  return ad_acon(uplevel, 0);
}

/* Either:
 *
 * 1) Create an instance of the uplevel argument for the outlined call that
 * expects scope_blk_sptr.
 *
 * 2) Create the uplevel table and pass that as an arg.
 *
 */
int
ll_load_outlined_args(int scope_blk_sptr, int callee_sptr, LOGICAL clone)
{
  LLUplevel *up;
  int uplevel_dtype, uplevel, base, count, addr, val, ilix, newcount;
  const int uplevel_blk_sptr = PARUPLEVELG(scope_blk_sptr);
  static int n;

  /* If this is not the parent for a nest of funcs just return uplevel tbl ptr
   * which was passed to this function as arg3.
   */
  base = 0;
  count = PARSYMSG(uplevel_blk_sptr)
              ? llmp_get_uplevel(uplevel_blk_sptr)->vals_count
              : 0;
  newcount = count;
  if (gbl.internal >= 1) {
    if (count == 0 && PARSYMSG(uplevel_blk_sptr) == 0) {
      const int key = llmp_get_next_key();
      LLUplevel *up = llmp_create_uplevel_bykey(key);
      PARSYMSP(uplevel_blk_sptr, key);
    }
    newcount = count + 1;
  }

  if (gbl.outlined) {
    uplevelSym = uplevel = aux.curr_entry->uplevel;
    ll_process_routine_parameters(callee_sptr);
    sym_is_refd(callee_sptr);
    /* Clone: See comment in this function's description above. */
    if (newcount) {
      if (clone)
        uplevel = cloneUplevel(uplevel, uplevel_blk_sptr);
      uplevelSym = uplevel;
    }
  } else { /* Else: is the parent and we need to create an uplevel table */
    if (newcount == 0) { /* No items to pass via uplevel, just pass null  */
      ll_process_routine_parameters(callee_sptr);
      return ad_aconi(0);
    }

    /* Create an uplevel instance and give it a custom struct type */
    uplevel_dtype = ll_make_uplevel_type(uplevel_blk_sptr);
    uplevelSym = uplevel = getnewccsym('M', ++n, ST_STRUCT);

    /* Set the uplevel dtype:
     * The uplevel will be presented as the third argument to an outlined
     * function: i8*, i8*, i8*.  However, ll_get_uplevel_offset will use the
     * actual dtype (uplevel_dtype) to obtain the proper offset of a field.
     */
    up = llmp_get_uplevel(uplevel_blk_sptr);
    llmp_uplevel_set_dtype(up, uplevel_dtype);

    SCP(uplevel, SC_LOCAL);
    REFP(uplevel, 1); /* don't want it to go in sym_is_refd */

    DTYPEP(uplevel, uplevel_dtype);

/* Align uplevel for GPU "align 8" */
    ll_process_routine_parameters(callee_sptr);
    sym_is_refd(callee_sptr);
    /* set alignment of last argument for GPU "align 8". It may not be the same
     * as uplevel if this is task */
    if (DTY(uplevel_dtype) == TY_STRUCT)
      DTY(uplevel_dtype + 4) = 7;

    if (DTY(DTYPEG(uplevel)) == TY_STRUCT)
      DTY(DTYPEG(uplevel) + 4) = 7;

    /* Debug */
    if (DBGBIT(45, 0x8))
      dumpUplevel(uplevel);
  }

  ilix =
      loadUplevelArgsForRegion(uplevel, base, newcount, uplevel_blk_sptr);
  if (TASKFNG(GBL_CURRFUNC) && DTYPEG(uplevel) == DT_ADDR)
    ilix = ad2ili(IL_LDA, ilix, addnme(NT_VAR, uplevel, 0, 0));

  return ilix;
}

int
ll_get_uplevel_offset(int sptr)
{
  int dtype, mem;
  if (gbl.outlined || ISTASKDUPG(GBL_CURRFUNC)) {
    int scope_sptr;
    int uplevel_stblk;
    LLUplevel *uplevel;
    
    if (ISTASKDUPG(GBL_CURRFUNC)) {
      scope_sptr = OUTLINEDG(TASKDUPG(GBL_CURRFUNC));
    } else {
      scope_sptr = OUTLINEDG(GBL_CURRFUNC);
    }
    uplevel_stblk = PARUPLEVELG(scope_sptr);
    uplevel = llmp_get_uplevel(uplevel_stblk);

    dtype = uplevel->dtype;
    for (mem = DTY(dtype + 1); mem > 1; mem = SYMLKG(mem))
      if (PAROFFSETG(mem) == sptr)
        return ADDRESSG(mem);
  }

  return ADDRESSG(sptr);
}

int
ll_make_outlined_call(int func_sptr, int arg1, int arg2, int arg3)
{
  int i, ilix, altili, argili;
  int nargs = 3;
  char *funcname = SYMNAME(func_sptr);

  argili = ad_aconi((ISZ_T)0);
  ilix = ll_ad_outlined_func(0, IL_JSR, funcname, nargs, argili, argili, arg3);

  altili = ll_make_outlined_gjsr(func_sptr, nargs, argili, argili, arg3);
  ILI_ALT(ilix) = altili;

  return ilix;
}

/* whicharg starts from 1 to narg - 1 */
int
ll_get_hostprog_arg(int func_sptr, int whicharg)
{
  int paramct, dpdscp, sym, uplevel, i, dtype;

  paramct = PARAMCTG(func_sptr);
  dpdscp = DPDSCG(func_sptr);

  sym = aux.dpdsc_base[dpdscp + (whicharg - 1)];
  return sym;

}

int
ll_make_outlined_call2(int func_sptr, int uplevel_ili)
{
  int i, ilix, altili, argili;
  int nargs = 3;
  int arg1, arg2, arg3, args[3];
  static int n;

  if (!gbl.outlined) {
    /* orphaned outlined function */
    arg1 = args[2] = ll_get_gtid_addr_ili();
    arg2 = args[1] = genNullArg();
    arg3 = args[0] = uplevel_ili;
  } else {
    /* The first and second arguments are from host program */
    arg1 = ll_get_hostprog_arg(GBL_CURRFUNC, 1);
    arg2 = ll_get_hostprog_arg(GBL_CURRFUNC, 2);
    arg3 = args[0] = uplevel_ili;
    arg1 = args[2] = mk_address(arg1);
    arg2 = args[1] = mk_address(arg2);
  }

  ilix = ll_ad_outlined_func2(0, IL_JSR, func_sptr, nargs, args);

  altili = ll_make_outlined_gjsr(func_sptr, nargs, arg1, arg2, arg3);
  ILI_ALT(ilix) = altili;

  return ilix;
}

/* Call an outlined task.
 * func_sptr: Outlined function representing a task.
 * task_sptr: Allocated kmpc task struct
 */
int
ll_make_outlined_task_call(int func_sptr, int task_sptr)
{
  int altili, ilix;
  int arg1, arg2, args[2] = {0};

  arg1 = args[1] = ad_icon(0);
  arg2 = args[0] = ad2ili(IL_LDA, ad_acon(task_sptr, 0),
                          addnme(NT_VAR, task_sptr, 0, (INT)0));
  ilix = ll_ad_outlined_func2(0, IL_JSR, func_sptr, 2, args);

  altili = ll_make_outlined_gjsr(func_sptr, 2, arg1, arg2, 0);
  ILI_ALT(ilix) = altili;

  return ilix;
}

void
llvm_set_unique_sym(int sptr)
{
  if (!llvmUniqueSym) { /* once set - don't overwrite it */
    llvmUniqueSym = sptr;
  }
}

void
ll_set_outlined_currsub()
{
  int scope_sptr;
  long gilmpos = ftell(gbl.ilmfil);
  gbl.currsub = llReadILMHeader();
  scope_sptr = OUTLINEDG(gbl.currsub);
  if (scope_sptr && gbl.currsub)
    ENCLFUNCP(scope_sptr, PARENCLFUNCG(scope_sptr));
  gbl.rutype = RU_SUBR;
  (void)fseek(gbl.ilmfil, gilmpos, 0);
}

/* should be call when the host program is done */
static void
resetThreadprivate()
{
  int sym, next_tp;
  for (sym = gbl.threadprivate; sym > NOSYM; sym = next_tp) {
    next_tp = TPLNKG(sym);
    TPLNKP(sym, 0);
  }
  gbl.threadprivate = NOSYM;
}

int
ll_get_gtid()
{
  return gtid;
}

void
ll_reset_gtid()
{
  gtid = 0;
}

void
ll_reset_outlined_func()
{
  uplevelSym = 0;
}

int
ll_get_uplevel_sym()
{
  return uplevelSym;
}

static void
llRestoreSavedILFil()
{
  if (savedILMFil)
    gbl.ilmfil = savedILMFil;
}

void
ll_open_parfiles()
{
  int fd1, fd2;
  strcpy(parFileNm1, "pgipar1XXXXXX");
  strcpy(parFileNm2, "pgipar2XXXXXX");
  fd1 = mkstemp(parFileNm1);
  fd2 = mkstemp(parFileNm2);
  par_file1 = fdopen(fd1, "w+");
  par_file2 = fdopen(fd2, "w+");
  if (!par_file1)
    errfatal(4);
  if (!par_file2)
    errfatal(4);
}

void
ll_unlink_parfiles()
{
  llRestoreSavedILFil();
  if (par_file1)
    unlink(parFileNm1);
  if (par_file2)
    unlink(parFileNm2);
  par_file1 = NULL;
  par_file2 = NULL;
}

/* START: OUTLINING MCONCUR */
void
llvmSetExpbCurIlt()
{
  expb.curilt = ILT_PREV(0);
}

int
llvmGetExpbCurIlt()
{
  return expb.curilt;
}

int
llvmAddConcurEntryBlk(int bih)
{
  int newbih, funcsptr, arg1, arg2, arg3, symdtype;
  int display_temp, asym, ili_uplevel, nme, ili;
  funcsptr = GBL_CURRFUNC;
  display_temp = 0;

  /* add entry block */
  newbih = addnewbih(bih, bih, bih);
  rdilts(newbih);
  expb.curbih = newbih;
  BIHNUMP(GBL_CURRFUNC, expb.curbih);
  expb.curilt = addilt(ILT_PREV(0), ad1ili(IL_ENTRY, GBL_CURRFUNC));
  wrilts(newbih);
  BIH_LABEL(newbih) = GBL_CURRFUNC;
  BIH_EN(newbih) = 1;

  gbl.outlined = 1;
  gbl.entbih = newbih;

  reset_kmpc_ident_dtype();

  reg_init(GBL_CURRFUNC);

  aux.curr_entry->uplevel = ll_get_shared_arg(GBL_CURRFUNC);
  asym = mk_argasym(aux.curr_entry->uplevel);
  ADDRESSP(asym, ADDRESSG(aux.curr_entry->uplevel)); /* propagate ADDRESS */
  MEMARGP(asym, 1);

  if (gbl.internal > 1) {
    rdilts(newbih);
    display_temp = getccsym('S', gbl.currsub, ST_VAR);
    SCP(display_temp, SC_PRIVATE);
    ENCLFUNCP(display_temp, GBL_CURRFUNC);
    DTYPEP(display_temp, DT_ADDR);
    sym_is_refd(display_temp);

    ili = ad_acon(display_temp, 0);
    nme = addnme(NT_VAR, display_temp, (INT)0, 0);

    ili_uplevel = mk_address(aux.curr_entry->uplevel);
    nme = addnme(NT_VAR, aux.curr_entry->uplevel, (INT)0, 0);
    ili_uplevel = ad2ili(IL_LDA, ili_uplevel, nme);
    ili_uplevel =
        ad2ili(IL_LDA, ili_uplevel, addnme(NT_IND, display_temp, nme, 0));

    ili = ad2ili(IL_LDA, ili, addnme(NT_IND, display_temp, nme, 0));
    nme = addnme(NT_VAR, display_temp, (INT)0, 0);
    ili = ad3ili(IL_STA, ili_uplevel, ili, nme);
    expb.curilt = addilt(expb.curilt, ili);
    wrilts(newbih);

    flg.recursive = TRUE;
  }

  newbih = addnewbih(bih, bih,
                     bih); /* add empty block  - make entry block separate */
  return display_temp;
}

void
llvmAddConcurExitBlk(int bih)
{
  int newbih;

  newbih = addnewbih(BIH_PREV(bih), bih, bih);
  rdilts(newbih);
  expb.curbih = newbih;
  expb.curilt = addilt(ILT_PREV(0), ad1ili(IL_EXIT, GBL_CURRFUNC));
  wrilts(newbih);
  BIH_XT(newbih) = 1;
  BIH_LAST(newbih) = 1;
  BIH_FT(newbih) = 0;
  expb.arglist = 0;
  expb.flags.bits.callfg = 0;
  mkrtemp_end();
}

/* END: OUTLINING MCONCUR */

/* START: TASKDUP(kmp_task_t* task, kmp_task_t* newtask, int lastitr)
 * write all ilms between IM_BTASKLOOP and IM_ETASKLOOP
 * to a taskdup routine.  Mostly use for firstprivate and
 * last iteration variables copy/constructor.
 * writeTaskdup is set when we see IM_BTASKLOOP and unset when 
 * we see IM_TASKLOOPREG. It then will be set again after IM_ETASKLOOPREG
 * until IM_ETASKLOOP(C/C++ may have firstprivate initialization
 * later).  Currently only private data allocation & initialization 
 * are expected in those ilms.  In future, if there are other ilms
 * in the mix, the we may need to provide some delimits to mark
 * where to start write and end.
 */
 
void
start_taskdup(int task_fnsptr, int curilm)
{
  int nw, len, noplen;
  ILM_T t[6], t2[6], t3[6];
  writeTaskdup = TRUE;
  t3[0] = IM_BOS;
  t3[1] = gbl.lineno;
  t3[2] = gbl.findex;
  t3[3] = ilmb.ilmavl;
  if (!TASKDUPG(task_fnsptr)) {
    int dupsptr = llMakeTaskdupRoutine(task_fnsptr);
    ILM_T t[6];
    ILM_T t2[6];
    ILM_T t3[4];

    t[0] = IM_BOS;
    t[1] = gbl.lineno;
    t[2] = gbl.findex;
    t[3] = 6;
    t[4] = IM_ENTRY;
    t[5] = dupsptr;

    t2[0] = IM_BOS;
    t2[1] = gbl.lineno;
    t2[2] = gbl.findex;
    t2[3] = 5;
    t2[4] = IM_ENLAB;
    t2[5] = 0;

    allocTaskdup(6);
    memcpy((TASKDUP_FILE+TASKDUP_AVL), (char*)t, 6 * sizeof(ILM_T));
    TASKDUP_AVL += 6;

    allocTaskdup(5);
    memcpy((TASKDUP_FILE+TASKDUP_AVL), (char*)t2, 5 * sizeof(ILM_T));
    TASKDUP_AVL += 5;
  }
  pos = 0;
  len = llGetILMLen(curilm);
  noplen = curilm + len;
  len = ilmb.ilmavl - (curilm + len);
  if (len) {
    allocTaskdup(4);
    memcpy((TASKDUP_FILE+TASKDUP_AVL), (char*)t3, 4 * sizeof(ILM_T));
    TASKDUP_AVL += 4;
    llWriteNopILM(gbl.lineno, 0, noplen - 4);
  }
}

void
restartRewritingILM(int curilm)
{
  int len, noplen, nw;
  ILM_T t[6];

  t[0] = IM_BOS;
  t[1] = gbl.lineno;
  t[2] = gbl.findex;
  t[3] = ilmb.ilmavl;
  pos = 0;
  len = llGetILMLen(curilm);
  noplen = curilm + len;
  len = ilmb.ilmavl - (curilm + len);
  setRewritingILM();
  if (len) {
    nw = fwrite((char *)t, sizeof(ILM_T), 4, par_curfile);
#if DEBUG
#endif
    llWriteNopILM(gbl.lineno, 0, noplen - 4);
  }
}

void
stop_taskdup(int task_fnsptr, int curilm)
{
  /* write IL_NOP until the end of ILM blocks */
  ilm_outlined_pad_ilm(curilm);
  writeTaskdup = FALSE;
  pos = 0;
}

static void
clearTaskdup()
{
  FREE(TASKDUP_FILE);
  TASKDUP_AVL = 0;
  TASKDUP_SZ = 0;
  TASKDUP_FILE = NULL;
}

static void
copyLastItr(int fnsptr, INT offset)
{
  ILM_T* ptr;
  int offset_sptr;
  int total_ilms = 0;
  INT tmp[2];
  tmp[0] = 0;
  tmp[1] = offset;
  offset_sptr = getcon(tmp, DT_INT);

  allocTaskdup(6);
  ptr = (TASKDUP_FILE + TASKDUP_AVL);

  *ptr++ = IM_BOS;
  *ptr++ = gbl.lineno;
  *ptr++ = gbl.findex;
  *ptr++ = 6;
  total_ilms = total_ilms + 4;

  *ptr++ = IM_TASKLASTPRIV;
  *ptr++ = offset_sptr;
  total_ilms = total_ilms + 2;

  TASKDUP_AVL += total_ilms;
}

void
finish_taskdup_routine(int curilm, int fnsptr, INT offset)
{
  int nw, len;
  ILM_T t[5];
  ILM_T nop = IM_NOP;

  if (!TASKDUP_AVL)
    return;

  t[0] = IM_BOS;
  t[1] = gbl.lineno;
  t[2] = gbl.findex;
  t[3] = 5;
  t[4] = IM_END;
  if (offset) { 
    copyLastItr(fnsptr, offset);
  }
  /* write taskdup ilms to file */
  if (TASKDUP_AVL) {
    allocTaskdup(5);
    memcpy((TASKDUP_FILE+TASKDUP_AVL), (char*)t, 5 * sizeof(ILM_T));
    TASKDUP_AVL += 5;
 
    nw = fwrite((char*)TASKDUP_FILE, sizeof(ILM_T), TASKDUP_AVL, par_curfile);
#ifdef DEBUG
#endif
  }
  clearTaskdup();
  writeTaskdup = FALSE;
  hasILMRewrite = 1;
  pos = 0;
}


static void
allocTaskdup(int len)
{
  NEED((TASKDUP_AVL+len+20), TASKDUP_FILE, ILM_T, TASKDUP_SZ, 
       (TASKDUP_AVL+len+20));
}

/* END: TASKDUP routine */

void
unsetRewritingILM()
{
  isRewritingILM = 0;
}

void
setRewritingILM()
{
  isRewritingILM = 1;
}

LOGICAL
ll_ilm_is_rewriting(void)
{
  return isRewritingILM;
}

int
ll_has_more_outlined()
{
  return hasILMRewrite;
}

int
llvm_ilms_rewrite_mode(void)
{
  if (gbl.ilmfil == par_file1 || gbl.ilmfil == par_file2)
    return 1;
  return 0;
}


/* used by Fortran only.  If gbl.ilmfil points to tempfile, then
 * we are processing ILMs in that file.  This function is called
 * after we emit we call schedule of current function and we are
 * trying to decide if we should continue processing the current
 * file or the next tempfile.
 */
int
llProcessNextTmpfile()
{
  if (gbl.ilmfil == par_file1 || gbl.ilmfil == par_file2)
    return 0;
  return hasILMRewrite;

}
