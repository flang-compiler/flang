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
 * \brief SMP expander routines
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
#include "kmpcutil.h"
#include "outliner.h"
#include "mp.h"
#include "x86.h"
#include "assem.h"
#include "llutil.h"
#include "llassem.h"
#include "llmputil.h"

static int jsr_add_arg(int, int, int);
static int make_call_result(int, int);
static int make_call(char *, int, int);
extern int lcpu_temp(int);
extern int ncpus_temp(int);
static int gen_int_load(int);
static int gen_int_store(int, int);
static void incr_par_cnt(void);
static void decr_ll_par_cnt(void);
static void incr_ll_par_cnt(void);
static int get_parrgn_temp(char *, int);
static int is_unnamed_cs(int);
static int add_mp_unp(void);
static int add_mp_unv(void);
static int add_mp_bcs_nest(void);
static int add_mp_ecs_nest(void);
static int alloc_threadprivate(int, int *);
static int get_elem_size(int);
static int get_num_sect(int *);
static int ll_get_private_temp(int);

#define mk_prototype mk_prototype_llvm

static int avail_ireg; /* next available integer register for jsr */
static int avail_freg; /* next available floating point register for jsr */
static int max_ireg;   /* max # of integer registers used by jsr */
static int max_freg;   /* max # of floating point registers used by jsr */

static int
    ll_par_cnt; /* counter to keep track of begin and end of parallel region */
static int par_cnt;         /* counter to record parallel regions */
static int parsect_cnt;     /* counter to record parallel sections */
static int crit_cnt;        /* counter for critical sections */
static int task_cnt;        /* counter for task regions  */
static int task_lab;        /* label after ETASK */
static int task_bv;         /* bit values for flag for BTASK & TASKREG:
                             *   0x01 -- untied
                             *   0x02 -- if clause present
                             *   0x04 -- orphaned (dynamic, not lexically, parallel)
                             *   0x08 -- nested task
                             *   0x10 -- forced defer (CUDA)
                             *   0x20 -- final task
                             *   0x40 -- execute immediately
                             */
static int task_dup;
static int task_ifv;        /* value of if clause for BTASK & TASKREG */
static int task_flags;      /* value of final clause for BTASK & TASKREG */
static int task_fnsptr;     /* store task func sptr */
static int task_alloc_sptr; /* store the return value from kmpc_alloc */
static int pparbih;    /* prologue bih of the current parallel region */
static int max_par_cnt = 0; /* maximum par_cnt for a function */
static int sum_par_cnt = 0; /* sum of par_cnts of functions already
                             * processed.  'sum_par_cnt+par_cnt' can be
                             * the suffix of the name of a temp created for
                             * a parallel region within a function so that
                             * the temp is:
                             * 1) unique across functions,
                             * 2) reused across parallel regions within
                             *    a function.
                             */
static int *bpard_sym; /* temp/const representing the 'inhibit' BPARD value */
static int bpard_siz;  /* size of bpard sym table */
static int scope_sptr;

static int *mppgbih;
static int mppgcnt;
static int mppgbih_siz;

static int *ptaskbih; /* prologue bih of the current task region */
static int ptaskbih_siz;
static struct {
  int lb_ili;
  int ub_ili;
  int st_ili;
  int lastitr;
  int flags;
  INT offset;
  int tasklpargs[10];  /* ili in order as enum tasklooparg below */
} tasklp_info;

enum taskloooparg
{
  TASKLPARG_TASK = 0,
  TASKLPARG_IF_VAL,
  TASKLPARG_LB,
  TASKLPARG_UB,
  TASKLPARG_ST,
  TASKLPARG_NOGROUP,
  TASKLPARG_SCHED,
  TASKLPARG_GRAINSIZE,
  TASKLPARG_TASKDUP,
  TASKLPARG_MAX
};

#define TASK_LB tasklp_info.lb_ili
#define TASK_LPVAR_OFFSET tasklp_info.offset
#define TASK_UB tasklp_info.ub_ili
#define TASK_ST tasklp_info.st_ili
#define TASK_LASTITR tasklp_info.lastitr

/* arguments to __kmpc_taskloop excepts ident and gtid */
#define TASKLPARGS tasklp_info.tasklpargs
#define TASKLP_TASK tasklp_info.tasklpargs[TASKLPARG_TASK]
#define TASKLP_IF tasklp_info.tasklpargs[TASKLPARG_IF_VAL]
#define TASKLP_LB tasklp_info.tasklpargs[TASKLPARG_LB]
#define TASKLP_UB tasklp_info.tasklpargs[TASKLPARG_UB]
#define TASKLP_ST tasklp_info.tasklpargs[TASKLPARG_ST]
#define TASKLP_NOGROUP tasklp_info.tasklpargs[TASKLPARG_NOGROUP]
#define TASKLP_SCHED tasklp_info.tasklpargs[TASKLPARG_SCHED]
#define TASKLP_GRAINSIZE tasklp_info.tasklpargs[TASKLPARG_GRAINSIZE]
#define TASKLP_TASKDUP tasklp_info.tasklpargs[TASKLPARG_TASKDUP]

static struct {
  int lb;   /* start at 0 */
  int ub;   /* number of sections */
  int st;   /* stride 1 */
  int last; /* flag for last section */
  int cnt;  /* running count */
  int bbih; /* start block for sections */
} sections_wrk = {0};

#define SECT_LB sections_wrk.lb
#define SECT_UB sections_wrk.ub
#define SECT_ST sections_wrk.st
#define SECT_LAST sections_wrk.last
#define SECT_CNT sections_wrk.cnt
#define SECT_BBIH sections_wrk.bbih

#define MP_NOT_IMPLEMENTED(_str) error(375, ERR_Fatal, 0, _str, NULL)

/* For use with generating an array filed with copyprivate addresses.
 * 'sptr' is either the base sptr or the TPpxxx thread private common block
 * vector.
 */
typedef struct _sptr_list_t {
  int o_sptr;
  int sptr;
  int size_ili;
  int vec_size_ili;
  LOGICAL is_common_block;
  struct _sptr_list_t *next;
  int cplus_assign_rou;
} sptr_list_t;

/* called once per function */
void
exp_smp_init(void)
{
  par_cnt = 0;
  parsect_cnt = 0;
  ll_par_cnt = 0;
  crit_cnt = 0;
  expb.lcpu2 = 0;
  expb.lcpu3 = 0;
  expb.ncpus2 = 0;
  max_par_cnt = 0;
  bpard_siz = 16;
  NEW(bpard_sym, int, bpard_siz);
  mppgbih_siz = 16;
  NEW(mppgbih, int, mppgbih_siz);
  mppgcnt = 0;
  ptaskbih_siz = 16;
  NEW(ptaskbih, int, ptaskbih_siz);
  task_cnt = 0;
}

void
exp_smp_fini(void)
{
  sum_par_cnt = +max_par_cnt;
  FREE(bpard_sym);
  FREE(mppgbih);
  FREE(ptaskbih);
}

static void
exp_smp_section_init()
{
  SECT_LB = ll_get_private_temp(DT_UINT);
  SECT_UB = ll_get_private_temp(DT_UINT);
  SECT_LAST = ll_get_private_temp(DT_UINT);
  SECT_ST = ll_get_private_temp(DT_UINT);
  SECT_CNT = 0;
  if (!gbl.outlined) {
    SCP(SECT_LB, SC_AUTO);
    SCP(SECT_UB, SC_AUTO);
    SCP(SECT_LAST, SC_AUTO);
    SCP(SECT_ST, SC_AUTO);
  }
  SECT_BBIH = expb.curbih;
}

static void
exp_smp_section_end()
{
  SECT_LB = ll_get_private_temp(DT_UINT);
  SECT_UB = ll_get_private_temp(DT_UINT);
  SECT_LAST = ll_get_private_temp(DT_UINT);
  SECT_ST = ll_get_private_temp(DT_UINT);
  SECT_CNT = 0;
  SECT_BBIH = 0;
}

static int
section_create_block(int nextLabel, int lb, int ub, int myVal)
{
  int ili, ubVal, lbVal;

  myVal = ad_icon(myVal);
  lbVal = ad3ili(IL_LD, ad_acon(lb, 0), addnme(NT_VAR, lb, 0, 0), MSZ_WORD);
  ubVal = ad3ili(IL_LD, ad_acon(ub, 0), addnme(NT_VAR, ub, 0, 0), MSZ_WORD);

  ili = ad4ili(IL_UICJMP, myVal, ubVal, CC_GT, nextLabel);
  RFCNTI(nextLabel);
  chk_block(ili);

  ili = ad4ili(IL_UICJMP, myVal, lbVal, CC_LT, nextLabel);
  RFCNTI(nextLabel);

  return ili;
}

static int
section_create_lastblock(int nextLabel, int lastValSym, int myVal)
{
  int ili, lastVal;

  myVal = ad_icon(myVal);
  lastVal = ad3ili(IL_LD, ad_acon(lastValSym, 0),
                   addnme(NT_VAR, lastValSym, 0, 0), MSZ_WORD);

  ili = ad4ili(IL_UICJMP, myVal, lastVal, CC_EQ, nextLabel);
  RFCNTI(nextLabel);
  return ili;
}
void
section_create_endblock(int endLabel)
{
  /* call kmpc_for_static_fini */
  int ili;

  wr_block();
  cr_block();
  ili = ll_make_kmpc_for_static_fini();
  exp_label(endLabel);
  iltb.callfg = 1;
  chk_block(ili);
  ili = ll_make_kmpc_barrier();
  iltb.callfg = 1;
  chk_block(ili);
  BIH_LABEL(expb.curbih) = endLabel;
  ILIBLKP(endLabel, expb.curbih);
}

/* set:       1 to set, 0 to restore
 * eampp:     if it is eampp, then subtract its value(1) from mppgcnt
 * nextbih:   use previous bih of mppgbih[mppgcnt - eampp]
 */
#define SET_MPPBIH 1
#define RESTORE_MPPBIH 0
#define IS_PREVMPPG 1
#define IS_NOTPREVMPPG 0
#define USE_NEXTBIH 1
#define NOTUSE_NEXTBIH 0

static void
resetMppBih(int set, int eampp, int nextbih)
{
  static int savebih;
  static int savex14;
  int bih;

  if (set) {
    savebih = expb.curbih;
    savex14 = flg.x[14];
    flg.x[14] |= 0x1000; /* don't split at calls */
    wr_block();
    expb.curbih = mppgbih[mppgcnt - eampp];
    if (nextbih)
      expb.curbih = BIH_NEXT(expb.curbih);
    rdilts(expb.curbih);
    expb.curilt = ILT_PREV(0);
  } else {
    if (nextbih) {
      bih = mppgbih[mppgcnt - eampp];
      bih = BIH_NEXT(bih);
      wrilts(bih);
    } else
      wrilts(mppgbih[mppgcnt - eampp]);
    expb.curbih = savebih;
    rdilts(expb.curbih);
    expb.curilt = ILT_PREV(0);
    flg.x[14] = savex14;
  }
}

static void
sptr_list_add(sptr_list_t **list, int sptr, int size_ili, LOGICAL is_cmblk,
              int cplus_assign_rou, int vec_size_ili, int o_sptr)
{
  sptr_list_t *node = malloc(sizeof(sptr_list_t));

  node->o_sptr = o_sptr;
  node->sptr = sptr;
  node->next = *list;
  node->is_common_block = is_cmblk;
  node->size_ili = size_ili;
  node->vec_size_ili = vec_size_ili; /* used for COPYIN_CL of arrays */
  node->cplus_assign_rou = cplus_assign_rou;
  *list = node;
}

static void
sptr_list_free(sptr_list_t **list)
{
  sptr_list_t *n = *list;
  while (n) {
    sptr_list_t *next = n->next;
    free(n);
    n = next;
  }
  *list = NULL;
}

static int
sptr_list_length(const sptr_list_t *list)
{
  int count = 0;
  const sptr_list_t *n;

  for (n = list; n; n = n->next)
    ++count;

  return count;
}

/* Returns an ili of a temporary variable that conatins size information
 * The runtime for instance, _mp_copypriv_kmpc, expects size_t* for size.
 *
 * 'bytes' is the actual byte size and not an sptr or ili.
 */
static int
gen_size_acon(int size_ili)
{
  int ili, tmp, nme;
  const int dtype = (TARGET_PTRSIZE == 8) ? DT_INT8 : DT_INT;

  tmp = ll_get_private_temp(dtype);
  SCP(tmp, SC_AUTO);

  ili = ad_acon(tmp, 0);
  nme = addnme(NT_VAR, tmp, 0, 0);
  ADDRTKNP(tmp, 1);

  if (TARGET_PTRSIZE == 8) {
    ili = ad4ili(IL_STKR, size_ili, ili, nme, MSZ_I8);
  } else {
    size_ili = ad1ili(IL_KIMV, size_ili);
    ili = ad4ili(IL_ST, size_ili, ili, nme, MSZ_WORD);
  }
  chk_block(ili);

  return ad_acon(tmp, 0);
}

/* Given a sptr list, create an array of pairs:
 * (size, address) where:
 * 'size' - Pointer to a temporary variable containing the byte size of
 *          sptr. (size_t *)
 * 'address' - Address of sptr. (void *).
 *
 * These pairs are represented in an array where
 * the even indices are the size pointers and the odd indices the
 * addresses.  The sentinel/terminator is the all-zero pair.
 * [(sz0,addr0), (sz1,addr1), ... (0x0, 0x0)].
 *
 * We represent these as an array, which is more convenient to manage
 * internally.  The runtime routine _mp_copypriv_kmpc expects this format.
 *
 * Returns: The sptr of this majestic array that we so masterfully create here.
 */
static int
make_copypriv_array(const sptr_list_t *list, LOGICAL pass_size_addresses)
{
  int i, ili, nme, n_elts, array, dtype, basenme, adsc;
  static int id;
  const sptr_list_t *node;

  /* Count the number of items in the list */
  n_elts = 0;
  for (node = list; node; node = node->next)
    ++n_elts;

  /* We represent each entry as a pair for each private variable (each node in
   * sptr_list): (size, sptr)
   *
   * +2 for the last node, the sentinel (null node), which tells the
   * runtime it has reached the end of the array.  Each node is 2 array elts.
   */
  n_elts = (n_elts * 2) + 2;

  /* Create the array dtype: each element is word size */
  array = getnewccsym('a', ++id, ST_ARRAY);
  {
    ADSC *adsc;
    INT con[2] = {0, n_elts};

    dtype = get_array_dtype(1, DT_CPTR);
    adsc = AD_DPTR(dtype);
    AD_LWBD(adsc, 0) = stb.i1;
    AD_UPBD(adsc, 0) = getcon(con, DT_INT);
    AD_NUMELM(adsc) = AD_UPBD(adsc, 0);
  }

  DTYPEP(array, dtype);
  SCP(array, SC_AUTO);

  /* Build the list: (size, sptr) pairs. */
  basenme = addnme(NT_VAR, array, 0, 0);
  for (node = list, i = 0; node; node = node->next, ++i) {
    int sptr_nme, sptr_ili;

    if (node->is_common_block || THREADG(node->sptr)) {
/* This is thread private so obtain address from the TP vector */
      if (node->is_common_block)
        ref_threadprivate(node->sptr, &sptr_ili, &sptr_nme);
      else
        ref_threadprivate_var(node->sptr, &sptr_ili, &sptr_nme, 1);
    } else {
      /* Else, this is not thread private */
      sptr_nme = addnme(NT_VAR, node->sptr, 0, 0);
      sptr_ili = mk_address(node->sptr);
    }

    /* array[i] = size */
    nme = add_arrnme(NT_ARR, array, basenme, 0, ad_icon(i), FALSE);
    if (pass_size_addresses) { /* why do I need to pass address? */
      ili = gen_size_acon(node->size_ili);
      ili = ad3ili(IL_STA, ili, ad_acon(array, i * TARGET_PTRSIZE), nme);
    } else {
      ili = ad4ili(IL_ST, node->size_ili, ad_acon(array, i * TARGET_PTRSIZE),
                   nme, TARGET_PTRSIZE == 8 ? MSZ_I8 : MSZ_WORD);
    }
    chk_block(ili);

    /* array[i+1] = local (stack based) sptr */
    ++i;
    nme = add_arrnme(NT_ARR, array, basenme, 0, ad_icon(i), FALSE);
    ili = ad3ili(IL_STA, sptr_ili, ad_acon(array, i * TARGET_PTRSIZE), nme);
    chk_block(ili);
  }

  /* Terminate the array with a sentinel that the runtime will recognize */
  nme = add_arrnme(NT_ARR, array, basenme, 0, ad_icon(i), FALSE);
  ili = ad3ili(IL_STA, ad_aconi(0), ad_acon(array, i * TARGET_PTRSIZE), nme);
  chk_block(ili);

  ++i;
  nme = add_arrnme(NT_ARR, array, basenme, 0, ad_icon(i), FALSE);
  ili = ad3ili(IL_STA, ad_aconi(0), ad_acon(array, i * TARGET_PTRSIZE), nme);
  chk_block(ili);

  return array;
}

static int
mk_memcpy()
{
  int func;
  func = mk_prototype("memcpy", NULL, DT_CPTR, 3, DT_CPTR, DT_CPTR, DT_UINT8);
  SCP(func, SC_EXTERN);
  func = mkfunc("memcpy");
  return func;
}

static void
addCopyinInplace(const sptr_list_t *list)
{
  int i, ili, nme, n_elts, dest_nme, argili, call;
  int master_ili, lab, altili, func, sptr;
  const sptr_list_t *node;

  n_elts = 0;
  lab = getlab();
  for (node = list, i = 0; node; node = node->next, ++i) {
    int sptr_nme, sptr_ili;

    sptr = node->o_sptr;
    if (STYPEG(sptr) == ST_CMBLK) {
      sptr = CMEMFG(sptr);
      if (!sptr)
        continue;
    } else if (SCG(sptr) == SC_BASED && POINTERG(sptr)) {
      sptr = MIDNUMG(sptr);
    }
    /* what we have here it TPxx, need to find the symbol it points to */
    /* master copy - should be passed from previous region */
    master_ili = mk_address(sptr);

    /* current threadprivate copy */
    sptr_ili = llGetThreadprivateAddr(node->sptr);
    dest_nme = ILI_OPND(sptr_ili, 2);

    if (n_elts == 0) {
      ili = ad4ili(IL_ACJMP, sptr_ili, master_ili, CC_EQ, lab);
      RFCNTI(lab);
      chk_block(ili);
      n_elts = 1;
    }

    /* now do a copy */
    altili = 0;
    {
      func = mk_memcpy();
      argili = jsr_add_arg(0, IL_ARGKR, sel_iconv(node->size_ili, 1));
      argili = jsr_add_arg(argili, IL_ARGAR, master_ili);
      argili = jsr_add_arg(argili, IL_ARGAR, sptr_ili);
      call = make_call("memcpy", IL_JSR, argili);
      argili = ad1ili(IL_NULL, 0);
      argili = ad4ili(IL_GARG, sel_iconv(node->size_ili, 1), argili, DT_INT8, 0);
      argili = ad4ili(IL_GARG, master_ili, argili, DT_CPTR, 0);
      argili = ad4ili(IL_GARG, sptr_ili, argili, DT_CPTR, 0);
      altili = ad3ili(IL_GJSR, func, argili, 0);
    }
    ILI_ALT(call) = altili;
    iltb.callfg = 1;
    chk_block(call);
  }
  if (n_elts) {
    wr_block();
    cr_block();

    /* create a block */
    BIH_LABEL(expb.curbih) = lab;
    ILIBLKP(lab, expb.curbih);
    ili = ll_make_kmpc_barrier();
    iltb.callfg = 1;
    chk_block(ili);

    wr_block();
    cr_block();
  }
}

static void
make_copypriv_array_tls(const sptr_list_t *list)
{
  int i, ili, nme, n_elts, array, dtype, basenme, adsc, argili, call;
  int master_ili, thread_addr, lab, altili, master_nme, func, sptr;
  const sptr_list_t *node;

  n_elts = 0;
  lab = getlab();
  for (node = list, i = 0; node; node = node->next, ++i) {
    int sptr_nme, sptr_ili;

    sptr = MIDNUMG(node->sptr);
    if (STYPEG(sptr) == ST_CMBLK) {
      sptr = CMEMFG(node->o_sptr);
      if (!sptr)
        continue;
    } else if (SCG(sptr) == SC_BASED && POINTERG(sptr)) {
      sptr = MIDNUMG(sptr);
    }
    master_nme = addnme(NT_VAR, sptr, 0, (INT)0);
    master_ili = mk_address(sptr);

    basenme = addnme(NT_VAR, node->sptr, 0, (INT)0);
    sptr_ili = ad2ili(IL_LDA, ad_acon(node->sptr, (INT)0), basenme);
    if (n_elts == 0) {
      ili = ad4ili(IL_ACJMP, sptr_ili, master_ili, CC_EQ, lab);
      RFCNTI(lab);
      chk_block(ili);
      n_elts = 1;
    }

    /* now do a copy */
    altili = 0;
    {
      func = mk_memcpy();
      argili = jsr_add_arg(0, IL_ARGIR, sel_iconv(node->size_ili, 0));
      argili = jsr_add_arg(argili, IL_ARGAR, master_ili);
      argili = jsr_add_arg(argili, IL_ARGAR, sptr_ili);
      call = make_call("memcpy", IL_JSR, argili);
      argili = ad1ili(IL_NULL, 0);
      argili = ad4ili(IL_GARG, sel_iconv(node->size_ili, 1), argili, DT_INT8, 0);
      argili = ad4ili(IL_GARG, master_ili, argili, DT_CPTR, 0);
      argili = ad4ili(IL_GARG, sptr_ili, argili, DT_CPTR, 0);
      altili = ad3ili(IL_GJSR, func, argili, 0);
    }
    ILI_ALT(call) = altili;
    iltb.callfg = 1;
    chk_block(call);
  }
  if (n_elts) {
    wr_block();
    cr_block();

    /* create a block */
    BIH_LABEL(expb.curbih) = lab;
    ILIBLKP(lab, expb.curbih);
    ili = ll_make_kmpc_barrier();
    iltb.callfg = 1;
    chk_block(ili);

    wr_block();
    cr_block();
  }
}


static int 
find_enlab_bih(int func) 
{
  int bih;
  bih = BIH_NEXT(BIHNUMG(func));
  return bih;
}

static void
set_taskloopvars(int lb, int ub, int stride, int lastitr)
{
  int nme, basenm, baseili, ili, bih, arg, asym;
  ILI_OP ld, st;
  MSZ msz;

  /* This code is in an outlined taskloop routine.
   * Load taskloop vars from arg1 to local/private vars.
   */
  arg = ll_get_hostprog_arg(GBL_CURRFUNC, 2);
  basenm = addnme(NT_VAR, arg, 0, 0);
  baseili = ad_acon(arg, 0);
  baseili = mk_address(arg);
  arg = mk_argasym(arg);
  basenm = addnme(NT_VAR, arg, 0, (INT)0);
  baseili = ad2ili(IL_LDA, baseili, basenm);
  bih = expb.curbih = find_enlab_bih(GBL_CURRFUNC);
  rdilts(bih);
  nme = addnme(NT_IND, lb, basenm, 0);
  ili = ad3ili(IL_AADD, baseili, ad_aconi(TASK_LPVAR_OFFSET), 0);
  ldst_msz(DT_INT8, &ld, &st, &msz);
  ili = ad3ili(ld, ili, nme, msz);
  ldst_msz(DTYPEG(lb), &ld, &st, &msz);
  if (msz != MSZ_I8)
    ili = kimove(ili);
  ili = ad4ili(st, ili, ad_acon(lb, 0), addnme(NT_VAR, lb, 0, 0), msz);
  chk_block(ili);
  wrilts(bih);

  bih = expb.curbih = find_enlab_bih(GBL_CURRFUNC);
  rdilts(bih);
  nme = addnme(NT_IND, ub, basenm, 0);
  ili = ad3ili(IL_AADD, baseili, 
               ad_aconi(TASK_LPVAR_OFFSET+zsize_of(DT_INT8)), 0);
  ldst_msz(DT_INT8, &ld, &st, &msz);
  ili = ad3ili(ld, ili, nme, msz);
  ldst_msz(DTYPEG(ub), &ld, &st, &msz);
  if (msz != MSZ_I8)
    ili = kimove(ili);
  ili = ad4ili(st, ili, ad_acon(ub, 0), addnme(NT_VAR, ub, 0, 0), msz);
  chk_block(ili);
  wrilts(bih);

  if (STYPEG(stride) != ST_CONST) {
    bih = expb.curbih = find_enlab_bih(GBL_CURRFUNC);
    rdilts(expb.curbih);
    nme = addnme(NT_IND, stride, basenm, 0);
    ili = ad3ili(IL_AADD, baseili, 
                 ad_aconi(TASK_LPVAR_OFFSET+(zsize_of(DT_INT8)*2)), 0);
  ldst_msz(DT_INT8, &ld, &st, &msz);
    ili = ad3ili(ld, ili, nme, msz);
    ldst_msz(DTYPEG(stride), &ld, &st, &msz);
    if (msz != MSZ_I8)
      ili = kimove(ili);
    ili = ad4ili(st, ili, ad_acon(stride, 0), addnme(NT_VAR, stride, 0, 0), msz);
    chk_block(ili);
    wrilts(bih);
  }

  if (lastitr && STYPEG(lastitr) != ST_CONST) {
    bih = expb.curbih = find_enlab_bih(GBL_CURRFUNC);
    rdilts(bih);
    nme = addnme(NT_IND, st, basenm, 0);
    ldst_msz(DT_INT, &ld, &st, &msz);
    ili = ad3ili(IL_AADD, baseili, 
                 ad_aconi(TASK_LPVAR_OFFSET+(zsize_of(DT_INT8)*3)), 0);
    ili = ad3ili(ld, ili, nme, msz);
    ldst_msz(DTYPEG(lastitr), &ld, &st, &msz);
    ili = ad4ili(st, ili, ad_acon(lastitr, 0), addnme(NT_VAR, lastitr, 0, 0), msz);
    chk_block(ili);
    wrilts(bih);
  }
}

static void
clear_taskloop_info()
{
  /* always keep the TASK_LPVAR_OFFSET we want to keep it the same */
  INT offset = TASK_LPVAR_OFFSET;
  BZERO(&tasklp_info, char, sizeof(tasklp_info));
  TASK_LPVAR_OFFSET = offset;
}

void
exp_smp(ILM_OP opc, ILM *ilmp, int curilm)
{
#ifdef IM_BPAR
  int argili = 0;
  int ili, tili, ili_arg;
  int lastilt;
  int sym, sptr, offset, savebih;
  int end_label, beg_label;
  int off;
  int addr, nmex, stili;
  int prev_scope;
  char name[10];
  int argilm;
  int tpv, pv;
  int savex14;
  char *doschedule;
  int semaphore, dotarget;
  static int assign_rou = 0; /* C++ only, lets avoid more ifdefs */
  ILM_T rou_op;
  int num_elem, element_size;
  loop_args_t loop_args;
  LLTask *task;
  LOGICAL is_cmblk;
  static sptr_list_t *copysptr_list = NULL;
  static int uplevel_sptr;
  static int single_thread, in_single;
  int nlower, nupper, nstride;
  int sz;
  ISZ_T size, num_elements;

  switch (opc) {
  case IM_BPAR:
  case IM_BPARN:
  case IM_BPARD:
  case IM_BPARA:
  case IM_EPAR:
  case IM_EPARD:
  case IM_TASKREG:
  case IM_TASKLOOPREG:
  case IM_ETASKREG:
  case IM_ETASKLOOPREG:
  case IM_BTARGET:
  case IM_ETARGET:
  case IM_BTEAMS:
  case IM_BTEAMSN:
  case IM_ETEAMS:
    break;
  default:
    ll_rewrite_ilms(-1, curilm, 0);
    break;
  }
  switch (opc) {
  case IM_BMPSCOPE:
    if (ll_ilm_is_rewriting())
      break;
    scope_sptr = ILM_OPND(ilmp, 1);
#ifdef PARUPLEVELG
    uplevel_sptr = PARUPLEVELG(scope_sptr);
#else
    uplevel_sptr = 0;
#endif
    break;
  case IM_EMPSCOPE:
    break;
  case IM_BPAR:
  case IM_BPARN:
  case IM_BPARA:
    if (flg.opt != 0) {
      wr_block();
      cr_block();
    }
    incr_par_cnt();
    BIH_FT(expb.curbih) = TRUE;
    BIH_QJSR(expb.curbih) = TRUE;
    BIH_NOMERGE(expb.curbih) = TRUE;
    if (gbl.outlined)
      expb.sc = SC_PRIVATE;
    if (ll_par_cnt == 1) {
      int isPar = ILI_OF(ILM_OPND(ilmp, 1));
      int par_label, end_label, iliarg, nthreads, proc_bind;
      sptr = ll_make_outlined_func(uplevel_sptr, scope_sptr);
      if (!PARENCLFUNCG(scope_sptr))
        PARENCLFUNCP(scope_sptr, sptr);

      ll_write_ilm_header(sptr, curilm);
      iliarg = ll_load_outlined_args(scope_sptr, sptr, gbl.outlined);

      /* if (isPar == 0)
             call omp_do_func(.....)
             goto do_end;
         par_label:
             call kmpc_fork_call (....., omp_do_func,.... )
         do_label:
       */

      par_label = getlab();
      end_label = getlab();

      isPar = ad3ili(IL_ICJMPZ, isPar, CC_EQ, par_label);
      RFCNTI(par_label);
      chk_block(isPar);

      ili = ll_make_kmpc_serialized_parallel();
      iltb.callfg = 1;
      chk_block(ili);

      ili = ll_make_outlined_call2(sptr, iliarg);
      iltb.callfg = 1;
      chk_block(ili);

      ili = ll_make_kmpc_end_serialized_parallel();
      iltb.callfg = 1;
      chk_block(ili);

      ili = ad1ili(IL_JMP, end_label);
      RFCNTI(end_label);
      chk_block(ili);

      wr_block();
      cr_block();
      exp_label(par_label);
      proc_bind = 0;
      if (opc == IM_BPARA) {
        int flag = ILM_OPND(ilmp, 3);
        if (flag & 0x2) {
          nthreads = ILI_OF(ILM_OPND(ilmp, 2));
          ili = ll_make_kmpc_push_num_threads(nthreads);
          iltb.callfg = 1;
          chk_block(ili);
        }
        if (flag & 0x1) {
          proc_bind = ILM_OPND(ilmp, 4);
        }
      } if (opc == IM_BPARN) {
        nthreads = ILI_OF(ILM_OPND(ilmp, 2));
        ili = ll_make_kmpc_push_num_threads(nthreads);
        iltb.callfg = 1;
        chk_block(ili);
      } 
      if (proc_bind) {
        proc_bind = ad_icon(proc_bind);
        ili = ll_make_kmpc_push_proc_bind(proc_bind);
        iltb.callfg = 1;
        chk_block(ili);
      }
      ili = ll_make_kmpc_fork_call(sptr, 1, &iliarg);
      iltb.callfg = 1;
      chk_block(ili);

      exp_label(end_label);

      ccff_info(MSGOPENMP, "OMP001", gbl.findex, gbl.lineno,
                "Parallel region activated", NULL);

    } else if (ll_par_cnt > 1) {
      ll_rewrite_ilms(-1, curilm, 0);
    }

    break;
  case IM_BPARD:
  bpard:
    /* lexically nested begin parallel */
    incr_par_cnt();
    if (ll_par_cnt > 1) {
      ll_rewrite_ilms(-1, curilm, 0);
      break;
    }
    if (flg.opt != 0) {
      wr_block();
      cr_block();
    }
    BIH_FT(expb.curbih) = TRUE;
    BIH_QJSR(expb.curbih) = TRUE;
    BIH_NOMERGE(expb.curbih) = TRUE;
    if (ll_par_cnt == 1) {
      int isPar = ILI_OF(ILM_OPND(ilmp, 1));
      int par_label, end_label, iliarg, proc_bind;
      sptr = ll_make_outlined_func(uplevel_sptr, scope_sptr);
      if (!PARENCLFUNCG(scope_sptr))
        PARENCLFUNCP(scope_sptr, sptr);
      ll_write_ilm_header(sptr, curilm);
      iliarg = ll_load_outlined_args(scope_sptr, sptr, gbl.outlined);

      /* if (isPar == 0)
             call omp_do_func(.....)
             goto do_end;
         par_label:
             call kmpc_fork_call (....., omp_do_func,.... )
         do_label:
       */

      par_label = getlab();
      end_label = getlab();

      isPar = ad3ili(IL_ICJMPZ, isPar, CC_EQ, par_label);
      RFCNTI(par_label);
      chk_block(isPar);

      ili = ll_make_kmpc_serialized_parallel();
      iltb.callfg = 1;
      chk_block(ili);

      ili = ll_make_outlined_call2(sptr, iliarg);
      iltb.callfg = 1;
      chk_block(ili);

      ili = ll_make_kmpc_end_serialized_parallel();
      iltb.callfg = 1;
      chk_block(ili);

      ili = ad1ili(IL_JMP, end_label);
      RFCNTI(end_label);
      chk_block(ili);

      proc_bind = ILM_OPND(ilmp, 2);
      if (proc_bind) {
        proc_bind = ad_icon(proc_bind);
        ili = ll_make_kmpc_push_proc_bind(proc_bind);
        iltb.callfg = 1;
        chk_block(ili);
      }

      wr_block();
      cr_block();
      exp_label(par_label);
      ili = ll_make_kmpc_fork_call(sptr, 1, &iliarg);
      iltb.callfg = 1;
      chk_block(ili);

      wr_block();
      cr_block();
      exp_label(end_label);
    }
    ccff_info(MSGOPENMP, "OMP001", gbl.findex, gbl.lineno,
              "Parallel region activated", NULL);
    break;
  case IM_BTEAMS:
  case IM_BTEAMSN:
    if (flg.opt != 0) {
      wr_block();
      cr_block();
    }
    incr_par_cnt();
    BIH_FT(expb.curbih) = TRUE;
    BIH_QJSR(expb.curbih) = TRUE;
    BIH_NOMERGE(expb.curbih) = TRUE;
    if (gbl.outlined)
      expb.sc = SC_PRIVATE;
    if (ll_par_cnt == 1) {
      int par_label, iliarg, nteams, n_limit;
      sptr = ll_make_outlined_func(uplevel_sptr, scope_sptr);
      if (!PARENCLFUNCG(scope_sptr))
        PARENCLFUNCP(scope_sptr, sptr);

      ll_write_ilm_header(sptr, curilm);
      iliarg = ll_load_outlined_args(scope_sptr, sptr, gbl.outlined);

      par_label = getlab();

      wr_block();
      cr_block();
      exp_label(par_label);
      if (opc == IM_BTEAMSN) {
        nteams = ILI_OF(ILM_OPND(ilmp, 1));
        n_limit = ILI_OF(ILM_OPND(ilmp, 2));
        ili = ll_make_kmpc_push_num_teams(nteams, n_limit);
        iltb.callfg = 1;
        chk_block(ili);
      }
      ili = ll_make_kmpc_fork_teams(sptr, 1, &iliarg);
      iltb.callfg = 1;
      chk_block(ili);

      ccff_info(MSGOPENMP, "OMP022", gbl.findex, gbl.lineno,
                "Teams region activated", NULL);

    } else if (ll_par_cnt > 1) {
      ll_rewrite_ilms(-1, curilm, 0);
    }

    break;

  case IM_BTARGET:
    /* lexically nested begin parallel */
    incr_par_cnt();
    if (ll_par_cnt > 1) {
      ll_rewrite_ilms(-1, curilm, 0);
      break;
    }
    if (flg.opt != 0) {
      wr_block();
      cr_block();
    }
    BIH_FT(expb.curbih) = TRUE;
    BIH_QJSR(expb.curbih) = TRUE;
    BIH_NOMERGE(expb.curbih) = TRUE;
    if (ll_par_cnt == 1) {
      int isPar = ILI_OF(ILM_OPND(ilmp, 1));
      int par_label, end_label, iliarg;
      sptr = ll_make_outlined_func(uplevel_sptr, scope_sptr);
      if (!PARENCLFUNCG(scope_sptr))
        PARENCLFUNCP(scope_sptr, sptr);
      ll_write_ilm_header(sptr, curilm);
      iliarg = ll_load_outlined_args(scope_sptr, sptr, gbl.outlined);

      /* if (isPar == 0)
             call host_version(.....)
             goto do_end;
         par_label:
             call target_offload (....., target_version_func_ptr,.... )
         do_label:
       */

      par_label = getlab();
      end_label = getlab();

      ili = ll_make_outlined_call2(sptr, iliarg);
      iltb.callfg = 1;
      chk_block(ili);
    }
    ccff_info(MSGOPENMP, "OMP020", gbl.findex, gbl.lineno,
              "Target region activated", NULL);
    break;
  case IM_ETARGET:
    if (ll_par_cnt == 1) {
      ilm_outlined_end_write(curilm);
    }
    decr_ll_par_cnt();
    if (ll_par_cnt >= 1)
      ll_rewrite_ilms(-1, curilm, 0);

    if (gbl.outlined)
      expb.sc = SC_AUTO;
    ccff_info(MSGOPENMP, "OMP021", gbl.findex, gbl.lineno,
              "Target region terminated", NULL);
    break;
  case IM_EPAR:
    if (ll_par_cnt == 1) {
      ilm_outlined_end_write(curilm);
    }
    decr_ll_par_cnt();
    if (ll_par_cnt >= 1)
      ll_rewrite_ilms(-1, curilm, 0);

    if (gbl.outlined)
      expb.sc = SC_AUTO;
    ccff_info(MSGOPENMP, "OMP002", gbl.findex, gbl.lineno,
              "Parallel region terminated", NULL);
    break;
  case IM_EPARD:
  epard:
    if (ll_par_cnt == 1) {
      ilm_outlined_end_write(curilm);
    }
    decr_ll_par_cnt();
    if (ll_par_cnt >= 1)
      ll_rewrite_ilms(-1, curilm, 0);
    ccff_info(MSGOPENMP, "OMP002", gbl.findex, gbl.lineno,
              "Parallel region terminated", NULL);
    break;
  case IM_ETEAMS:
    if (ll_par_cnt == 1) {
      ilm_outlined_end_write(curilm);
    }
    decr_ll_par_cnt();
    if (ll_par_cnt >= 1)
      ll_rewrite_ilms(-1, curilm, 0);

    if (gbl.outlined)
      expb.sc = SC_AUTO;
    ccff_info(MSGOPENMP, "OMP023", gbl.findex, gbl.lineno,
              "Teams region terminated", NULL);
    break;
  case IM_BCS:
    /*
     * It's required that the front-end does not generate nested
     * critical sections (static only).  Keeping the semaphore variable
     * around for the IM_ECS depends on this.  If nested critical sections
     * need to be supported, then need to add a field to the IM_BCS and
     * IM_ECS ilms which will be the semaphore variable created by the
     * front-ends.
     */
    if (ll_ilm_is_rewriting())
      break;
    crit_cnt++;
    if (flg.opt != 0) {
      wr_block();
      cr_block();
    }
    BIH_FT(expb.curbih) = TRUE;
    BIH_QJSR(expb.curbih) = TRUE;
    BIH_NOMERGE(expb.curbih) = TRUE;
    bihb.csfg = BIH_CS(expb.curbih) = TRUE;
    ili = add_mp_bcs_nest();
    iltb.callfg = 1;
    chk_block(ili);
    ccff_info(MSGOPENMP, "OMP003", gbl.findex, gbl.lineno,
              "Begin critical section", NULL);
    break;
  case IM_ECS:
    if (ll_ilm_is_rewriting())
      break;
    crit_cnt--;
    BIH_FT(expb.curbih) = TRUE;
    BIH_QJSR(expb.curbih) = TRUE;
    BIH_NOMERGE(expb.curbih) = TRUE;
    BIH_CS(expb.curbih) = TRUE;
    ili = add_mp_ecs_nest();
    iltb.callfg = 1;
    chk_block(ili);
    wr_block();
    cr_block();
    if (crit_cnt <= 0)
      bihb.csfg = 0;
    ccff_info(MSGOPENMP, "OMP004", gbl.findex, gbl.lineno,
              "End critical section", NULL);
    break;
  case IM_P:
    if (ll_ilm_is_rewriting())
      break;
    crit_cnt++;
    if (flg.opt != 0) {
      wr_block();
      cr_block();
    }
    BIH_FT(expb.curbih) = TRUE;
    BIH_QJSR(expb.curbih) = TRUE;
    BIH_NOMERGE(expb.curbih) = TRUE;
    bihb.csfg = BIH_CS(expb.curbih) = TRUE;
    sym = ILM_OPND(ilmp, 1);
    if (!XBIT(69, 0x40) || !is_unnamed_cs(sym)) {
      ili = add_mp_p(sym);
    } else {
      ili = add_mp_unp();
    }
    iltb.callfg = 1;
    chk_block(ili);
    semaphore = MIDNUMG(sym);
    ccff_info(MSGOPENMP, "OMP012", gbl.findex, gbl.lineno,
              "Begin critical section (%semaphore)", "semaphore=%s",
              SYMNAME(semaphore), NULL);
    break;
  case IM_V:
    if (ll_ilm_is_rewriting())
      break;
    crit_cnt--;
    BIH_FT(expb.curbih) = TRUE;
    BIH_QJSR(expb.curbih) = TRUE;
    BIH_NOMERGE(expb.curbih) = TRUE;
    BIH_CS(expb.curbih) = TRUE;
    sym = ILM_OPND(ilmp, 1);
    if (!XBIT(69, 0x40) || !is_unnamed_cs(sym)) {
      ili = add_mp_v(sym);
    } else {
      ili = add_mp_unv();
    }
    iltb.callfg = 1;
    chk_block(ili);
    wr_block();
    cr_block();
    if (crit_cnt <= 0)
      bihb.csfg = 0;
    semaphore = MIDNUMG(sym);
    ccff_info(MSGOPENMP, "OMP013", gbl.findex, gbl.lineno,
              "End critical section (%semaphore)", "semaphore=%s",
              SYMNAME(semaphore), NULL);
    break;
  case IM_MPSCHED: {
    if (!ll_ilm_is_rewriting()) {
      const int lower = ILM_OPND(ilmp, 1);
      const int upper = ILM_OPND(ilmp, 2);
      const int stride = ILM_OPND(ilmp, 3);
      const int last = ILM_OPND(ilmp, 4);
      const int dtype = ILM_OPND(ilmp, 5);
      ili = ll_make_kmpc_dispatch_next(lower, upper, stride, last, dtype);
      iltb.callfg = 1;
      chk_block(ili);
      ILM_RESULT(curilm) = ili;
    }
    break;
  }
  case IM_MPBORDERED: {
    if (!ll_ilm_is_rewriting()) {
      BIH_NOMERGE(expb.curbih) = TRUE;
      crit_cnt++;
      bihb.csfg = BIH_CS(expb.curbih) = TRUE;
      ili = ll_make_kmpc_ordered();
      iltb.callfg = 1;
      chk_block(ili);
    }
    break;
  }
  case IM_MPEORDERED: {
    if (!ll_ilm_is_rewriting()) {
      ili = ll_make_kmpc_end_ordered();
      iltb.callfg = 1;
      BIH_CS(expb.curbih) = TRUE;
      chk_block(ili);
      wr_block();
      crit_cnt--;
      if (crit_cnt <= 0)
        bihb.csfg = 0;
    }
    break;
  }
  case IM_MPTASKLOOP:
    if (ll_ilm_is_rewriting()) 
      break;

    {
      int lb, ub, st, lastitr;

      lb = ILM_OPND(ilmp, 1);
      ub = ILM_OPND(ilmp, 2);
      st = ILM_OPND(ilmp, 3);
      lastitr = ILM_OPND(ilmp, 4);

      ENCLFUNCP(lb, task_fnsptr);
      ENCLFUNCP(ub, task_fnsptr);
      ENCLFUNCP(st, task_fnsptr);
      TASK_LASTITR = lastitr;
      if (lastitr) {
        ENCLFUNCP(lastitr, task_fnsptr);
      }
      set_taskloopvars(lb, ub, st, lastitr);
    }

    break;
  case IM_MPLOOP: {
    int sched;
    if (ll_par_cnt >= 1)
      break;
    nlower = ILM_OPND(ilmp, 1);
    nupper = ILM_OPND(ilmp, 2);
    nstride = ILM_OPND(ilmp, 3);
    if (!XBIT(183, 0x100000)) {
      nlower = getccsym_copy(nlower);
      nupper = getccsym_copy(nupper);
      nstride = getccsym_copy(nstride);
      ENCLFUNCP(nlower, GBL_CURRFUNC);
      ENCLFUNCP(nupper, GBL_CURRFUNC);
      ENCLFUNCP(nstride, GBL_CURRFUNC);
      exp_add_copy(nlower, ILM_OPND(ilmp, 1));
      exp_add_copy(nupper, ILM_OPND(ilmp, 2));
      exp_add_copy(nstride, ILM_OPND(ilmp, 3));
    }
    loop_args.lower = nlower;
    loop_args.upper = nupper;
    loop_args.stride = nstride;
    loop_args.chunk = ILM_OPND(ilmp, 4);
    loop_args.last = ILM_OPND(ilmp, 5);
    loop_args.dtype = ILM_OPND(ilmp, 6);
    loop_args.sched = ILM_OPND(ilmp, 7);
    sched = mp_sched_to_kmpc_sched(loop_args.sched);
    switch (sched) {
    case KMP_SCH_STATIC:
    case KMP_SCH_STATIC_CHUNKED:
      if ((ILM_OPND(ilmp, 7) & 0xff00) == MP_SCH_CHUNK_1) {
        doschedule = "static cyclic";
        ccff_info(MSGOPENMP, "OMP014", gbl.findex, gbl.lineno,
                  "Parallel loop activated with %schedule schedule",
                  "schedule=%s", doschedule, NULL);
      }

    case KMP_DISTRIBUTE_STATIC_CHUNKED:
    case KMP_DISTRIBUTE_STATIC:
      ili = ll_make_kmpc_for_static_init(&loop_args);
      break;
    default:
      ili = ll_make_kmpc_dispatch_init(&loop_args);
    }
    iltb.callfg = 1;
    chk_block(ili);
    BIH_NOMERGE(expb.curbih) = TRUE;
    if (!XBIT(183,0x100000)) {
      exp_add_copy(ILM_OPND(ilmp, 1), nlower);
      exp_add_copy(ILM_OPND(ilmp, 2), nupper);
      exp_add_copy(ILM_OPND(ilmp, 3), nstride);
    }

    /* constant propagation stop when it sees function call. We may have some
     * stride that needs to propagate for computation of tripcount. */
    if (flg.opt != 0) {
      wr_block();
      cr_block();
    }

    break;
  }
  case IM_MPDISTLOOP: {
    int sched;
    if (ll_par_cnt >= 1)
      break;
    loop_args.lower = ILM_OPND(ilmp, 1);
    loop_args.upper = ILM_OPND(ilmp, 2);
    loop_args.stride = ILM_OPND(ilmp, 3);
    loop_args.chunk = ILM_OPND(ilmp, 4);
    loop_args.last = ILM_OPND(ilmp, 5);
    loop_args.upperd = ILM_OPND(ilmp, 6);
    loop_args.dtype = ILM_OPND(ilmp, 7);
    loop_args.sched = ILM_OPND(ilmp, 8);
    sched = mp_sched_to_kmpc_sched(loop_args.sched);
    switch (sched) {
    case KMP_SCH_STATIC:
    case KMP_SCH_STATIC_CHUNKED:
    case KMP_DISTRIBUTE_STATIC_CHUNKED:
    case KMP_DISTRIBUTE_STATIC:
      ili = ll_make_kmpc_dist_for_static_init(&loop_args);
      break;
    default:
      ili = ll_make_kmpc_dist_dispatch_init(&loop_args);
    }
    iltb.callfg = 1;
    chk_block(ili);
    BIH_NOMERGE(expb.curbih) = TRUE;

    /* constant propagation stop when it sees function call. We may have some
     * stride that needs to propagate for computation of tripcount. */
    if (flg.opt != 0) {
      wr_block();
      cr_block();
    }

    break;
  }
  case IM_MPLOOPFINI: {
    if (ll_par_cnt >= 1)
      break;
    const int sched = mp_sched_to_kmpc_sched(ILM_OPND(ilmp, 2));
    if (sched == KMP_ORD_STATIC || sched == KMP_ORD_DYNAMIC_CHUNKED) {
      ili = ll_make_kmpc_dispatch_fini(ILM_OPND(ilmp, 1));
      iltb.callfg = 1;
      chk_block(ili);
    } else if (sched == KMP_SCH_STATIC || sched == KMP_SCH_STATIC_CHUNKED ||
               sched == KMP_DISTRIBUTE_STATIC ||
               sched == KMP_DISTRIBUTE_STATIC_CHUNKED) {
      ili = ll_make_kmpc_for_static_fini();
      iltb.callfg = 1;
      chk_block(ili);
    }
    break;
  }
  case IM_BPDO:
  case IM_EPDO:
    break;

  case IM_PDO:
    if (ll_par_cnt >= 1)
      break;
    sym = ILM_OPND(ilmp, 1);
    if (ILIBLKG(sym))
      BIH_PARLOOP(ILIBLKG(sym)) = 1;
    switch (ILM_OPND(ilmp, 2) & 0xff) {
    case 6: /* distribute static schedule */
    case 0: /* static schedule */
      switch (ILM_OPND(ilmp, 2) & 0xff00) {
      case 0:
        doschedule = "static block";
        break;
      case MP_SCH_CHUNK_1:
        doschedule = "static cyclic";
        break;
      case MP_SCH_BLK_CYC:
        doschedule = "static block-cyclic";
        break;
      case MP_SCH_BLK_ALN:
        /* also PARALN */
        doschedule = "static block";
        break;
      default:
        doschedule = "";
        break;
      }
      break;
    case 1:
      doschedule = " dynamic";
      break;
    case 2:
      doschedule = " guided";
      break;
    case 3:
      doschedule = " interleaved"; /* not used */
      break;
    case 4:
      doschedule = " runtime schedule";
      break;
    case 5:
      doschedule = " auto schedule";
      break;
    default:
#if DEBUG
      interr("exp_smp: IM_PDO unknown schedule", ILM_OPND(ilmp, 2) & 0xff, 3);
#endif
      doschedule = " static";
    }
    if ((ILM_OPND(ilmp, 2) & 0xff) == 6) {
      ccff_info(MSGOPENMP, "OMP024", gbl.findex, gbl.lineno,
              "Distribute loop activated with %schedule schedule", "schedule=%s",
              doschedule, NULL);
      break;
    }
      ccff_info(MSGOPENMP, "OMP014", gbl.findex, gbl.lineno,
              "Parallel loop activated with %schedule schedule", "schedule=%s",
              doschedule, NULL);
    break;
  case IM_BARRIER:
    if (ll_par_cnt >= 1)
      break;
    else if (!XBIT(183, 0x2000)) { /* If kmpc enabled */
      ili = ll_make_kmpc_barrier();
      iltb.callfg = 1;
    }
    chk_block(ili);
    ccff_info(MSGOPENMP, "OMP015", gbl.findex, gbl.lineno, "Barrier", NULL);
    break;
  case IM_BSECTIONS:
    if (!ll_ilm_is_rewriting()) {
      ccff_info(MSGOPENMP, "OMP005", gbl.findex, gbl.lineno, "Begin sections",
                NULL);

      if (flg.opt != 0) {
        wr_block();
        cr_block();
      }
      parsect_cnt++;
      BIH_PARSECT(expb.curbih) = bihb.parsectfg = TRUE;
      exp_smp_section_init();
      wr_block();
      cr_block();
    }
    break;
  case IM_MASTER:
    if (ll_par_cnt >= 1)
      break;
    ccff_info(MSGOPENMP, "OMP008", gbl.findex, gbl.lineno,
              "Begin master region", NULL);

    parsect_cnt++;
    if (flg.opt != 0) {
      wr_block();
      cr_block();
    }
    sym = ILM_OPND(ilmp, 1);
    ili = ll_make_kmpc_master();
    ili = ad3ili(IL_ICJMPZ, ili, CC_EQ, sym);
    iltb.callfg = 1;
    BIH_PARSECT(expb.curbih) = bihb.parsectfg = TRUE;
    chk_block(ili);
    break;
  case IM_SECTION:
    if (!ll_ilm_is_rewriting()) {
      ccff_info(MSGOPENMP, "OMP006", gbl.findex, gbl.lineno, "New section",
                NULL);

      if (SECT_CNT == 0) { /* first section make call */
        /* we should know lower bound but don't know upper bound */
        /* make a call to static_for_init here - we will fill upper bound later
         */
        int *args, lb, ub, st, last;
        wr_block();
        cr_block();
        ili = ad4ili(IL_ST, ad_icon(0), ad_acon(SECT_LB, 0),
                     addnme(NT_VAR, SECT_LB, 0, 0), MSZ_WORD);
        chk_block(ili);
        ili = ad4ili(IL_ST, ad_icon(1), ad_acon(SECT_ST, 0),
                     addnme(NT_VAR, SECT_ST, 0, 0), MSZ_WORD);
        chk_block(ili);
        args = ll_make_sections_args(SECT_LB, SECT_UB, SECT_ST, SECT_LAST);
        ili = ll_make_kmpc_for_static_init_args(DT_UINT, args);
        iltb.callfg = 1;
        chk_block(ili);
      }

      /*
       * if (lb != cnt)
       *   jmp to next label
       */
      wr_block();
      cr_block();
      exp_label(ILM_OPND(ilmp, 3));
      BIH_LABEL(expb.curbih) = ILM_OPND(ilmp, 3);
      ILIBLKP(BIH_LABEL(expb.curbih), expb.curbih);

      ili = section_create_block(ILM_OPND(ilmp, 2), SECT_LB, SECT_UB, SECT_CNT);
      chk_block(ili);
      RFCNTI(ILM_OPND(ilmp, 2));

      ++SECT_CNT;
    }
    break;
  case IM_LSECTION:
    if (!ll_ilm_is_rewriting()) {
      wr_block();
      cr_block();
      exp_label(ILM_OPND(ilmp, 3));
      BIH_LABEL(expb.curbih) = ILM_OPND(ilmp, 3);
      ILIBLKP(BIH_LABEL(expb.curbih), expb.curbih);
      wr_block();
      cr_block();

      /* now assign the upper bound to SECT_UB */
      savebih = expb.curbih;
      savex14 = flg.x[14];
      flg.x[14] |= 0x1000;
      wr_block();
      expb.curbih = SECT_BBIH;
      rdilts(expb.curbih);
      expb.curilt = ILT_PREV(0);
      ili = ad4ili(IL_ST, ad_icon(SECT_CNT - 1), ad_acon(SECT_UB, 0),
                   addnme(NT_VAR, SECT_UB, 0, 0), MSZ_WORD);
      expb.curilt = addilt(expb.curilt, ili);
      wrilts(SECT_BBIH);
      expb.curbih = savebih;
      rdilts(expb.curbih);
      expb.curilt = ILT_PREV(0);
      flg.x[14] = savex14;
    }
    break;
  case IM_ESECTIONS:
    if (!ll_ilm_is_rewriting()) {
      exp_smp_section_end();
      ccff_info(MSGOPENMP, "OMP007", gbl.findex, gbl.lineno, "End sections",
                NULL);
      goto esect_shared;
    }
    break;
  case IM_CANCEL:
    if (!ll_ilm_is_rewriting()) {
      int ifcancel = ILI_OF(ILM_OPND(ilmp, 3));
      int cancel_kind = ILM_OPND(ilmp, 2);
      int label = ILM_OPND(ilmp, 1);

      int cancel_label = getlab();
      ifcancel = ad3ili(IL_ICJMPZ, ifcancel, CC_EQ, cancel_label);
      RFCNTI(cancel_label);
      chk_block(ifcancel);

      ili = ll_make_kmpc_cancel(ad_icon(cancel_kind));
      ifcancel = ad3ili(IL_ICJMPZ, ili, CC_NE, label);
      iltb.callfg = 1;
      chk_block(ifcancel);

      wr_block();
      cr_block();
      exp_label(cancel_label);
      ccff_info(MSGOPENMP, "OMP026", gbl.findex, gbl.lineno, "Cancel", NULL);
    }
    break;
  case IM_CANCELPOINT:
    if (!ll_ilm_is_rewriting()) {
      int cancel_kind = ILM_OPND(ilmp, 2);
      int label = ILM_OPND(ilmp, 1);
      ili = ll_make_kmpc_cancellationpoint(ad_icon(cancel_kind));
      ili = ad3ili(IL_ICJMPZ, ili, CC_NE, label);
      iltb.callfg = 1;
      chk_block(ili);
      ccff_info(MSGOPENMP, "OMP027", gbl.findex, gbl.lineno,
                "Cancellation point", NULL);
    }
    break;
  case IM_SINGLE:
    if (flg.opt != 0) {
      wr_block();
      cr_block();
    }
    if (!ll_ilm_is_rewriting()) {
      parsect_cnt++;
      ccff_info(MSGOPENMP, "OMP010", gbl.findex, gbl.lineno,
                "Begin single region", NULL);
      single_thread = ll_get_private_temp(DT_INT);
      in_single = ll_get_private_temp(DT_INT);
      ili = gen_int_store(single_thread, ad_icon(-1));
      chk_block(ili);
      ili = gen_int_store(in_single, ad_icon(0));
      chk_block(ili);
      if (!gbl.outlined) {
        SCP(single_thread, SC_AUTO);
        SCP(in_single, SC_AUTO);
      }
      ili = ll_make_kmpc_single();
      sym = ILM_OPND(ilmp, 2);
      ili = ad3ili(IL_ICJMPZ, ili, CC_EQ, sym), iltb.callfg = 1;
      BIH_PARSECT(expb.curbih) = bihb.parsectfg = TRUE;
      chk_block(ili);
    }
    break;
  case IM_EMASTER:
    if (ll_par_cnt >= 1)
      break;
    ili = ll_make_kmpc_end_master();
    iltb.callfg = 1;
    chk_block(ili);
    ccff_info(MSGOPENMP, "OMP009", gbl.findex, gbl.lineno, "End master region",
              NULL);
    goto esect_shared;
  case IM_ESINGLE:
    if (flg.opt != 0) {
      wr_block();
      cr_block();
    }
    if (!ll_ilm_is_rewriting()) {
      int threadili;
      threadili = ll_get_gtid_val_ili();
      ili = gen_int_store(single_thread, threadili);
      chk_block(ili);
      ili = gen_int_store(in_single, ad_icon(1));
      chk_block(ili);
      ili = ll_make_kmpc_end_single();
      iltb.callfg = 1;
      chk_block(ili);
      ccff_info(MSGOPENMP, "OMP011", gbl.findex, gbl.lineno,
                "End single region", NULL);
    } else {
      break;
    }

  esect_shared:
    BIH_PARSECT(expb.curbih) = TRUE;
    exp_label(ILM_OPND(ilmp, 1));
    parsect_cnt--;
    if (parsect_cnt <= 0)
      bihb.parsectfg = FALSE;
    break;

  /* C, FORTRAN */
  case IM_BCOPYIN:
  case IM_ECOPYIN:
    if (!ll_ilm_is_rewriting()) {
      if (opc == IM_ECOPYIN) {
        const int n = sptr_list_length(copysptr_list);

        if (XBIT(69, 0x80)) {
          make_copypriv_array_tls(copysptr_list);
          sptr_list_free(&copysptr_list);
          break;
        }

        addCopyinInplace(copysptr_list);
        sptr_list_free(&copysptr_list);
        break;
      }
    }
    break;

  case IM_COPYIN:
    if (ll_ilm_is_rewriting()) {
      break;
    }
    sym = ILM_OPND(ilmp, 1); /* variable/common block to be copied */
    tpv = MIDNUMG(sym);

    if (STYPEG(sym) == ST_CMBLK) {
      /* entire common block is being copied */
      size = SIZEG(sym);
      off = ad1ili(IL_ICON, stb.i0);
    } else if (SCG(sym) == SC_CMBLK) {
      /* a variable of the common block is being copied */
      size = size_of(DTYPEG(sym));
      /* locate common block */
      sym = MIDNUMG(sym);
      tpv = MIDNUMG(sym);
    }
    else if (SCG(sym) == SC_BASED && POINTERG(sym)) {
      pv = MIDNUMG(sym);
      if (SCG(pv) == SC_CMBLK) {
        /* f90 pointer or allocatable common block member:
         *
         * MIDNUM locates the user/compiler-created pointer;
         * its MIDNUM locates the common block.
         */
        int sdsptr;
        size = size_of(DTYPEG(pv));
        sdsptr = SDSCG(sym); /* $sd */
        if (sdsptr) {
          size += size_of(DT_ADDR);        /* $o */
          size += size_of(DTYPEG(sdsptr)); /* $sd */
        }
        ADDRTKNP(sym, 1);
        tpv = MIDNUMG(MIDNUMG(pv));
      } else {
        /* f90 pointer or allocatable:
         *
         * MIDNUM locates the user/compiler-created pointer;
         * its MIDNUM locates the variable's thread pointer vector
         * Could compute the size of that variable or just of the
         * pointer dtype ...
         */
        size = size_of(DT_ADDR);
#if DEBUG
        assert(size == size_of(DTYPEG(pv)),
               "COPYIN size incorrect for SC_BASED sym", sym, 4);
#endif
        ADDRTKNP(sym, 1);
        tpv = MIDNUMG(pv);
      }
    }
    else if (SCG(sym) == SC_BASED) {
      /* Cray pointee:
       * MIDNUM locates the variable's thread pointer vector and
       * its MIDNUM locates the user/compiler-created pointer that's
       * actually copied.  Could compute the size of that variable
       * or just of the pointer dtype ...
       */
      size = size_of(DT_ADDR);
#if DEBUG
      assert(size == size_of(DTYPEG(MIDNUMG(MIDNUMG(sym)))),
             "COPYIN size incorrect for SC_BASED sym", sym, 4);
#endif
      ADDRTKNP(sym, 1);
    } else {
      /* regular user var being copied */
      size = size_of(DTYPEG(sym));
      ADDRTKNP(sym, 1);
    }
    /* FALSE: Because we want to always use the vector/cache (tpv)
     * and not the data item from the cache.
     */
    sz = ad_kconi(size);
    sptr_list_add(&copysptr_list, tpv, sz, FALSE, 0, 0, sym);
    break;

#ifdef IM_COPYIN_A
  case IM_COPYIN_A:
    if (ll_ilm_is_rewriting()) {
      break;
    }
    sym = ILM_OPND(ilmp, 1); /* allocatable to be copied */
                             /*
                              * MIDNUM locates the user/compiler-created pointer;
                              * its MIDNUM locates the variable's thread pointer vector
                              */
    pv = MIDNUMG(sym);
    if (SCG(sym) == SC_BASED && POINTERG(sym)) {
      if (SCG(pv) == SC_CMBLK) {
        /* f90 pointer or allocatable common block member:
         *
         * MIDNUM(sym)locates the user/compiler-created pointer (pv)
         * which is a member of the common block.  Its MIDNUM locates
         * the common block.
         */
        pv = MIDNUMG(pv); /* locate common block */
      }
    }
    sz = ILI_OF(ILM_OPND(ilmp, 2));
    ADDRTKNP(sym, 1);
    tpv = MIDNUMG(pv);

    sptr_list_add(&copysptr_list, tpv, sz, FALSE, 0, 0, sym);
    break;
#endif

  case IM_BCOPYPRIVATE:
  case IM_ECOPYPRIVATE:
    if (!ll_ilm_is_rewriting()) {
      if (opc == IM_ECOPYPRIVATE) {
        addr = make_copypriv_array(copysptr_list, TRUE);
        stili = gen_int_load(in_single);

        /* c++ will set up assign_rou from IM_COPYPRIVATE_CL (_P) */
        if (!assign_rou) {
          assign_rou = ad_acon(mkfunc("_mp_copypriv_kmpc"), 0);
        }

        ili = ll_make_kmpc_copyprivate(addr, stili, assign_rou);

        assign_rou = 0;
        iltb.callfg = 1;
        chk_block(ili);
        sptr_list_free(&copysptr_list);
      }
    }
    break;
  case IM_COPYPRIVATE_CL_P:
    if (ll_ilm_is_rewriting()) {
      break;
    }
    /* C++ ONLY class copyprivate */
    argilm = ILM_OPND(ilmp, 2);
    sym = ILM_OPND((ILM *)(ilmb.ilm_base + argilm), 1);
    assign_rou = ad_acon(ILM_OPND(ilmp, 3), 0);
    if (DTY(DTYPEG(sym)) == TY_ARRAY) {
      element_size = get_elem_size(DTYPEG(sym));
      num_elements = extent_of(DTYPEG(sym));
      size = num_elements * element_size; /* Total size required for
                                                        llvm memcpy */
    } else {
      size = size_of(DTYPEG(sym)); /* used for POD */
    }
    sz = ad_kconi(size);
    sptr_list_add(&copysptr_list, sym, sz, FALSE, assign_rou, 0, sym);
    ADDRTKNP(sym, 1);
    break;

#ifdef IM_COPYPRIVATE_PA
  case IM_COPYPRIVATE_PA:
    if (ll_ilm_is_rewriting()) {
      break;
    }
    argilm = ILM_OPND(ilmp, 2);
    sym = ILM_OPND((ILM *)(ilmb.ilm_base + argilm), 1);
    ili = ILI_OF(ILM_OPND(ilmp, 3));
    ili = sel_iconv(ili, 1);

    sptr_list_add(&copysptr_list, sym, ili, FALSE, 0, 0, sym);
    break;
#endif

  case IM_COPYPRIVATE_P:
    if (ll_ilm_is_rewriting()) {
      break;
    }
    argilm = ILM_OPND(ilmp, 2);
    sym = ILM_OPND((ILM *)(ilmb.ilm_base + argilm), 1);
    size = size_of(DTYPEG(sym));
    sz = ad_kconi(size);
    sptr_list_add(&copysptr_list, sym, sz, FALSE, 0, 0, sym);
    break;

  case IM_COPYPRIVATE_CL:
    if (ll_ilm_is_rewriting()) {
      break;
    }
    /* C++ ONLY class copyprivate */
    sym = ILM_OPND(ilmp, 2); /* variable/class to be copied out */
    assign_rou = ad_acon(ILM_OPND(ilmp, 3), 0);
    if (DTY(DTYPEG(sym)) == TY_ARRAY) {
      element_size = get_elem_size(DTYPEG(sym));
      num_elements = extent_of(DTYPEG(sym));
      size = num_elements * element_size; /* Total size required for
                                             llvm memcpy */
    } else {
      size = size_of(DTYPEG(sym)); /* used for POD */
    }
    sz = ad_kconi(size);
    sptr_list_add(&copysptr_list, sym, sz, FALSE, assign_rou, 0, sym);
    ADDRTKNP(sym, 1);
    break;

  case IM_COPYPRIVATE:
    if (ll_ilm_is_rewriting()) {
      break;
    }

    sym = ILM_OPND(ilmp, 2); /* variable/common block to be copied out */
    is_cmblk = FALSE;

    if (STYPEG(sym) == ST_CMBLK) {
      /* Entire common block */
      size = SIZEG(sym);
      sym = MIDNUMG(sym);
      is_cmblk = TRUE;
    } else if (SCG(sym) == SC_CMBLK) {
      /* Var in common block */
      size = size_of(DTYPEG(sym));
      sym = MIDNUMG(sym);
      sym = MIDNUMG(sym);
      is_cmblk = TRUE;
    } else {
      size = size_of(DTYPEG(sym));
    }
    sz = ad_kconi(size);
    sptr_list_add(&copysptr_list, sym, sz, is_cmblk, 0, 0, sym);
    ADDRTKNP(sym, 1);
    break;

#ifdef IM_FLUSH
  case IM_FLUSH:
    if (ll_ilm_is_rewriting()) {
      break;
    }
    ili = ll_make_kmpc_flush();
    iltb.callfg = 1;
    chk_block(ili);
    break;
#endif
  case IM_TASKGROUP:
    if (ll_ilm_is_rewriting()) {
      break;
    }
    ili = ll_make_kmpc_taskgroup();
    iltb.callfg = 1;
    chk_block(ili);
    break;
  case IM_ETASKGROUP:
    if (ll_ilm_is_rewriting()) {
      break;
    }
    ili = ll_make_kmpc_end_taskgroup();
    iltb.callfg = 1;
    chk_block(ili);
    break;

  case IM_BTASKLOOP:
    if (ll_ilm_is_rewriting()) {
      break;
    }
    set_istaskloop();
    goto shared_task;
  case IM_BTASK:
    if (ll_ilm_is_rewriting()) {
      break;
    }
shared_task:
    wr_block();
    cr_block();

    task_cnt++;
    task_bv = ILM_OPND(ilmp, 2);
    task_ifv = ILI_OF(ILM_OPND(ilmp, 3));
    task_alloc_sptr = getnewccsym('z', GBL_CURRFUNC, ST_VAR);
    if (gbl.outlined)
      SCP(task_alloc_sptr, SC_PRIVATE);
    else
      SCP(task_alloc_sptr, SC_AUTO);
    DTYPEP(task_alloc_sptr, DT_CPTR);

    task_flags = ll_get_private_temp(DT_INT);
    if (gbl.outlined)
      SCP(task_flags, SC_PRIVATE);
    else
      SCP(task_flags, SC_AUTO);
    /* Note: kmpc(5.0) does not use mergeable and priority flags */
    if (task_bv & MP_TASK_FINAL) {
      const int kmpc_flags = mp_to_kmpc_tasking_flags(task_bv);

      /* Expand the 'final' expression */
      const int lab = getlab();
      RFCNTI(lab);
      ili = ad3ili(IL_ICJMPZ, ILI_OF(ILM_OPND(ilmp, 4)), CC_EQ, lab);
      chk_block(ili);

      /* In the branch: update the flags variable */
      ili = gen_int_store(task_flags, ad_icon(kmpc_flags));
      chk_block(ili);
      wr_block();
      exp_label(lab);
    } else {
      if (task_bv & MP_TASK_UNTIED) {
        ili = ad_icon(0);
      } else {
        ili = ad_icon(1);
      }
      ili = gen_int_store(task_flags, ili);
      chk_block(ili);
    }

    /* mark for __kmpc_task_alloc */
    NEED(mppgcnt + 1, mppgbih, int, mppgbih_siz, mppgbih_siz + 16);
    mppgbih[mppgcnt] = expb.curbih;
    mppgcnt++;

    wr_block();
    cr_block();

    /* create task here because we want to set ENCLFUNC for all firstprivate
     * variable and loop variables(for taskloop)*/
    task = llmp_get_task(scope_sptr);
    if (!task)
      task = llmp_create_task(scope_sptr);
    assert(task, "No task associated to this scope sptr", scope_sptr, 4);
    task_fnsptr = ll_make_outlined_task(uplevel_sptr, scope_sptr);
    llmp_task_set_fnsptr(task, task_fnsptr);
    if (!PARENCLFUNCG(scope_sptr))
      PARENCLFUNCP(scope_sptr, task_fnsptr);
    if (opc == IM_BTASKLOOP) {
      /* Reserve space for taskloop vars & lastiter on task_alloc ptr.  */
        TASK_LPVAR_OFFSET = llmp_task_add_loopvar(task, 4, DT_INT8);

      if (task_bv & MP_TASK_IF) {
        int lab = getlab();
        RFCNTI(lab);
        TASKLP_IF = ad_icon(1);
        ili = ad3ili(IL_ICJMPZ, task_ifv, CC_NE, lab);
        chk_block(ili);
        TASKLP_IF = ad_icon(0);
        exp_label(lab);
      } else {
        TASKLP_IF = ad_icon(0);
      }
      if (task_bv & MP_TASK_NOGROUP) {
        TASKLP_NOGROUP = ad_icon(1);
      } else  {
        TASKLP_NOGROUP = ad_icon(0);
      }
      if (task_bv & MP_TASK_GRAINSIZE) {
        TASKLP_SCHED = ad_icon(1);
      } else if (task_bv & MP_TASK_NUM_TASKS) {
        TASKLP_SCHED = ad_icon(2);
      } else {
        TASKLP_SCHED = ad_icon(0);
      }
      TASKLP_GRAINSIZE = ILI_OF(ILM_OPND(ilmp, 6));
    }

    break;

/* Add a first private variable to our internal representation
 * of a task.
 */
#ifdef IM_TASKFIRSTPRIV
  case IM_TASKFIRSTPRIV:
    if (ll_ilm_is_rewriting())
      break;

    if (is_taskloop()) {
      /* code between TASKFIRSTPRIV and ETASKFIRST will be added to taskdup routine */
      start_taskdup(task_fnsptr, curilm);
    }
    sym = ILM_OPND(ilmp, 1);
    sptr = ILM_OPND(ilmp, 2);

    {
      LLTask *task = llmp_get_task(scope_sptr);
      if (!task)
        task = llmp_create_task(scope_sptr);
      offset = llmp_task_add_firstprivate(task, sym, sptr);
      ADDRESSP(sptr, offset);
      ENCLFUNCP(sptr, task_fnsptr);
    }
    break;
#endif
#ifdef IM_ETASKFIRSTPRIV
  case IM_ETASKFIRSTPRIV:
    if (ll_ilm_is_rewriting())
      break;

    if (is_taskloop()) {
      stop_taskdup(task_fnsptr);
    }
#endif

    break;
  case IM_TASKREG:
    if (flg.opt != 0) {
      wr_block();
      cr_block();
    }
    ccff_info(MSGOPENMP, "OMP016", gbl.findex, gbl.lineno, "Begin task", NULL);

    incr_ll_par_cnt();

    if (ll_par_cnt > 1)
      ll_rewrite_ilms(-1, curilm, 0);
    else if (ll_par_cnt == 1) {
      ll_write_ilm_header(task_fnsptr, curilm);
    }
    break;
  case IM_TASKLOOPREG:
    if (flg.opt != 0) {
      wr_block();
      cr_block();
    }
    ccff_info(MSGOPENMP, "OMP028", gbl.findex, gbl.lineno, "Begin taskloop", NULL);
    incr_ll_par_cnt();

    if (ll_par_cnt > 1) {
      ll_rewrite_ilms(-1, curilm, 0);
      break;
    } else if (ll_par_cnt == 1) {
      ll_write_ilm_header(task_fnsptr, curilm);
    }
    TASK_LB = ILI_OF(ILM_OPND(ilmp, 1));
    TASK_UB = ILI_OF(ILM_OPND(ilmp, 2));
    TASK_ST = ILI_OF(ILM_OPND(ilmp, 3));

    break;

  case IM_ETASKREG:
    if (ll_par_cnt == 1) {
      ilm_outlined_end_write(curilm);
      ccff_info(MSGOPENMP, "OMP017", gbl.findex, gbl.lineno, "End task", NULL);
    }
    decr_ll_par_cnt();
    if (ll_par_cnt >= 1) {
      ll_rewrite_ilms(-1, curilm, 0);
      break;
    }
    if (gbl.outlined)
      expb.sc = SC_PRIVATE;
    else
      expb.sc = SC_AUTO;
    break;
  case IM_ETASKLOOPREG:
    if (ll_par_cnt == 1) {
      ilm_outlined_end_write(curilm);
      ccff_info(MSGOPENMP, "OMP029", gbl.findex, gbl.lineno, "End taskloop", NULL);
    }
    decr_ll_par_cnt();
    if (ll_par_cnt >= 1) {
      ll_rewrite_ilms(-1, curilm, 0);
      break;
    }
    set_istaskloop();
    if (gbl.outlined)
      expb.sc = SC_PRIVATE;
    else
      expb.sc = SC_AUTO;
    break;

  case IM_ETASKLOOP:
    if (ll_ilm_is_rewriting())
      break;

  case IM_ETASK:
    if (ll_ilm_is_rewriting())
      break;

    /* Insert kmpc_task_alloc here because default firstprivate assignment can
     * be done after IM_ETASKREG/ETASKLOOPREG and we need to collect 
     * the size of all firstprivate vars and pass to kmpc.
     */

    {
      int lab, end_lab, s_scope;

      resetMppBih(SET_MPPBIH, IS_PREVMPPG, NOTUSE_NEXTBIH);
      /* Load args first */
      s_scope = scope_sptr;
      scope_sptr = OUTLINEDG(task_fnsptr);
      ili_arg = ll_load_outlined_args(scope_sptr, task_fnsptr, FALSE);

      task_alloc_sptr = ll_make_kmpc_task_arg(task_alloc_sptr, task_fnsptr,
                                              scope_sptr, task_flags, ili_arg);
      /* Load taskloop values and store onto task_alloc ptr 
       * Also get its address on task_alloc ptr to pass
       * to __kmpc_taskloop.
       */
      if (opc == IM_ETASKLOOP) {
        int nme, ldnme, task_ili, addr, ilix;
        ILI_OP ld, st;
        MSZ msz;
        INT offset=0;

        ili = ad_acon(task_alloc_sptr, offset);
        nme = addnme(NT_VAR, task_alloc_sptr, (INT)0, 0);
        task_ili = ad2ili(IL_LDA, ili, nme);
        ldst_msz(DT_INT8, &ld, &st, &msz);

        offset = ad_aconi(TASK_LPVAR_OFFSET);
        ili = ad3ili(IL_AADD, task_ili, offset, 0);
        TASKLP_LB = ili;
        ili = ad4ili(st, TASK_LB, ili, nme, msz);
        chk_block(ili);

        offset = ad_aconi(TASK_LPVAR_OFFSET+zsize_of(DT_INT8));
        ili = ad3ili(IL_AADD, task_ili, offset, 0);
        TASKLP_UB = ili;
        ili = ad4ili(st, TASK_UB, ili, nme, msz);
        chk_block(ili);

        offset = ad_aconi(TASK_LPVAR_OFFSET+(zsize_of(DT_INT8)*2));
        ili = ad3ili(IL_AADD, task_ili, offset, 0);
        ili = ad4ili(st, TASK_ST, ili, nme, msz);
        TASKLP_ST = TASK_ST;

        chk_block(ili);
      } 

      resetMppBih(RESTORE_MPPBIH, IS_PREVMPPG, NOTUSE_NEXTBIH);
      scope_sptr = s_scope;

      /* If 'if' clause is used, this is the false branch, if (0) then... */
      end_lab = ILM_OPND(ilmp, 1);
      if (opc == IM_ETASK) {
         if (task_bv & MP_TASK_IF) {
          lab = getlab();
          RFCNTI(lab);
          ili = ad3ili(IL_ICJMPZ, task_ifv, CC_NE, lab);
          chk_block(ili);

          iltb.callfg = 1; /* Begin */
          ili = ll_make_kmpc_task_begin_if0(task_alloc_sptr);
          chk_block(ili);

          iltb.callfg = 1; /* Call task */
          ili = ll_make_outlined_task_call(task_fnsptr, task_alloc_sptr);
          chk_block(ili);

          iltb.callfg = 1; /* End */
          ili = ll_make_kmpc_task_complete_if0(task_alloc_sptr);
          chk_block(ili);

          /* Create and jump to an end label at the end of the task */
          RFCNTI(end_lab);
          ili = ad1ili(IL_JMP, end_lab);
          chk_block(ili);

          exp_label(lab);
        }
      }
      wr_block();
      cr_block();
      if (opc == IM_ETASK) {
        /* Make api call */
        ili = ll_make_kmpc_task(task_alloc_sptr);
      } else {
        TASKLP_TASK = ad2ili(IL_LDA, ad_acon(task_alloc_sptr, 0),
                             addnme(NT_VAR, task_alloc_sptr, 0, 0));
        if (TASKDUPG(task_fnsptr)) {
          TASKLP_TASKDUP = ad2ili(IL_LDA, ad_acon(TASKDUPG(task_fnsptr), 0),
                                  addnme(NT_VAR, task_fnsptr, 0, 0));
        } else {
          TASKLP_TASKDUP = 0;
        }
        ili = ll_make_kmpc_taskloop(TASKLPARGS);
        clear_taskloop_info();
      }
      iltb.callfg = 1;
      chk_block(ili);
    }

    /* reset once done processing a task - need this for mk_address to work */
    task_alloc_sptr = 0;

    task_cnt--;
    mppgcnt--;

    exp_label(ILM_OPND(ilmp, 1));
    break;

  case IM_TASKWAIT:
    if (ll_ilm_is_rewriting())
      break;
    ccff_info(MSGOPENMP, "OMP018", gbl.findex, gbl.lineno, "Taskwait", NULL);
    ili = ll_make_kmpc_task_wait();
    iltb.callfg = 1;
    chk_block(ili);
    break;

  case IM_TASKYIELD:
    if (ll_ilm_is_rewriting())
      break;
    ccff_info(MSGOPENMP, "OMP019", gbl.findex, gbl.lineno, "Taskyield", NULL);
    ili = ll_make_kmpc_task_yield();
    iltb.callfg = 1;
    chk_block(ili);
    break;

  case IM_BMPPG:
    if (ll_ilm_is_rewriting())
      break;

    /* create a block for kmpc_task_alloc */

    NEED(mppgcnt + 1, mppgbih, int, mppgbih_siz, mppgbih_siz + 16);
    mppgbih[mppgcnt] = expb.curbih;
    mppgcnt++;

    /* for task call */
    wr_block();
    cr_block();
    break;

  case IM_EMPPG:
    if (ll_ilm_is_rewriting())
      break;
    mppgcnt--;
    break;

  case IM_BAMPPG:
    if (ll_ilm_is_rewriting())
      break;

    resetMppBih(SET_MPPBIH, IS_PREVMPPG, NOTUSE_NEXTBIH);

    break;

  case IM_EAMPPG:
    if (ll_ilm_is_rewriting())
      break;

    resetMppBih(RESTORE_MPPBIH, IS_PREVMPPG, NOTUSE_NEXTBIH);
    break;

  case IM_TARGETUPDATE:
  /* don't update if false - currently has no effect because host is device */
    break;

  case IM_BTARGETDATA:
  case IM_TARGETENTERDATA:
  case IM_TARGETEXITDATA:
    dotarget = ILI_OF(ILM_OPND(ilmp, 1));
    beg_label = getlab();
    end_label = getlab();

    dotarget = ad3ili(IL_ICJMPZ, dotarget, CC_EQ, end_label);
    RFCNTI(end_label);
    chk_block(dotarget);

    wr_block();
    cr_block();
    exp_label(beg_label);

    /* .... TODO: call to runtime target data here  */

    exp_label(end_label);

    break;
  case IM_ETARGETDATA:
    break;
  case IM_BDISTRIBUTE:
    if (ll_ilm_is_rewriting())
    ccff_info(MSGOPENMP, "OMP024", gbl.findex, gbl.lineno,
              "Distribute loop activated", NULL);
    break;
  case IM_EDISTRIBUTE:
    if (ll_ilm_is_rewriting())
    ccff_info(MSGOPENMP, "OMP025", gbl.findex, gbl.lineno,
              "Distribute loop terminated", NULL);
      break;
    break;

  case IM_MP_ATOMIC:
    if (ll_ilm_is_rewriting())
      break;
    wr_block();
    cr_block();
    bihb.csfg = BIH_CS(expb.curbih) = TRUE;
    break;
  case IM_MP_ENDATOMIC:
    if (ll_ilm_is_rewriting())
      break;
    wr_block();
    cr_block();
    bihb.csfg = BIH_CS(expb.curbih) = FALSE;
    break;

  case IM_MP_ATOMICREAD:
    if (ll_ilm_is_rewriting())
      break;
    ILM_RESULT(curilm) = exp_mp_atomic_read(ilmp);
    break; 

  case IM_MP_ATOMICWRITE:
    if (ll_ilm_is_rewriting())
      break;
    exp_mp_atomic_write(ilmp);
    break;
  case IM_MP_ATOMICUPDATE:
    if (ll_ilm_is_rewriting())
      break;
    exp_mp_atomic_update(ilmp);
    break;
  case IM_MP_ATOMICCAPTURE:
    if (ll_ilm_is_rewriting())
      break;
    exp_mp_atomic_capture(ilmp);
    break;

  default:
    interr("exp_smp: unsupported opc", opc, 3);
    break;
  }

#endif /* end #ifdef IM_BPAR */

}

/* opc: IL_DAIR/IL_DAAR/IL_DADP/IL_DASP/IL_ARGxx (x86).
 * Add argument expression argili to existing argument list arglist
 * using opcode opc. If arglist = 0, begin a new list.
 */
static int
jsr_add_arg(int arglist, int opc, int argili)
{
  int rg;
  int ili;

  if (arglist == 0) {
    arglist = ad1ili(IL_NULL, 0);
    avail_ireg = 0;
    avail_freg = 0;
  }
  /*
   * WARNING: For the x86, this implies that the standard call mechanism is
   * being used.  If there are multiple arguments, they need to be pushed
   * on the stack in reverse order (first jsr_add_arg() call is for the last
   * argument, ...).
   */
  switch (opc) {
  case IL_ARGAR:
    ili = ad3ili(IL_ARGAR, argili, arglist, 0);
    return ili;
  case IL_ARGIR:
  case IL_ARGKR:
  case IL_ARGSP:
  case IL_ARGDP:
    ili = ad2ili(opc, argili, arglist);
    return ili;
  default:
    /* allow arguments to be passed in registers and on the stack */
    break;
  }
  assert(is_daili_opcode(opc), "jsr_add_arg: invalid opcode", opc, 4);
  if (opc == IL_DAIR || opc == IL_DAAR || opc == IL_DAKR)
    rg = IR(avail_ireg++);
  else {
    if (opc == IL_DADP && (avail_freg & 1))
      avail_freg++;
    rg = SP(avail_freg);
    avail_freg++;
    if (opc == IL_DADP)
      avail_freg++;
  }

  ili = ad3ili(opc, argili, rg, arglist);
  return ili;
}

/* opc: IL_DFRDP / IL_DFRSP, depending on result type of call */
/* Return the ili of a call to a function that returns a result, using ili
 * callili, followed by freeing of the appropriate argument registers with
 * opcode opc. */
static int
make_call_result(int opc, int callili)
{
  int rg;
  int ili;

  assert(IL_DFRIR <= opc && opc <= IL_DFRAR, "make_call_result: invalid opcode",
         opc, 4);

  switch (opc) {
  case IL_DFRIR:
    rg = IR_RETVAL;
    break;
  case IL_DFRSP:
    rg = SP_RETVAL;
    break;
  case IL_DFRDP:
    rg = DP_RETVAL;
    break;
  case IL_DFRAR:
    rg = AR_RETVAL;
    break;
  default:
    interr("make_call_result: invalid register free opcode", opc, 4);
  }
  ili = ad2ili(opc, callili, rg);

  return ili;
}

/** \brief Return the ili of a call to a function with name fname, and argument
 * list argili. If argili = 0, argument list is empty.
 *
 * \param fname  function name
 * \param opc    IL_QJSR/IL_JSR
 * \param argili argument list
 */
static int
make_call(char *fname, int opc, int argili)
{
  int ili;
  LOGICAL old_share_proc, old_share_qjsr;

  if (argili == 0) {
    argili = ad1ili(IL_NULL, 0);
    avail_ireg = 0;
    avail_freg = 0;
  }

  old_share_proc = share_proc_ili;
  old_share_qjsr = share_qjsr_ili;
  share_proc_ili = FALSE;
  share_qjsr_ili = FALSE;
  ili = ad2ili(opc, mkfunc(fname), argili);
  share_proc_ili = old_share_proc;
  share_qjsr_ili = old_share_qjsr;

  if (avail_freg > 0 && avail_freg < 4)
    avail_freg = 4;
  if (avail_ireg > max_ireg)
    max_ireg = avail_ireg;
  if (avail_freg > max_freg)
    max_freg = avail_freg;

  return ili;
}

int
lcpu_temp(int sc)
{
  int sym;
  char name[10];
  static int lcpu_cnt = 0; /* counter for lcpu temporaries */

  strcpy(name, ".lcp");
  sprintf(&name[4], "%05d", lcpu_cnt);
  lcpu_cnt++;
  sym = getcctemp_sc(name, ST_VAR,
                     sc); /* lcpu variable, 1 per critical section */
  DTYPEP(sym, DT_INT);
  return sym;
}

int
ncpus_temp(int sc)
{
  int sym;
  char name[10];
  static int ncpus_cnt = 0; /* counter for ncpus temporaries */

  strcpy(name, ".ncp");
  sprintf(&name[4], "%05d", ncpus_cnt);
  ncpus_cnt++;
  sym = getcctemp_sc(name, ST_VAR,
                     sc); /* ncpus variable, 1 per critical section */
  DTYPEP(sym, DT_INT);
  return sym;
}

static int
gen_int_load(int sym)
{
  int ili;
  int nme;

  ili = ad_acon(sym, 0);
  nme = addnme(NT_VAR, sym, 0, 0);
  ili = ad3ili(IL_LD, ili, nme, MSZ_WORD);
  return ili;
}

static int
gen_int_store(int sym, int rhs)
{
  int ili;
  int nme;

  ili = ad_acon(sym, 0);
  nme = addnme(NT_VAR, sym, 0, 0);
  ili = ad4ili(IL_ST, rhs, ili, nme, MSZ_WORD);
  return ili;
}

static int
add_mp_bcs_nest()
{
  int ili;
  ili = make_call("_mp_bcs_nest", IL_JSR, 0);
  return ili;
}

static int
add_mp_ecs_nest()
{
  int ili;
  ili = make_call("_mp_ecs_nest", IL_JSR, 0);
  return ili;
}

/** \brief Insert semaphore wait (enter critical section)
 */
int
add_mp_p(int semaphore)
{
  int ili;
  ili = ll_make_kmpc_critical(semaphore);
  return ili;
}

/** \brief Insert semaphore signal (end critical section)
 */
int
add_mp_v(int semaphore)
{
  int ili;
  ili = ll_make_kmpc_end_critical(semaphore);
  return ili;
}

int
add_mp_penter(int ispar)
{
  int size_symptr;
  int sizeili, argili, ili;
  int funcsptr;
  return ili;
}

int
add_mp_pexit(void)
{
  return 0;
}

int
add_mp_ncpus(void)
{
  return 0;
}

int
add_mp_ncpus3(void)
{
  return 0;
}

int
add_mp_lcpu(void)
{
  return 0;
}

int
add_mp_barrier2(void)
{
  return 0;
}

/* for compiler generated routines that have referenced the threadprivate
   variables, but do not need the kmpc_threadprivate_cached set up
 */
void
clear_tplnk(void)
{
  int sym;
  for (sym = gbl.threadprivate; sym > NOSYM; sym = TPLNKG(sym)) {
    TPLNKP(sym, 0);
    THPRVTOPTP(sym, 0); /* so much trouble clear this too, damnit */
  }
  gbl.threadprivate = 1;
}

/** \brief Generate any mp-specific prologue for a function.
 */
void
exp_mp_func_prologue(void)
{
  int sym, ili, tmpthread;
  int func;
  int next_tp;
  int cond_ili = 0;
  int class_register = 0;
  int bih = 0;

#ifdef CUDAG
  if (CUDAG(GBL_CURRFUNC) == CUDA_GLOBAL || CUDAG(GBL_CURRFUNC) == CUDA_DEVICE)
    return;
#endif
  if (1) {
    for (sym = gbl.threadprivate; sym > NOSYM; sym = TPLNKG(sym)) {
      /* For each threadprivate common, must 'declare' the threads'
       * copies by calling:
       * _kmpc_threadprivate_cached(&cmn_block, &cmn_vector, size(cmn_block))
       */
      int call;

      tmpthread = alloc_threadprivate(sym, &cond_ili);
      if (gbl.outlined)
        func = gbl.currsub;
      else
        func =
            gbl.entries; /* this does not really work for entry because for llvm
                          * entry are done very late and in separate function.
                          */
      for (func = gbl.currsub; func != NOSYM; func = SYMLKG(func)) {
        if (EXPDBG(8, 256))
          fprintf(gbl.dbgfil, "---_kmpc_threadprivate_cached: in %s ---\n", SYMNAME(func));

        bih = expb.curbih = find_enlab_bih(func);
        rdilts(expb.curbih); /* get block after entry */
        expb.curilt = 0;
        iltb.callfg = 1;
        chk_block(tmpthread);
        wrilts(expb.curbih);
      }
      THPRVTOPTP(sym, 0);
    }
  }

  ll_save_gtid_val(bih);
}

void
no_pad_func(char *fname)
{
  int sptr;

  sptr = mkfunc(fname);
  NOPADP(sptr, 1);
}

static void
incr_ll_par_cnt(void)
{
  ll_par_cnt++;
}

static void
decr_ll_par_cnt(void)
{
  ll_par_cnt--;
  if (ll_par_cnt == 0)
    ll_write_ilm_end();
}

static void
incr_par_cnt(void)
{
  par_cnt++;
  if (par_cnt > max_par_cnt)
    max_par_cnt = par_cnt;
  incr_ll_par_cnt();
}

static int
get_parrgn_temp(char *pfx, int dtype)
{
  char name[32];
  int sym;

  sprintf(name, "%s%05d", pfx, sum_par_cnt + par_cnt);
  sym = getcctemp_sc(name, ST_VAR, expb.sc);
  DTYPEP(sym, DT_INT);
  return sym;
}

static int
is_unnamed_cs(int sem)
{

  if (strcmp(SYMNAME(MIDNUMG(sem)), "__cs_unspc") == 0)
    return 1;
  return 0;
}

static int
add_mp_unp(void)
{
  int ili;
  ili = ll_make_kmpc_critical(0);
  return ili;
}

static int
add_mp_unv(void)
{
  int ili;
  ili = ll_make_kmpc_end_critical(0);
  return ili;
}

/* isn't there some standard routine I can subsititue for this? */
static int
get_elem_size(int dtype)
{

  int dd;

  dd = dtype;

  while (dd && (DTY(dd) == TY_ARRAY)) {
    dd = DTY(dd + 1);
  }
  if (DTY(dd) == TY_STRUCT)
    return (DTY(dd + 2));
  return (0);
}

int
_make_mp_get_threadprivate(int data_ili, int size_ili, int cache_ili)
{
  int argili, ili, con;
  int null_arg;
  INT tmp[2];
  tmp[0] = 0;
  tmp[1] = 0;
  con = getcon(tmp, DT_INT);
  null_arg = ad1ili(IL_ACON, con);

  argili = jsr_add_arg(0, IL_ARGAR, cache_ili);
  mk_prototype("_mp_get_threadprivate", NULL, DT_CPTR, 5, DT_CPTR, DT_INT,
               DT_CPTR, DT_INT8, DT_CPTR);
  size_ili = sel_iconv(size_ili, 1);
  argili = jsr_add_arg(argili, IL_ARGKR, size_ili);
  argili = jsr_add_arg(argili, IL_ARGAR, data_ili);
  argili = jsr_add_arg(argili, IL_ARGIR, ll_get_gtid_val_ili());
  argili = jsr_add_arg(argili, IL_ARGAR, null_arg);
  ili = make_call("_mp_get_threadprivate", IL_QJSR, argili);
  ili = genretvalue(ili, IL_DFRAR);
  return ili;
}

/** \brief C and Fortran threadprivate : for simple POD */
static int
alloc_threadprivate(int sym, int *tmpthr)
{
  int cm;
  int size;
  int adr_vector;
  int adr_cm;
  int call;

  cm = MIDNUMG(sym); /* corresponding common block  or threadprivate var */
  if (STYPEG(cm) == ST_CMBLK) {
    adr_cm = ad_acon(CMEMFG(cm), 0); /* &cmn_block */
    size = ad_icon((INT)SIZEG(cm));
  }
  else if (SCG(cm) == SC_BASED && POINTERG(cm)) {
    /*
     * Cannot rely on the SYMLK chain appearing as
     *     $p -> $o -> $sd
     * Apparently, these links only occur for the
     * pointer's internal variables if the pointer
     * does not have the SAVE attribute.  Without
     * these fields, the correct size of the threads'
     * copies cannot be computed.
     * Just explicitly look for the internal pointer
     * and descriptor. If the descriptor is present,
     * can assume that there is an offest variable which
     * only needs to be accounted for in the size
     * computation of the threads' copies.
     * Setup up the MIDNUM fields as follows where
     * foo is the symtab entry which has the POINTER
     * flag set:
     *    foo    -> foo$p
     *    TPpfoo -> foo
     *    foo$p  -> TPpfoo
     *    foo$sd -> TPpfoo
     * Note that foo's SDSC -> foo$sd.
     * Before we had:
     *    foo    -> TPpfoo
     *    TPpfoo -> foo$p
     * which is a problem for computing the size
     * when starting with TPpfoo.
     */
    int tptr;
    int sdsptr;
    tptr = MIDNUMG(cm);
    adr_cm = ad_acon(tptr, 0); /* &tp_var */
    size = size_of(DTYPEG(tptr));
    sdsptr = SDSCG(cm); /* $sd */
    if (sdsptr) {
      size += size_of(DT_ADDR);        /* $o */
      size += size_of(DTYPEG(sdsptr)); /* $sd */
    }
    size = ad_icon(size);
  }
  else if (DTY(DTYPEG(cm)) == TY_PTR) {
    /*
     * Given the above code for POINTER, this code is
     * probably dead, but leave it just in case.
     */
    adr_cm = ad_acon(cm, 0); /* &tp_var */
    size = size_of(DTYPEG(cm));
    if (SYMLKG(cm) != NOSYM) {
      size += size_of(DTYPEG(SYMLKG(cm))); /* $o */
      if (SYMLKG(SYMLKG(cm)) != NOSYM) {
        size += size_of(DTYPEG(SYMLKG(SYMLKG(cm)))); /* $sd */
      }
    }
    size = ad_icon(size);
  } else if (SCG(sym) == SC_BASED) {
    adr_cm = ad_acon(cm, 0); /* &tp_var */
    size = ad_icon(size_of(DTYPEG(cm)));
  } else {
    adr_cm = ad_acon(cm, 0); /* &tp_var */
    size = ad_icon(size_of(DTYPEG(cm)));
  }
  adr_vector = ad_acon(sym, 0); /* &cmn_vector/tp_vector */

  if (tmpthr) {
    int tili;
    int tsym;
    ll_set_new_threadprivate(sym);
    tsym = THPRVTOPTG(sym);
    if (XBIT(69, 0x80)) { /* experiment flag */
      tili = _make_mp_get_threadprivate(adr_cm, size, adr_vector);
      *tmpthr = ad3ili(IL_STA, tili, adr_vector, addnme(NT_VAR, sym, 0, 0));
    } else {
      tili = ll_make_kmpc_threadprivate_cached(adr_cm, size, adr_vector);
      *tmpthr =
          ad3ili(IL_STA, tili, ad_acon(tsym, 0), addnme(NT_VAR, tsym, 0, 0));
    }
  }
  return *tmpthr;
}

int
get_threadprivate_origsize(int sym)
{
  int cm;
  int size;

  if (SCG(sym) == SC_CMBLK)
    sym = MIDNUMG(sym); /* get the original common block */

  sym = MIDNUMG(sym);
  cm = MIDNUMG(sym);
  if (STYPEG(cm) == ST_CMBLK) {
    size = ad_icon((INT)SIZEG(cm));
  }
  else if (SCG(cm) == SC_BASED && POINTERG(cm)) {
    int tptr;
    int sdsptr;
    tptr = MIDNUMG(cm);
    size = size_of(DTYPEG(tptr));
    sdsptr = SDSCG(cm); /* $sd */
    if (sdsptr) {
      size += size_of(DT_ADDR);        /* $o */
      size += size_of(DTYPEG(sdsptr)); /* $sd */
    }
    size = ad_icon(size);
  }
  else if (DTY(DTYPEG(cm)) == TY_PTR) {
    size = size_of(DTYPEG(cm));
    if (SYMLKG(cm) != NOSYM) {
      size += size_of(DTYPEG(SYMLKG(cm))); /* $o */
      if (SYMLKG(SYMLKG(cm)) != NOSYM) {
        size += size_of(DTYPEG(SYMLKG(SYMLKG(cm)))); /* $sd */
      }
    }
    size = ad_icon(size);
  } else if (SCG(sym) == SC_BASED) {
    size = ad_icon(size_of(DTYPEG(cm)));
  } else {
    size = ad_icon(size_of(DTYPEG(cm)));
  }

  return size;
}

static int
ll_get_private_temp(int dtype)
{
  int sptr;
  static int count;
  sptr = getnewccsym('s', count++, ST_VAR);
  SCP(sptr, SC_PRIVATE);
  DTYPEP(sptr, dtype);
  ENCLFUNCP(sptr, GBL_CURRFUNC);
  return sptr;

}
static int
get_num_sect(int *tab)
{
  int i;
  if (!tab)
    return 0;
  for (i = 0; tab[i] != -1; i++) {
  }
  return i;
}

int
ll_task_alloc_sptr()
{
  return task_alloc_sptr;
}
