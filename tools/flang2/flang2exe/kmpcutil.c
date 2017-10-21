/*
 * Copyright (c) 2016-2017, NVIDIA CORPORATION.  All rights reserved.
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

#define MXIDLEN 250
static int kmpc_ident_dtype;

/* Flags for use with the entry */
#define DT_VOID_NONE DT_NONE

#define KMPC_FLAG_NONE 0x00
#define KMPC_FLAG_STR_FMT 0x01  /* Treat KMPC_NAME as a format str */

static const struct kmpc_api_entry_t kmpc_api_calls[] = {
        [KMPC_API_BAD] = {"__INVALID_KMPC_API_NAME__", -1, -1, -1},
        [KMPC_API_FORK_CALL] = {"__kmpc_fork_call", 0, DT_VOID_NONE, 0},
        [KMPC_API_BARRIER] = {"__kmpc_barrier", 0, DT_VOID_NONE, 0},
        [KMPC_API_CANCEL_BARRIER] = {"__kmpc_cancel_barrier", 0, DT_VOID_NONE,
                                     0},
        [KMPC_API_COPYPRIVATE] = {"__kmpc_copyprivate", 0, DT_VOID_NONE, 0},
        [KMPC_API_CRITICAL] = {"__kmpc_critical", 0, DT_VOID_NONE, 0},
        [KMPC_API_END_CRITICAL] = {"__kmpc_end_critical", 0, DT_VOID_NONE, 0},
        [KMPC_API_SINGLE] = {"__kmpc_single", IL_DFRIR, DT_INT, 0},
        [KMPC_API_END_SINGLE] = {"__kmpc_end_single", 0, DT_VOID_NONE, 0},
        [KMPC_API_MASTER] = {"__kmpc_master", IL_DFRIR, DT_INT, 0},
        [KMPC_API_END_MASTER] = {"__kmpc_end_master", 0, DT_VOID_NONE, 0},
        [KMPC_API_FLUSH] = {"__kmpc_flush", 0, DT_VOID_NONE, 0},
        [KMPC_API_ORDERED] = {"__kmpc_ordered", 0, DT_VOID_NONE, 0},
        [KMPC_API_END_ORDERED] = {"__kmpc_end_ordered", 0, DT_VOID_NONE, 0},
        [KMPC_API_FOR_STATIC_INIT] = {"__kmpc_for_static_init_%d%s", 0,
                                      DT_VOID_NONE, KMPC_FLAG_STR_FMT},
        [KMPC_API_FOR_STATIC_FINI] = {"__kmpc_for_static_fini", 0, DT_VOID_NONE,
                                      0},
        [KMPC_API_DISPATCH_INIT] =
            {"__kmpc_dispatch_init_%d%s", 0, DT_VOID_NONE,
             KMPC_FLAG_STR_FMT}, /*4,4u,8,8u are possible*/
        [KMPC_API_DISPATCH_NEXT] =
            {"__kmpc_dispatch_next_%d%s", IL_DFRIR, DT_INT,
             KMPC_FLAG_STR_FMT}, /*4,4u,8,8u are possible*/
        [KMPC_API_DISPATCH_FINI] =
            {"__kmpc_dispatch_fini_%d%s", 0, DT_VOID_NONE,
             KMPC_FLAG_STR_FMT}, /*4,4u,8,8u are possible*/
        [KMPC_API_GLOBAL_THREAD_NUM] = {"__kmpc_global_thread_num", IL_DFRIR,
                                        DT_INT, 0},
        [KMPC_API_GLOBAL_NUM_THREADS] = {"__kmpc_global_num_threads", IL_DFRIR,
                                         DT_INT, 0},
        [KMPC_API_BOUND_THREAD_NUM] = {"__kmpc_bound_thread_num", IL_DFRIR,
                                       DT_INT, 0},
        [KMPC_API_BOUND_NUM_THREADS] = {"__kmpc_bound_num_threads", IL_DFRIR,
                                        DT_INT, 0},
        [KMPC_API_PUSH_NUM_THREADS] = {"__kmpc_push_num_threads", 0,
                                       DT_VOID_NONE, 0},
        [KMPC_API_SERIALIZED_PARALLEL] = {"__kmpc_serialized_parallel", 0,
                                          DT_VOID_NONE, 0},
        [KMPC_API_END_SERIALIZED_PARALLEL] = {"__kmpc_end_serialized_parallel",
                                              0, DT_VOID_NONE, 0},
        [KMPC_API_THREADPRIVATE_CACHED] = {"__kmpc_threadprivate_cached",
                                           IL_DFRAR, DT_CPTR, 0},
        [KMPC_API_THREADPRIVATE_REGISTER_VEC] =
            {"__kmpc_threadprivate_register_vec", 0, DT_VOID_NONE, 0},
        [KMPC_API_THREADPRIVATE_REGISTER] = {"__kmpc_threadprivate_register", 0,
                                             DT_VOID_NONE, 0},
        [KMPC_API_TASK] = {"__kmpc_omp_task", 0, DT_VOID_NONE, 0},
        [KMPC_API_TASK_BEGIN_IF0] = {"__kmpc_omp_task_begin_if0", 0,
                                     DT_VOID_NONE, 0},
        [KMPC_API_TASK_COMPLETE_IF0] = {"__kmpc_omp_task_complete_if0", 0,
                                        DT_VOID_NONE, 0},
        [KMPC_API_TASK_ALLOC] = {"__kmpc_omp_task_alloc", IL_DFRAR, DT_CPTR, 0},
        [KMPC_API_TASK_WAIT] = {"__kmpc_omp_taskwait", IL_DFRIR, DT_INT, 0},
        [KMPC_API_TASK_YIELD] = {"__kmpc_omp_taskyield", IL_DFRIR, DT_INT, 0},
        [KMPC_API_CANCEL] = {"__kmpc_cancel", IL_DFRIR, DT_INT, 0},
        [KMPC_API_CANCELLATIONPOINT] = {"__kmpc_cancellationpoint", IL_DFRIR,
                                        DT_INT, 0},
        [KMPC_API_TASKGROUP] = {"__kmpc_taskgroup", 0, DT_VOID_NONE, 0},
        [KMPC_API_END_TASKGROUP] = {"__kmpc_end_taskgroup", 0, DT_VOID_NONE, 0},
        [KMPC_API_TASK_WITH_DEPS] = {"__kmpc_task_with_deps", IL_DFRIR, DT_INT,
                                     0},
        [KMPC_API_WAIT_DEPS] = {"__kmpc_wait_deps", 0, DT_VOID_NONE, 0},
        [KMPC_API_TASKLOOP] = {"__kmpc_taskloop", 0, DT_VOID_NONE, 0},
        [KMPC_API_THREADPRIVATE] = {"__kmpc_threadprivate", IL_DFRAR, DT_CPTR,
                                    0},
        [KMPC_API_FORK_TEAMS] = {"__kmpc_fork_teams", 0, DT_VOID_NONE, 0},
        [KMPC_API_PUSH_NUM_TEAMS] = {"__kmpc_push_num_teams", 0, DT_VOID_NONE,
                                     0},
        [KMPC_API_DIST_FOR_STATIC_INIT] = {"__kmpc_dist_for_static_init_%d%s",
                                           0, DT_VOID_NONE, KMPC_FLAG_STR_FMT},
        [KMPC_API_DIST_DISPATCH_INIT] =
            {"__kmpc_dist_dispatch_init_%d%s", 0, DT_VOID_NONE,
             KMPC_FLAG_STR_FMT}, /*4,4u,8,8u are possible*/
        [KMPC_API_PUSH_PROC_BIND] = {"__kmpc_push_proc_bind", 0, DT_VOID_NONE,
                                     0},
        [KMPC_API_ATOMIC_RD] = {"__kmpc_atomic_%s%d_rd", 0, DT_VOID_NONE,
                                     KMPC_FLAG_STR_FMT},
        [KMPC_API_ATOMIC_WR] = {"__kmpc_atomic_%s%d_wr", 0, DT_VOID_NONE,
                                     KMPC_FLAG_STR_FMT},
};

#define KMPC_NAME(_api) kmpc_api_calls[KMPC_CHK(_api)].name
#define KMPC_RET_DTYPE(_api) kmpc_api_calls[KMPC_CHK(_api)].ret_dtype

#define KMPC_RET_ILIOPC(_api) kmpc_api_calls[KMPC_CHK(_api)].ret_iliopc
#define KMPC_FLAGS(_api) kmpc_api_calls[KMPC_CHK(_api)].flags

#define KMPC_CHK(_api) \
  (((_api) > KMPC_API_BAD && (_api) < KMPC_API_N_ENTRIES) ? _api : KMPC_API_BAD)


/*
 * void __kmpc_fork_call ( ident_t loc, kmp_int32 argc, kmpc_micro
 * microtask,...)
 *                         DT_ADDR      INT             DT_ADDR
 *
 *
 *
 * all outlined function are in this form:
 *
 * void outlined_func_uniqname(INT* gbl_tid, INT* bnd_tid, struct* );
 *
 * gbl_tid: global thread identity of thread
 * bnd_tid: local id of thread
 * struct*: pointers to shared variables - actual struct size depends on size of
 * sh vars.
 *
 */

static void
dump_loop_args(const loop_args_t *args)
{
  FILE *fp = gbl.dbgfil ? gbl.dbgfil : stdout;
  fprintf(fp, "********** KMPC Loop Arguments (line:%d) **********\n",
          gbl.lineno);
  fprintf(fp, "Lower Bound: %d (%s) (%s)\n", args->lower, SYMNAME(args->lower),
          stb.tynames[DTY(DTYPEG(args->lower))]);
  fprintf(fp, "Upper Bound: %d (%s) (%s)\n", args->upper, SYMNAME(args->upper),
          stb.tynames[DTY(DTYPEG(args->upper))]);
  fprintf(fp, "Stride:      %d (%s) (%s)\n", args->stride,
          SYMNAME(args->stride), stb.tynames[DTY(DTYPEG(args->stride))]);
  fprintf(fp, "Chunk:       %d (%s) (%s)\n", args->chunk, SYMNAME(args->chunk),
          stb.tynames[DTY(DTYPEG(args->chunk))]);
  fprintf(fp, "dtype:       %d (%s) \n", args->dtype,
          stb.tynames[DTY(args->dtype)]);
  fprintf(fp, "**********\n\n");
}

/* Return ili (icon/kcon, or a loaded value) for use with mk_kmpc_api_call
 * arguments.
 */
static int
ld_sptr(int sptr)
{
  ISZ_T sz = size_of(DTYPEG(sptr));

  if (STYPEG(sptr) == ST_CONST) {
    if (sz == 8)
      return ad_kcon(CONVAL1G(sptr), CONVAL2G(sptr));
    else
      return ad_icon(CONVAL2G(sptr));
  } else {
    int nme = addnme(NT_VAR, sptr, 0, 0);
    int ili = mk_address(sptr);
    if (ILI_OPC(ili) == IL_LDA)
      nme = ILI_OPND(ili, 2);
    if (sz == 8)
      return ad3ili(IL_LDKR, ili, nme, MSZ_I8);
    else
      return ad3ili(IL_LD, ili, nme, mem_size(DTY(DTYPEG(sptr))));
  }

  assert(0, "Invalid sptr for mk_kmpc_api_call arguments", sptr, 4);
}

static int
gen_null_arg()
{
  int con, ili;
  INT tmp[2];

  tmp[0] = 0;
  tmp[1] = 0;
  con = getcon(tmp, DT_INT);
  ili = ad1ili(IL_ACON, con);
  return ili;
}

int
ll_make_kmpc_struct_type(int count, char *name, KMPC_ST_TYPE *meminfo)
{
  int dtype, i, mem, tag, prev_mem, first_mem;
  ISZ_T sz;
  char sname[MXIDLEN];

  tag = 0;
  dtype = cg_get_type(6, TY_STRUCT, NOSYM);
  if (name) {
    sprintf(sname, "struct%s", name);
    tag = getsymbol(sname);
    DTYPEP(tag, dtype);
  }

  prev_mem = first_mem = sz = 0;
  mem = NOSYM;
  for (i = 0; i < count; ++i) {
    mem = addnewsym(meminfo[i].name);
    STYPEP(mem, ST_MEMBER);
    PAROFFSETP(mem, meminfo[i].psptr);
    DTYPEP(mem, meminfo[i].dtype);
    if (prev_mem > 0)
      SYMLKP(prev_mem, mem);
    SYMLKP(mem, NOSYM);
    PSMEMP(mem, mem);
    VARIANTP(mem, prev_mem);
    CCSYMP(mem, 1);
    ADDRESSP(mem, sz);
    SCP(mem, 0);
    if (first_mem == 0)
      first_mem = mem;
    sz += size_of(meminfo[i].dtype);
    prev_mem = mem;
  }

  DTY(dtype + 1) = first_mem; /* first mem */
  DTY(dtype + 2) = sz;        /* size */
  DTY(dtype + 3) = tag;
  DTY(dtype + 4) = 0; /* align */
  DTY(dtype + 5) = 0;

  return dtype;
}

/*
 * struct ident { i32, i32, i32, i32, char* }
 */
static int
ll_make_kmpc_ident_type(void)
{
  KMPC_ST_TYPE meminfo[] = {{"reserved_1", DT_INT, 0, 0},
                            {"flags", DT_INT, 0, 0},
                            {"reserved_2", DT_INT, 0, 0},
                            {"reserved_3", DT_INT, 0, 0},
                            {"psource", DT_CPTR, 0, 0}};

  if (kmpc_ident_dtype == 0)
    kmpc_ident_dtype =
        ll_make_kmpc_struct_type(5, "_pgi_kmpc_ident_t", meminfo);
  return kmpc_ident_dtype;
}

/* Name 'nm' should be formatted and passed in: use build_kmpc_api_name() */
static int
ll_make_kmpc_proto(const char *nm, int kmpc_api, int argc, int *args)
{
  int ret_dtype;
  /* args contains a list of dtype.  The actual sptr of args will be create in
     ll_make_ftn_outlined_params.
   */
  const int func_sptr = getsymbol(nm);

  if (!nm)
    nm = KMPC_NAME(kmpc_api);

  ret_dtype = KMPC_RET_DTYPE(kmpc_api);

  DTYPEP(func_sptr, ret_dtype);
  SCP(func_sptr, SC_EXTERN);
  STYPEP(func_sptr, ST_PROC);
  CCSYMP(func_sptr,
         1); /* currently we make all CCSYM func varargs in Fortran. */
  CFUNCP(func_sptr, 1);
  ll_make_ftn_outlined_params(func_sptr, argc, args);
  ll_process_routine_parameters(func_sptr);

  /* Update ABI (special case) */
  if (kmpc_api == KMPC_API_FORK_CALL) {
    LL_ABI_Info *abi = ll_proto_get_abi(nm);
    abi->is_varargs = TRUE;
  }
  /* Update ABI (special case) */
  if (kmpc_api == KMPC_API_FORK_TEAMS) {
    LL_ABI_Info *abi = ll_proto_get_abi(nm);
    abi->is_varargs = TRUE;
  }
  return func_sptr;
}

/* Argument instance representing location information
 * This creates a struct ptr instance (for use as an argument).
 * src/kmp.h:
 * ident_t = {
 *     i32 reserved;
 *     i32 flags;
 *     i32 reserved;
 *     i32 reserved;
 *     char *psource -- funcname;lineno;lineno
 */
static int
make_kmpc_ident_arg(void)
{
  int i, ilix, nme, offset;
  static int n;
  const int dtype = ll_make_kmpc_ident_type();
  const int ident = getnewccsym('I', ++n, ST_STRUCT);

  SCP(ident, SC_LOCAL);
  REFP(ident, 1); /* don't want it to go in sym_is_refd */
  DTYPEP(ident, dtype);
  nme = addnme(NT_VAR, ident, 0, 0);

  /* Set the fields to to 0 for now */
  offset = 0;
  for (i = DTY(dtype + 1); i > NOSYM; i = SYMLKG(i)) {
    const int addr = ad_acon(ident, offset);
    ilix = ad4ili(IL_ST, ad_icon(0), addr, addnme(NT_MEM, PSMEMG(i), nme, 0),
                  mem_size(DTY(DTYPEG(i))));
    offset += size_of(DTYPEG(i));
    chk_block(ilix);
  }

  return ad_acon(ident, 0);
}

/* The return value is allocated and maintained locally, please do not call
 * 'free' on this, bad things will probably happen.
 *
 * Caller is responsible for calling va_end()
 *
 * This function will maintain one allocation for unique function name.
 */
static const char *
build_kmpc_api_name(int kmpc_api, va_list va)
{
  static hashmap_t names; /* Maintained in this routine */

  if (!names)
    names = hashmap_alloc(hash_functions_strings);

  if (KMPC_FLAGS(kmpc_api) & KMPC_FLAG_STR_FMT) {
    char *nm, *res;

    /* Construct the name */
    vasprintf(&nm, KMPC_NAME(kmpc_api), va);

    /* If the name has already been allocated, use that to save memory */
    if (hashmap_lookup(names, (hash_key_t)nm, (hash_data_t *)&res)) {
      free(nm);
      return res;
    } else {
      hashmap_insert(names, (hash_key_t)nm, (hash_data_t)nm);
      return nm;
    }
  } else
    return KMPC_NAME(kmpc_api);

  assert(FALSE, "build_kmpc_api_name: Incorrect return value", 0, 4);
}

/* Returns the function prototype sptr.
 * 'reg_opc'   ILI op code for return value, e.g., IL_DFRIR or 0 if void
 * 'ret_dtype' dtype representing return value
 */
static int
mk_kmpc_api_call(int kmpc_api, int n_args, int *arg_dtypes, int *arg_ilis, ...)
{
  int i, r, ilix, altilix, gargs, fn_sptr, garg_ilis[n_args];
  const char *nm;
  const int ret_opc = KMPC_RET_ILIOPC(kmpc_api);
  const int ret_dtype = KMPC_RET_DTYPE(kmpc_api);
  va_list va;

  /* Some calls will make use of this (see: KMPC_FLAG_STR_FMT) */
  va_start(va, arg_ilis);

  /* Create the prototype for the API call */
  nm = build_kmpc_api_name(kmpc_api, va);
  fn_sptr = ll_make_kmpc_proto(nm, kmpc_api, n_args, arg_dtypes);
  sym_is_refd(fn_sptr);

  /* Update ACC routine tables and then create the JSR */
  update_acc_with_fn(fn_sptr);
  ilix = ll_ad_outlined_func2(ret_opc, IL_JSR, fn_sptr, n_args, arg_ilis);

  /* Create the GJSR */
  for (i = n_args - 1; i >= 0; --i) /* Reverse the order */
    garg_ilis[i] = arg_ilis[n_args - 1 - i];
  gargs = ll_make_outlined_garg(n_args, garg_ilis, arg_dtypes);
  altilix = ad3ili(IL_GJSR, fn_sptr, gargs, 0);

  /* Add gjsr as an alt to the jsr */
  if (ret_opc)
    ILI_ALT(ILI_OPND(ilix, 1)) = altilix;
  else
    ILI_ALT(ilix) = altilix;

  va_end(va);
  return ilix;
}

/* Generic routine that returns a jsr to __kmpc_<api_name>
 * This is for all kmpc function calls that look like:
 * int api_func(ident *)
 */
static int
ll_make_kmpc_generic_ptr(int kmpc_api)
{
  int args[1], arg_types[1] = {DT_CPTR};
  args[0] = gen_null_arg();
  return mk_kmpc_api_call(kmpc_api, 1, arg_types, args);
}

#define KMPC_GENERIC_P(_fn, _api)          \
  int _fn(void)                            \
  {                                        \
    return ll_make_kmpc_generic_ptr(_api); \
  }
KMPC_GENERIC_P(ll_make_kmpc_global_thread_num, KMPC_API_GLOBAL_THREAD_NUM)
KMPC_GENERIC_P(ll_make_kmpc_global_num_threads, KMPC_API_GLOBAL_NUM_THREADS)
KMPC_GENERIC_P(ll_make_kmpc_bound_thread_num, KMPC_API_BOUND_THREAD_NUM)
KMPC_GENERIC_P(ll_make_kmpc_bound_num_threads, KMPC_API_BOUND_NUM_THREADS)

/* Generic routine that returns a jsr to __kmpc_<api_name>
 * This is for all kmpc function calls that look like
 * void api_func(ident *, global_tid).
 *
 * Many kmpc routines follow this prototype so we generalize our code here to
 * generate this.
 */
static int
ll_make_kmpc_generic_ptr_int(int kmpc_api)
{
  int args[2], arg_types[2] = {DT_CPTR, DT_INT};
  args[1] = gen_null_arg();
  args[0] = ll_get_gtid_val_ili();
  return mk_kmpc_api_call(kmpc_api, 2, arg_types, args);
}

#define KMPC_GENERIC_P_I(_fn, _api)            \
  int _fn(void)                                \
  {                                            \
    return ll_make_kmpc_generic_ptr_int(_api); \
  }
KMPC_GENERIC_P_I(ll_make_kmpc_barrier, KMPC_API_BARRIER)
KMPC_GENERIC_P_I(ll_make_kmpc_cancel_barrier, KMPC_API_CANCEL_BARRIER)
KMPC_GENERIC_P_I(ll_make_kmpc_master, KMPC_API_MASTER)
KMPC_GENERIC_P_I(ll_make_kmpc_end_master, KMPC_API_END_MASTER)
KMPC_GENERIC_P_I(ll_make_kmpc_single, KMPC_API_SINGLE)
KMPC_GENERIC_P_I(ll_make_kmpc_end_single, KMPC_API_END_SINGLE)
KMPC_GENERIC_P_I(ll_make_kmpc_ordered, KMPC_API_ORDERED)
KMPC_GENERIC_P_I(ll_make_kmpc_end_ordered, KMPC_API_END_ORDERED)
KMPC_GENERIC_P_I(ll_make_kmpc_for_static_fini, KMPC_API_FOR_STATIC_FINI)
KMPC_GENERIC_P_I(ll_make_kmpc_task_wait, KMPC_API_TASK_WAIT)
KMPC_GENERIC_P_I(ll_make_kmpc_taskgroup, KMPC_API_TASKGROUP)
KMPC_GENERIC_P_I(ll_make_kmpc_end_taskgroup, KMPC_API_END_TASKGROUP)

/* Generic routine that returns a jsr to __kmpc_<api_name>
 * This is for all kmpc function calls that look like
 * void api_func(ident *, global_tid, kmp_int32).
 *
 * Many kmpc routines follow this prototype so we generalize our code here to
 * generate this.
 */
static int
ll_make_kmpc_generic_ptr_2int(int kmpc_api, int argili)
{
  int args[3], arg_types[3] = {DT_CPTR, DT_INT, DT_INT};
  args[2] = gen_null_arg();
  args[1] = ll_get_gtid_val_ili();
  args[0] = argili;
  return mk_kmpc_api_call(kmpc_api, 3, arg_types, args);
}

#define KMPC_GENERIC_P_2I(_fn, _api, argili)            \
  int _fn(int argili)                                   \
  {                                                     \
    return ll_make_kmpc_generic_ptr_2int(_api, argili); \
  }
KMPC_GENERIC_P_2I(ll_make_kmpc_push_proc_bind, KMPC_API_PUSH_PROC_BIND, argili)
KMPC_GENERIC_P_2I(ll_make_kmpc_push_num_threads, KMPC_API_PUSH_NUM_THREADS,
                  argili)
KMPC_GENERIC_P_2I(ll_make_kmpc_cancel, KMPC_API_CANCEL, argili)
KMPC_GENERIC_P_2I(ll_make_kmpc_cancellationpoint, KMPC_API_CANCELLATIONPOINT,
                  argili)

/* arglist is 1 containing the uplevel pointer */
int
ll_make_kmpc_fork_call(int sptr, int argc, int *arglist)
{
  int argili, args[4], arg_types[] = {DT_CPTR, DT_INT, DT_CPTR, 0};
  arg_types[3] = DT_CPTR;
  args[3] = gen_null_arg(); /* ident */
  args[2] = ad_icon(argc);
  args[1] = ad1ili(IL_ACON, get_acon(sptr, 0));
  args[0] = *arglist;
  return mk_kmpc_api_call(KMPC_API_FORK_CALL, 4, arg_types, args);
}

/* arglist is 1 containing the uplevel pointer */
int
ll_make_kmpc_fork_teams(int sptr, int argc, int *arglist)
{
  int argili, args[4], arg_types[] = {DT_CPTR, DT_INT, DT_CPTR, 0};
  arg_types[3] = DT_CPTR;
  args[3] = gen_null_arg(); /* ident */
  args[2] = ad_icon(argc);
  args[1] = ad1ili(IL_ACON, get_acon(sptr, 0));
  args[0] = *arglist;
  return mk_kmpc_api_call(KMPC_API_FORK_TEAMS, 4, arg_types, args);
}

/* Return a result or JSR ili to __kmpc_flush() */
int
ll_make_kmpc_flush(void)
{
  int args[1], arg_types[1] = {DT_CPTR};
  args[0] = gen_null_arg();
  return mk_kmpc_api_call(KMPC_API_FLUSH, 1, arg_types, args);
}

/* Return a result or JSR ili to __kmpc_copyprivate() */
int
ll_make_kmpc_copyprivate(int array_sptr, int single_ili, int copyfunc_acon)
{
  int args[6], arg_types[6] = {DT_CPTR, DT_INT, -1, DT_CPTR, DT_CPTR, DT_INT};
  args[5] = gen_null_arg();        /* ident */
  args[4] = ll_get_gtid_val_ili(); /* tid   */
  if (TARGET_PTRSIZE == 8) {
    /* cpy_size (ignore) */
    arg_types[2] = DT_INT8;
    args[3] = ad_kconi(0);
  } else {
    /* cpy_size (ignore) */
    arg_types[2] = DT_INT;
    args[3] = ad_icon(0);
  }
  args[2] = ad_acon(array_sptr, 0); /* cpy_data          */
  args[1] = copyfunc_acon;          /* cpy_func          */
  args[0] = single_ili;             /* didit             */
  return mk_kmpc_api_call(KMPC_API_COPYPRIVATE, 6, arg_types, args);
}

/* Return a result or JSR ili to __kmpc_critical() */
int
ll_make_kmpc_critical(int sem)
{
  int args[3], arg_types[3] = {DT_CPTR, DT_INT, DT_CPTR};
  args[2] = gen_null_arg();        /* ident */
  args[1] = ll_get_gtid_val_ili(); /* tid   */
  if (sem)
    args[0] = ad_acon(sem, 0); /* critical_name:= i32 [8] */
  else
    args[0] = ad_aconi(0); /* critical_name:= i32 [8] */
  return mk_kmpc_api_call(KMPC_API_CRITICAL, 3, arg_types, args);
}

/* Return a result or JSR ili to __kmpc_end_critical() */
int
ll_make_kmpc_end_critical(int sem)
{
  int args[3], arg_types[3] = {DT_CPTR, DT_INT, DT_CPTR};
  args[2] = gen_null_arg();        /* ident */
  args[1] = ll_get_gtid_val_ili(); /* tid   */
  if (sem)
    args[0] = ad_acon(sem, 0); /* critical_name:= i32 [8] */
  else
    args[0] = ad_aconi(0); /* critical_name:= i32 [8] */
  return mk_kmpc_api_call(KMPC_API_END_CRITICAL, 3, arg_types, args);
}

/* Return a result or JSR ili to __kmpc_push_num_teams() */
int
ll_make_kmpc_push_num_teams(int nteams_ili, int thread_limit_ili)
{
  int args[4], arg_types[4] = {DT_CPTR, DT_INT, DT_INT, DT_INT};
  args[3] = gen_null_arg();        /* ident */
  args[2] = ll_get_gtid_val_ili(); /* tid   */
  args[1] = nteams_ili;            /* num_threads := i32 [8] */
  args[0] = thread_limit_ili;      /* thread_limit := i32 [8] */
  return mk_kmpc_api_call(KMPC_API_PUSH_NUM_TEAMS, 4, arg_types, args);
}

/* Return a result or JSR ili to __kmpc_serialized_parallel() */
int
ll_make_kmpc_serialized_parallel(void)
{
  int args[2], arg_types[2] = {DT_CPTR, DT_INT};
  args[1] = gen_null_arg();        /* ident */
  args[0] = ll_get_gtid_val_ili(); /* tid   */
  return mk_kmpc_api_call(KMPC_API_SERIALIZED_PARALLEL, 2, arg_types, args);
}

/* Return a result or JSR ili to __kmpc_end_serialized_parallel() */
int
ll_make_kmpc_end_serialized_parallel(void)
{
  int args[2], arg_types[2] = {DT_CPTR, DT_INT};
  args[1] = gen_null_arg();        /* ident */
  args[0] = ll_get_gtid_val_ili(); /* tid   */
  return mk_kmpc_api_call(KMPC_API_END_SERIALIZED_PARALLEL, 2, arg_types, args);
}

/* Return a result or JSR ili to __kmpc_threadprivate_cached() */
int
ll_make_kmpc_threadprivate_cached(int data_ili, int size_ili, int cache_ili)
{
  int args[5], arg_types[5] = {DT_CPTR, DT_INT, DT_CPTR, DT_INT8, DT_CPTR};
  /*size_t*/
  args[4] = gen_null_arg();        /* ident     */
  args[3] = ll_get_gtid_val_ili(); /* tid       */
  args[2] = data_ili;              /* data      */

  /* size */
  args[1] = (IL_RES(ILI_OPC(size_ili)) != ILIA_KR) ? ad1ili(IL_IKMV, size_ili)
                                                   : size_ili;
  args[0] = cache_ili; /* cache     */
  return mk_kmpc_api_call(KMPC_API_THREADPRIVATE_CACHED, 5, arg_types, args);
}

/* Return a result or JSR ili to __kmpc_threadprivate_register() */
int
ll_make_kmpc_threadprivate_register(int data_ili, int ctor_ili, int cctor_ili,
                                    int dtor_ili)
{
  int args[5], arg_types[5] = {DT_CPTR, DT_CPTR, DT_CPTR, DT_CPTR, DT_CPTR};
  args[4] = gen_null_arg(); /* ident     */
  args[3] = data_ili;       /* data      */
  args[2] = ctor_ili;       /* ctor   funcptr */
  args[1] = cctor_ili;      /* cctor funcptr  */
  args[0] = dtor_ili;       /* dtor  funcptr  */
  return mk_kmpc_api_call(KMPC_API_THREADPRIVATE_REGISTER, 5, arg_types, args);
}
/* Return a result or JSR ili to __kmpc_threadprivate_register_vec() */
int
ll_make_kmpc_threadprivate_register_vec(int data_ili, int ctor_ili,
                                        int cctor_ili, int dtor_ili,
                                        int size_ili)
{
  int args[6],
      arg_types[6] = {DT_CPTR, DT_CPTR, DT_CPTR, DT_CPTR, DT_CPTR, DT_INT8};
  /* size_t */
  args[5] = gen_null_arg(); /* ident          */
  args[4] = data_ili;       /* data           */
  args[3] = ctor_ili;       /* ctor func_ptr  */
  args[2] = cctor_ili;      /* cctor func_ptr */
  args[1] = dtor_ili;       /* dtor  func_ptr */
  args[0] = size_ili;       /* vec size       */
  return mk_kmpc_api_call(KMPC_API_THREADPRIVATE_REGISTER_VEC, 6, arg_types,
                          args);
}

/* Returns the KMPC flags set for tasking flags given our representation
 * as presented in the MP_TASK_XXX flags (see mp.h).
 */
int
mp_to_kmpc_tasking_flags(const int mp)
{
  int kmpc = (mp & MP_TASK_UNTIED) ? KMPC_TASK_UNTIED : KMPC_TASK_TIED;
  if (mp & MP_TASK_FINAL)
    kmpc |= KMPC_TASK_FINAL;
  if (mp & MP_TASK_PRIORITY)
    kmpc |= KMPC_TASK_PRIORITY;
  return kmpc;
}

/* Return an sptr to the allocated task object:  __kmp_omp_task_alloc()
 * base:        sptr for storing return value from __kmpc_omp_task_alloc.
 * sptr:        sptr representing the outlined function that is the task.
 * flags:       MP_TASK_xxx flags (see mp.h).
 * scope_sptr:  ST_BLOCK containing the uplevel block.
 * uplevel_ili: The uplevel copy, the "cloned", sptr.
 */
int
ll_make_kmpc_task_arg(int base, int sptr, int scope_sptr, int flags_sptr,
                      int uplevel_ili)
{
  LLTask *task;
  LLUplevel *up;
  int offset, size, i, nme, call_ili, ilix, args[6];
  int uplevel_sym;
  int arg_types[6] = {DT_CPTR, DT_INT, DT_INT, DT_INT, DT_INT, DT_CPTR};

  /* Calculate size of all privates */
  task = llmp_get_task(scope_sptr);
  size = llmp_task_get_size(task);

  /* Create the api call */
  args[5] = gen_null_arg();        /* ident             */
  args[4] = ll_get_gtid_val_ili(); /* tid               */
  args[3] = ld_sptr(flags_sptr);   /* flags             */
  args[2] = ad_icon(size);         /* sizeof_kmp_task_t */
  args[1] = ad_icon(sizeof(void*));/* sizeof_shareds    */
  args[0] = ad_acon(sptr, 0);      /* task_entry        */
  call_ili = mk_kmpc_api_call(KMPC_API_TASK_ALLOC, 6, arg_types, args);

  /* Create a temp to store the allocation result into */
  ADDRTKNP(base, TRUE);
  nme = addnme(NT_VAR, base, 0, 0);
  ilix = ad4ili(IL_STA, call_ili, ad_acon(base, 0), nme, MSZ_PTR);
  chk_block(ilix);

  /* store uplevel pointer into shared var area.
   * __kmp_omp_task_alloc will set the value of ptr_to_offset2.
   * task_alloc_sptr =>
   *      offset  [ offset0        | offset1    | offset2  | ... ]     
   *      info    [ ptr_to_offset2 | other-data | share_ptr| ... ]
   */
  ilix = ad2ili(IL_LDA, ad_acon(base, 0), nme);  /* tmp = *(task_alloc_sptr) */
  uplevel_sym = ll_get_uplevel_sym();
  nme = addnme(NT_IND, uplevel_sym, nme, 0);
  ilix = ad2ili(IL_LDA, ilix, nme);              /* tmp[0] - address of shared ptr */
  ilix = ad4ili(IL_STA, uplevel_ili, ilix, nme, MSZ_PTR);
  chk_block(ilix);

  return base;
}

/* Return a JSR ili to __kpmc_omp_task.
 * task_sptr: sptr representing the allocated task value
 *            resulting from calling kmpc_omp_task_alloc()
 *            See: ll_make_kmpc_task_arg()
 */
int
ll_make_kmpc_task(int task_sptr)
{
  int args[3], arg_types[3] = {DT_CPTR, DT_INT, DT_CPTR};
  args[2] = gen_null_arg();        /* ident */
  args[1] = ll_get_gtid_val_ili(); /* tid   */
  args[0] = ad2ili(IL_LDA, ad_acon(task_sptr, 0),
                   addnme(NT_VAR, task_sptr, 0, 0)); /* task  */
  return mk_kmpc_api_call(KMPC_API_TASK, 3, arg_types, args);
}

/* Return a JSR ili to __kmpc_omp_taskyield  */
int
ll_make_kmpc_task_yield(void)
{
  int args[3], arg_types[3] = {DT_CPTR, DT_INT, DT_INT};
  args[2] = gen_null_arg();        /* ident    */
  args[1] = ll_get_gtid_val_ili(); /* tid      */
  args[0] = ad_icon(0);            /* end_part */
  return mk_kmpc_api_call(KMPC_API_TASK_YIELD, 3, arg_types, args);
}

/* Return a JSR ili to __kmpc_omp_task_begin_if0.
 * task_sptr: sptr representing the allocated task
 */
int
ll_make_kmpc_task_begin_if0(int task_sptr)
{
  int args[3], arg_types[3] = {DT_CPTR, DT_INT, DT_CPTR};
  args[2] = gen_null_arg();                           /* ident */
  args[1] = ll_get_gtid_val_ili();                    /* tid   */
  args[0] = ad2ili(IL_LDA, ad_acon(task_sptr, 0), 0); /* task  */
  return mk_kmpc_api_call(KMPC_API_TASK_BEGIN_IF0, 3, arg_types, args);
}

/* Return a JSR ili to __kmpc_omp_task_complete_if0.
 * task_sptr: sptr representing the allocated task
 */
int
ll_make_kmpc_task_complete_if0(int task_sptr)
{
  int args[3], arg_types[3] = {DT_CPTR, DT_INT, DT_CPTR};
  args[2] = gen_null_arg();                           /* ident */
  args[1] = ll_get_gtid_val_ili();                    /* tid   */
  args[0] = ad2ili(IL_LDA, ad_acon(task_sptr, 0), 0); /* task  */
  return mk_kmpc_api_call(KMPC_API_TASK_COMPLETE_IF0, 3, arg_types, args);
}

/* Given an mp (schedule enumeration) return a KMPC equivalent enumerated
 * value.
 */
#define SCHED_PREFIX(_enum_val) (MP_SCH_##_enum_val)
kmpc_sched_e
mp_sched_to_kmpc_sched(int sched)
{
  switch (sched) {
  case SCHED_PREFIX(AUTO):
    return KMP_SCH_AUTO;
  case SCHED_PREFIX(DYNAMIC):
    return KMP_SCH_DYNAMIC_CHUNKED;
  case SCHED_PREFIX(GUIDED):
    return KMP_SCH_GUIDED_CHUNKED;
  case SCHED_PREFIX(RUNTIME):
    return KMP_SCH_RUNTIME_CHUNKED;
  case SCHED_PREFIX(STATIC):
    return KMP_SCH_STATIC;
  case SCHED_PREFIX(DIST_STATIC):
    return KMP_DISTRIBUTE_STATIC;

  /* Ordered */
  case SCHED_PREFIX(AUTO) | MP_SCH_ATTR_ORDERED:
    return KMP_ORD_AUTO;
  case SCHED_PREFIX(RUNTIME) | MP_SCH_ATTR_ORDERED:
    return KMP_ORD_RUNTIME;
  case SCHED_PREFIX(STATIC) | MP_SCH_ATTR_ORDERED:
    return KMP_ORD_STATIC;
  case SCHED_PREFIX(STATIC) | MP_SCH_ATTR_ORDERED | MP_SCH_ATTR_CHUNKED:
    return KMP_ORD_STATIC_CHUNKED;
  case SCHED_PREFIX(DYNAMIC) | MP_SCH_ATTR_ORDERED:
  case SCHED_PREFIX(DYNAMIC) | MP_SCH_ATTR_ORDERED | MP_SCH_ATTR_CHUNKED:
    return KMP_ORD_DYNAMIC_CHUNKED;

  /* Special cases of static */
  case SCHED_PREFIX(STATIC) | MP_SCH_ATTR_CHUNKED:
  case SCHED_PREFIX(STATIC) | MP_SCH_ATTR_CHUNKED | MP_SCH_CHUNK_1:
  case SCHED_PREFIX(STATIC) | MP_SCH_CHUNK_1:
  case SCHED_PREFIX(STATIC) | MP_SCH_ATTR_CHUNKED | MP_SCH_BLK_CYC:
  case SCHED_PREFIX(STATIC) | MP_SCH_BLK_CYC:
  case SCHED_PREFIX(STATIC) | MP_SCH_BLK_ALN:
    return KMP_SCH_STATIC_CHUNKED;

  /* distribute scheduling */
  case SCHED_PREFIX(DIST_STATIC) | MP_SCH_ATTR_CHUNKED:
    return KMP_DISTRIBUTE_STATIC_CHUNKED;

  case SCHED_PREFIX(DIST_STATIC) | MP_SCH_ATTR_CHUNKED | MP_SCH_BLK_CYC:
  case SCHED_PREFIX(DIST_STATIC) | MP_SCH_ATTR_CHUNKED | MP_SCH_CHUNK_1:
    return KMP_DISTRIBUTE_STATIC_CHUNKED;

  default:
    error(155, ERR_Warning, gbl.lineno, "Unsupported OpenMP schedule type.",
          NULL);
  }
  return KMP_SCH_DEFAULT;
}

/* Returns 'true' if this dtype is to be treated as a signed value */
static LOGICAL
is_signed(int dtype)
{
  return TRUE;
}

/* Return a JSR ili to __kmpc_for_static_init_<size><signed|unsigned>
 *
 * args: list of ili values for each arg, see the case below if args is NULL.
 */
int
ll_make_kmpc_for_static_init_args(int dtype, int *inargs)
{
  int args[9], arg_types[9] = {DT_CPTR, DT_INT,  DT_INT,  DT_CPTR, DT_CPTR,
                               DT_CPTR, DT_CPTR, DT_INT8, DT_INT8};
  if (!inargs) {
    args[8] = gen_null_arg();        /* ident     */
    args[7] = ll_get_gtid_val_ili(); /* tid       */
    args[6] = ad_icon(0);            /* sched     */
    args[5] = gen_null_arg();        /* plastiter */
    args[4] = gen_null_arg();        /* plower    */
    args[3] = gen_null_arg();        /* pupper    */
    args[2] = gen_null_arg();        /* pstridr   */
    args[1] = ad_icon(0);            /* incr      */
    args[0] = ad_icon(0);            /* chunk     */
    inargs = args;
  }

  arg_types[7] = dtype; /* incr  */
  arg_types[8] = dtype; /* chunk */

  return mk_kmpc_api_call(KMPC_API_FOR_STATIC_INIT, 9, arg_types, inargs,
                          size_of(dtype), is_signed(dtype) ? "" : "u");
}

/* Return a JSR ili to __kmpc_for_static_init_<size><signed|unsigned>
 * TODO: Merge this routine with ll_make_kmpc_for_static_init_args
 */
int
ll_make_kmpc_for_static_init(const loop_args_t *inargs)
{
  int args[9], arg_types[9] = {DT_CPTR, DT_INT,  DT_INT,  DT_CPTR, DT_CPTR,
                               DT_CPTR, DT_CPTR, DT_INT8, DT_INT8};
  const int dtype = inargs->dtype;
  const int lower = inargs->lower;
  const int upper = inargs->upper;
  const int stride = inargs->stride;
  const int last = inargs->last;
  int chunk = inargs->chunk ? ld_sptr(inargs->chunk) : ad_icon(0);
  const int sched = mp_sched_to_kmpc_sched(inargs->sched);
  const int dtypesize = size_of(dtype);

  if (dtypesize == 4) {
    chunk = kimove(chunk);
  } else if (dtypesize == 8) {
    chunk = ikmove(chunk);
  }

  args[8] = gen_null_arg();        /* ident */
  args[7] = ll_get_gtid_val_ili(); /* tid   */
  args[6] = ad_icon(sched);        /* sched     */
  if (last
      && STYPEG(last) != ST_CONST
      ) {
    args[5] = mk_address(last); /* plastiter */
    ADDRTKNP(last, 1);
  } else {
    args[5] = gen_null_arg();
  }
  args[4] = mk_address(lower);  /* plower    */
  args[3] = mk_address(upper);  /* pupper    */
  args[2] = mk_address(stride); /* pstridr   */
  args[1] = ld_sptr(stride);    /* incr      */
  args[0] = chunk;              /* chunk     */

  ADDRTKNP(upper, 1);
  ADDRTKNP(stride, 1);
  ADDRTKNP(lower, 1);

  arg_types[7] = dtype; /* incr  */
  arg_types[8] = dtype; /* chunk */

  if (DBGBIT(45, 0x8))
    dump_loop_args(inargs);

  return mk_kmpc_api_call(KMPC_API_FOR_STATIC_INIT, 9, arg_types, args,
                          size_of(dtype), is_signed(dtype) ? "" : "u");
}

/* Return a JSR ili to __kmpc_dist_for_static_init_<size><signed|unsigned> */
int
ll_make_kmpc_dist_for_static_init(const loop_args_t *inargs)
{
  int args[10], arg_types[10] = {DT_CPTR, DT_INT,  DT_INT,  DT_CPTR, DT_CPTR,
                                 DT_CPTR, DT_CPTR, DT_CPTR, DT_INT8, DT_INT8};
  const int dtype = inargs->dtype;
  const int lower = inargs->lower;
  const int upper = inargs->upper;
  const int stride = inargs->stride;
  const int last = inargs->last;
  const int upperd = inargs->upperd;
  int chunk = inargs->chunk ? ld_sptr(inargs->chunk) : ad_icon(0);
  const int sched = mp_sched_to_kmpc_sched(inargs->sched);
  const int dtypesize = size_of(dtype);

  if (dtypesize == 4) {
    chunk = kimove(chunk);
  } else if (dtypesize == 8) {
    chunk = ikmove(chunk);
  }

  args[9] = gen_null_arg();        /* ident */
  args[8] = ll_get_gtid_val_ili(); /* tid   */
  args[7] = ad_icon(sched);        /* sched */
  if (last
      && STYPEG(last) != ST_CONST
      ) {
    args[6] = mk_address(last); /* plastiter */
    ADDRTKNP(last, 1);
  } else {
    args[6] = gen_null_arg();
  }
  args[5] = mk_address(lower);  /* plower    */
  args[4] = mk_address(upper);  /* pupper    */
  args[3] = mk_address(upperd); /* upperd   */
  args[2] = mk_address(stride); /* pstridr   */
  args[1] = ld_sptr(stride);    /* incr      */
  args[0] = chunk;              /* chunk     */

  ADDRTKNP(upper, 1);
  ADDRTKNP(stride, 1);
  ADDRTKNP(lower, 1);
  ADDRTKNP(upperd, 1);

  arg_types[8] = dtype; /* incr  */
  arg_types[9] = dtype; /* chunk */

  if (DBGBIT(45, 0x8))
    dump_loop_args(inargs);

  return mk_kmpc_api_call(KMPC_API_DIST_FOR_STATIC_INIT, 10, arg_types, args,
                          size_of(dtype), is_signed(dtype) ? "" : "u");
}

/* Return a result or JSR ili to __kmpc_dispatch_next_<size><signed|unsigned>
 * lower, upper, stride: sptrs
 */
int
ll_make_kmpc_dispatch_next(int lower, int upper, int stride, int last,
                           int dtype)
{
  int args[6],
      arg_types[6] = {DT_CPTR, DT_INT, DT_CPTR, DT_CPTR, DT_CPTR, DT_CPTR};

  /* Stride cannot be a pointer to a const, it will be updated by kmpc */
  args[5] = gen_null_arg();        /* ident     */
  args[4] = ll_get_gtid_val_ili(); /* tid       */
  if (last
      && STYPEG(last) != ST_CONST
      ) {
    args[3] = mk_address(last); /* plastflag */
    ADDRTKNP(last, 1);
  } else {
    args[3] = gen_null_arg();
  }
  args[2] = mk_address(lower);  /* plower    */
  args[1] = mk_address(upper);  /* pupper    */
  args[0] = mk_address(stride); /* pstride   */
  ADDRTKNP(upper, 1);
  ADDRTKNP(lower, 1);
  ADDRTKNP(stride, 1);
  return mk_kmpc_api_call(KMPC_API_DISPATCH_NEXT, 6, arg_types, args,
                          size_of(dtype), is_signed(dtype) ? "" : "u");
}

/* Return a result or JSR ili to __kmpc_dispatch_init_<size><signed|unsigned> */
int
ll_make_kmpc_dispatch_init(const loop_args_t *inargs)
{
  int args[7], arg_types[7] = {DT_CPTR,  DT_INT,  DT_INT, DT_UINT8,
                               DT_UINT8, DT_INT8, DT_INT8};

  const int dtype = inargs->dtype;
  const int lower = ld_sptr(inargs->lower);
  const int upper = ld_sptr(inargs->upper);
  const int stride = ld_sptr(inargs->stride);
  int chunk = ld_sptr(inargs->chunk);
  const int sched = mp_sched_to_kmpc_sched(inargs->sched);
  const int dtypesize = size_of(dtype);

  if (dtypesize == 4) {
    chunk = kimove(chunk);
  } else if (dtypesize == 8) {
    chunk = ikmove(chunk);
  }

  /* Update to use the proper dtype */
  arg_types[3] = dtype; /* lower  */
  arg_types[4] = dtype; /* upper  */
  arg_types[5] = dtype; /* stride */
  arg_types[6] = dtype; /* chunk  */

  /* Build up the arguments */
  args[6] = gen_null_arg();        /* ident */
  args[5] = ll_get_gtid_val_ili(); /* tid   */
  args[4] = ad_icon(sched);        /* sched */
  args[3] = lower;                 /* lower */
  args[2] = upper;                 /* upper */
  args[1] = stride;                /* incr  */
  args[0] = chunk;                 /* chunk */

  if (DBGBIT(45, 0x8))
    dump_loop_args(inargs);

  /* This will have a 4,4u,8,8u appended to the end of the function name */
  return mk_kmpc_api_call(KMPC_API_DISPATCH_INIT, 7, arg_types, args,
                          size_of(dtype), is_signed(dtype) ? "" : "u");
}

/* Return a result or JSR ili to
 * __kmpc_dist_dispatch_init_<size><signed|unsigned> */
int
ll_make_kmpc_dist_dispatch_init(const loop_args_t *inargs)
{
  int args[8], arg_types[8] = {DT_CPTR,  DT_INT,   DT_INT,  DT_CPTR,
                               DT_UINT8, DT_UINT8, DT_INT8, DT_INT8};

  const int dtype = inargs->dtype;
  const int lower = ld_sptr(inargs->lower);
  const int upper = ld_sptr(inargs->upper);
  const int stride = ld_sptr(inargs->stride);
  const int last = inargs->last;
  const int upperd = inargs->upperd;
  int chunk = ld_sptr(inargs->chunk);
  const int sched = mp_sched_to_kmpc_sched(inargs->sched);
  const int dtypesize = size_of(dtype);

  if (dtypesize == 4) {
    chunk = kimove(chunk);
  } else if (dtypesize == 8) {
    chunk = ikmove(chunk);
  }

  /* Update to use the proper dtype */
  arg_types[4] = dtype; /* lower  */
  arg_types[5] = dtype; /* upper  */
  arg_types[6] = dtype; /* stride */
  arg_types[7] = dtype; /* chunk  */

  /* Build up the arguments */
  args[7] = gen_null_arg();        /* ident */
  args[6] = ll_get_gtid_val_ili(); /* tid   */
  args[5] = ad_icon(sched);        /* sched */

  if (last
      && STYPEG(last) != ST_CONST
      ) {
    args[4] = mk_address(last); /* plastiter */
    ADDRTKNP(last, 1);
  } else {
    args[4] = gen_null_arg();
  }

  args[3] = lower;  /* lower */
  args[2] = upper;  /* upper */
  args[1] = stride; /* incr  */
  args[0] = chunk;  /* chunk */

  if (DBGBIT(45, 0x8))
    dump_loop_args(inargs);

  /* This will have a 4,4u,8,8u appended to the end of the function name */
  return mk_kmpc_api_call(KMPC_API_DIST_DISPATCH_INIT, 8, arg_types, args,
                          size_of(dtype), is_signed(dtype) ? "" : "u");
}

/* Return a result or JSR ili to __kmpc_dispatch_fini_<size><signed|unsigned> */
int
ll_make_kmpc_dispatch_fini(int dtype)
{
  int args[2], arg_types[2] = {DT_CPTR, DT_INT};
  args[1] = gen_null_arg();
  args[0] = ll_get_gtid_val_ili();
  return mk_kmpc_api_call(KMPC_API_DISPATCH_FINI, 2, arg_types, args,
                          size_of(dtype), is_signed(dtype) ? "" : "u");
}

/* Return a result or JSR ili to __kmpc_taskloop 
 * kmpc_taskloop(ident, gtid, (?*) task, 
                 (int)if_val,
                 (int64*) lb,   (int64*)ub, (int64)st,
                 (int)nogroup,  (int)sched, (int64)grainsize,
                  * task_dup)
 */
int
ll_make_kmpc_taskloop(int *inargs)
{
  int args[11],
      arg_types[11] = {DT_CPTR, DT_INT, DT_CPTR, DT_INT,  DT_CPTR, DT_CPTR,
                       DT_INT8, DT_INT, DT_INT,  DT_INT8, DT_CPTR};

  /* Build up the arguments */
  args[10] = gen_null_arg();       /* ident */
  args[9] = ll_get_gtid_val_ili(); /* gtid   */
  args[8] = inargs[0]; 	           /* task structure */
  args[7] = inargs[1];             /* if_val */
  args[6] = inargs[2];             /* lower */
  args[5] = inargs[3];             /* upper */
  args[4] = inargs[4];             /* stride */
  args[3] = inargs[5];             /* 1:nogroup */
  args[2] = inargs[6];             /* 0:none,1:grainsize,2:num_task */
  args[1] = inargs[7];             /* grainsize */
  args[0] = inargs[8]? inargs[8]:gen_null_arg();  /* task_dup */

  return mk_kmpc_api_call(KMPC_API_TASKLOOP, 11, arg_types, args);
  return 0;
}

int
ll_make_kmpc_task_with_deps(const loop_args_t *inargs)
{
  return 0;
}

int
ll_make_kmpc_omp_wait_deps(const loop_args_t *inargs)
{
  return 0;
}

void
reset_kmpc_ident_dtype()
{
  kmpc_ident_dtype = 0;
}

/* Return a result or JSR ili to __kmpc_threadprivate() */
int
ll_make_kmpc_threadprivate(int data_ili, int size_ili)
{
  int args[4], arg_types[4] = {DT_CPTR, DT_INT, DT_CPTR, DT_INT8};
  /*size_t*/
  args[3] = gen_null_arg();        /* ident     */
  args[2] = ll_get_gtid_val_ili(); /* tid       */
  args[1] = data_ili;              /* data      */
  args[0] = size_ili;              /* size      */
  return mk_kmpc_api_call(KMPC_API_THREADPRIVATE, 4, arg_types, args);
}

int
ll_make_kmpc_atomic_write(int* opnd, DTYPE dtype)
{
  char* type="";
  int args[4], arg_types[4] = {DT_CPTR, DT_INT, DT_CPTR, DT_CPTR};
  args[3] = gen_null_arg();        /* ident     */
  args[2] = ll_get_gtid_val_ili(); /* tid       */
  args[1] = opnd[1];               /*  lhs      */
  args[0] = opnd[2];               /*  rhs      */

  switch(dtype) {
  case DT_BLOG:
  case DT_SLOG:
  case DT_LOG:
  case DT_LOG8:
  case DT_BINT:
#ifdef DT_SINT
  case DT_SINT:
#endif
  case DT_USINT:
  case DT_INT:
  case DT_UINT:
  case DT_INT8:
    return mk_kmpc_api_call(KMPC_API_ATOMIC_WR, 4, arg_types, args,
                          "fixed", size_of(dtype));

#ifdef DT_FLOAT
  case DT_FLOAT:
#endif
#ifdef DT_DBLE
  case DT_DBLE:
    return mk_kmpc_api_call(KMPC_API_ATOMIC_WR, 4, arg_types, args,
                          "float", size_of(dtype));
#endif
  case DT_CMPLX:
  case DT_DCMPLX:
    return mk_kmpc_api_call(KMPC_API_ATOMIC_WR, 4, arg_types, args,
                          "cmplx", size_of(dtype));
  default: 
    break;
  }
  return 0;
}


int
ll_make_kmpc_atomic_read(int* opnd, DTYPE dtype)
{
  char* type="";
  int args[4], arg_types[5] = {DT_CPTR, DT_INT, DT_CPTR, DT_CPTR};
  args[3] = gen_null_arg();        /* ident     */
  args[2] = ll_get_gtid_val_ili(); /* tid       */
  args[1] = opnd[1];               /*  lhs      */
  args[0] = opnd[2];               /*  rhs      */

  switch(dtype) {
  case DT_BLOG:
  case DT_SLOG:
  case DT_LOG:
  case DT_LOG8:
  case DT_BINT:
#ifdef DT_SINT
  case DT_SINT:
#endif
  case DT_USINT:
  case DT_INT:
  case DT_UINT:
  case DT_INT8:
    return mk_kmpc_api_call(KMPC_API_ATOMIC_RD, 4, arg_types, args,
                          "fixed", size_of(dtype));

#ifdef DT_FLOAT
  case DT_FLOAT:
#endif
#ifdef DT_DBLE
  case DT_DBLE:
    return mk_kmpc_api_call(KMPC_API_ATOMIC_RD, 4, arg_types, args,
                          "float", size_of(dtype));
#endif
  case DT_CMPLX:
  case DT_DCMPLX:
    return mk_kmpc_api_call(KMPC_API_ATOMIC_RD, 4, arg_types, args,
                          "cmplx", size_of(dtype));
  default: 
    break;
  }
  return 0;
}
