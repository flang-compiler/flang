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
 * \brief Various definitions for the kmpc runtime
 */

#ifndef __KMP_RUNTIME_H__
#define __KMP_RUNTIME_H__

/* KMPC Task Flags
 * See KMPC's kmp.h struct kmp_tasking_flags
 */
#define KMPC_TASK_UNTIED     0x00
#define KMPC_TASK_TIED       0x01
#define KMPC_TASK_FINAL      0x02
#define KMPC_TASK_MERGED_IF0 0x04
#define KMPC_TASK_DTOR_THK   0x08
#define KMPC_TASK_PROXY      0x10
#define KMPC_TASK_PRIORITY   0x20

/* KMPC Schedule Types
 * https://www.openmprtl.org/sites/default/files/resources/libomp_20151009_manual.pdf
 * Additional types mentioned in the source and used in the refereced manual's
 * example (KMP_SCH_DYNAMIC_CHUNKED).
 */
typedef enum _kmpc_sched_e {
  KMP_SCH_LOWER = 32,
  KMP_SCH_STATIC_CHUNKED = 33,
  KMP_SCH_STATIC = 34,
  KMP_SCH_DYNAMIC_CHUNKED = 35,
  KMP_SCH_GUIDED_CHUNKED = 36,
  KMP_SCH_RUNTIME_CHUNKED = 37,
  KMP_SCH_AUTO = 38,
  KMP_SCH_STATIC_STEAL = 44,
  KMP_SCH_UPPER = 45,
  KMP_ORD_LOWER = 64,
  KMP_ORD_STATIC_CHUNKED = 65,
  KMP_ORD_STATIC = 66,
  KMP_ORD_DYNAMIC_CHUNKED = 67,
  KMP_ORD_GUIDED_CHUNKED = 68,
  KMP_ORD_RUNTIME = 69,
  KMP_ORD_AUTO = 70,
  KMP_ORD_UPPER = 72,
  KMP_DISTRIBUTE_STATIC_CHUNKED = 91,
  KMP_DISTRIBUTE_STATIC = 92,
  KMP_NM_LOWER = 160,
  KMP_NM_STATIC = 162,
  KMP_NM_GUIDED_CHUNKED = 164,
  KMP_NM_AUTO = 166,
  KMP_NM_ORD_STATIC = 194,
  KMP_NM_ORD_AUTO = 198,
  KMP_NM_UPPER = 200,
  KMP_SCH_DEFAULT = KMP_SCH_STATIC
} kmpc_sched_e;

/* Argument type used for handling for loops and scheduling.
 * All values here are sptrs.
 */
typedef struct _loop_args_t {
  int lower;
  int upper;
  int stride;
  int chunk;
  int last;
  int upperd;
  int dtype;          /* Lower/Upper bound data type INT,INT8,UINT, UINT8 */
  kmpc_sched_e sched; /* KMPC schedule type */
} loop_args_t;

struct kmpc_api_entry_t {
  const char *name;     /* KMPC API function name                    */
  const int ret_iliopc; /* KMPC API function return value ili opcode */
  const int ret_dtype;  /* KMPC API function return value type       */
  const int flags;      /* (Optional) See KMPC_FLAG_XXX above        */
};

/* Used internally for creating structs, or representing formal parameters when
 * generating fortran outlined function/task signatures.
 */
typedef struct any_kmpc_struct {
  char *name;
  int dtype;
  int byval;
  int psptr;
} KMPC_ST_TYPE;

extern int ll_make_kmpc_dispatch_next(int, int, int, int, int);
extern int ll_make_kmpc_dispatch_init(const loop_args_t *);
extern int ll_make_kmpc_dispatch_fini(int);
extern int ll_make_kmpc_fork_call(int, int, int *);
extern int ll_make_kmpc_for_static_init(const loop_args_t *);
extern int ll_make_kmpc_for_static_init_args(int, int *);
extern int ll_make_kmpc_barrier(void);
extern int ll_make_kmpc_cancel_barrier(void);
extern int ll_make_kmpc_master(void);
extern int ll_make_kmpc_end_master(void);
extern int ll_make_kmpc_flush(void);
extern int ll_make_kmpc_single(void);
extern int ll_make_kmpc_ordered(void);
extern int ll_make_kmpc_end_single(void);
extern int ll_make_kmpc_end_serialized_parallel(void);
extern int ll_make_kmpc_end_ordered(void);
extern int ll_make_kmpc_for_static_fini(void);
extern int ll_make_kmpc_task(int);
extern int ll_make_kmpc_task_arg(int, int, int, int, int);
extern int ll_make_kmpc_task_begin_if0(int);
extern int ll_make_kmpc_task_complete_if0(int);
extern int ll_make_kmpc_task_wait(void);
extern int ll_make_kmpc_task_yield(void);
extern int ll_make_kmpc_threadprivate_cached(int, int, int);
extern int ll_make_kmpc_threadprivate(int, int);
extern int ll_make_kmpc_global_thread_num(void);
extern int ll_make_kmpc_global_num_threads(void);
extern int ll_make_kmpc_bound_thread_num(void);
extern int ll_make_kmpc_bound_num_threads(void);
extern int ll_make_kmpc_push_num_threads(int);
extern int ll_make_kmpc_serialized_parallel(void);
extern int ll_make_kmpc_critical(int);
extern int ll_make_kmpc_end_critical(int);
extern int ll_make_kmpc_copyprivate(int, int, int);
extern int ll_make_kmpc_cancel(int);
extern int ll_make_kmpc_cancellationpoint(int);
extern int ll_make_kmpc_struct_type(int, char *, KMPC_ST_TYPE *);
extern int ll_make_kmpc_taskgroup(void);
extern int ll_make_kmpc_end_taskgroup(void);
extern int ll_make_kmpc_push_num_teams(int, int);
extern int ll_make_kmpc_fork_teams(int, int, int *);
extern int ll_make_kmpc_dist_for_static_init(const loop_args_t *);
extern int ll_make_kmpc_dist_dispatch_init(const loop_args_t *);
extern int ll_make_kmpc_push_proc_bind(int);
extern int ll_make_kmpc_atomic_rd(int, int, char*, char*);
extern int ll_make_kmpc_atomic_wr(int, int, char*);
int ll_make_kmpc_taskloop(int*);

extern void reset_kmpc_ident_dtype();
extern int mp_to_kmpc_tasking_flags(int);

/* Given a MP_ or DI_ schedule type and return the KMPC equivalent */
extern kmpc_sched_e mp_sched_to_kmpc_sched(int sched);

/* KMPC API macros and structs */
enum {
  KMPC_API_BAD,
  KMPC_API_FORK_CALL,
  KMPC_API_BARRIER,
  KMPC_API_CANCEL_BARRIER,
  KMPC_API_COPYPRIVATE,
  KMPC_API_CRITICAL,
  KMPC_API_END_CRITICAL,
  KMPC_API_SINGLE,
  KMPC_API_END_SINGLE,
  KMPC_API_MASTER,
  KMPC_API_END_MASTER,
  KMPC_API_FLUSH,
  KMPC_API_ORDERED,
  KMPC_API_END_ORDERED,
  KMPC_API_FOR_STATIC_INIT,
  KMPC_API_FOR_STATIC_FINI,
  KMPC_API_DISPATCH_INIT,
  KMPC_API_DISPATCH_NEXT,
  KMPC_API_DISPATCH_FINI,
  KMPC_API_GLOBAL_THREAD_NUM,
  KMPC_API_GLOBAL_NUM_THREADS,
  KMPC_API_BOUND_THREAD_NUM,
  KMPC_API_BOUND_NUM_THREADS,
  KMPC_API_PUSH_NUM_THREADS,
  KMPC_API_SERIALIZED_PARALLEL,
  KMPC_API_END_SERIALIZED_PARALLEL,
  KMPC_API_THREADPRIVATE_CACHED,
  KMPC_API_THREADPRIVATE_REGISTER_VEC,
  KMPC_API_THREADPRIVATE_REGISTER,
  KMPC_API_TASK_ALLOC,
  KMPC_API_TASK,
  KMPC_API_TASK_BEGIN_IF0,
  KMPC_API_TASK_COMPLETE_IF0,
  KMPC_API_TASK_WAIT,
  KMPC_API_TASK_YIELD,
  KMPC_API_CANCEL,
  KMPC_API_CANCELLATIONPOINT,
  KMPC_API_TASKGROUP,
  KMPC_API_END_TASKGROUP,
  KMPC_API_TASK_WITH_DEPS,
  KMPC_API_WAIT_DEPS,
  KMPC_API_TASKLOOP,
  KMPC_API_THREADPRIVATE,
  KMPC_API_PUSH_NUM_TEAMS,
  KMPC_API_FORK_TEAMS,
  KMPC_API_DIST_FOR_STATIC_INIT,
  KMPC_API_DIST_DISPATCH_INIT,
  KMPC_API_PUSH_PROC_BIND,
  KMPC_API_ATOMIC_RD,
  KMPC_API_ATOMIC_WR,
  KMPC_API_N_ENTRIES /* <-- Always last */
};

#endif /* __KMP_RUNTIME_H__ */
