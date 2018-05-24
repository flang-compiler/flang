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

/*
 *  ompstubs.c:
 *
 *  libpgc stubs for omp_ routines not appearing in crit.o & omp.o for
 *  non-x64/x86 targets.
 *
 */

#ifndef OMP_50
#define OMP_50
#endif

#include <ompstubs.h>

#ifdef __cplusplus
extern "C" {
#endif

int omp_get_thread_limit_(void)
{
  return omp_get_thread_limit();
}

int omp_in_parallel_(void)
{
  return omp_in_parallel();
}

void omp_set_num_threads_(int num_threads)
{
  omp_set_num_threads(num_threads);
}

void omp_set_lock_(omp_lock_t *sem)
{
  omp_set_lock(sem);
}

void omp_unset_lock_(omp_lock_t *sem)
{
  omp_unset_lock(sem);
}

int omp_get_num_procs_(void)
{
  return omp_get_num_procs();
}

int omp_get_num_threads_(void)
{
  return omp_get_num_threads();
}

int omp_get_max_threads_(void)
{
  return omp_get_max_threads();
}

int omp_get_thread_num_(void)
{
  return omp_get_thread_num();
}

void
omp_set_dynamic_(int dynamic_threads)
{
  omp_set_dynamic(dynamic_threads);
}

int omp_get_dynamic_(void)
{
  return omp_get_dynamic();
}

void omp_set_nested_(int nested)
{
  omp_set_nested(nested);
}

int omp_get_nested_(void)
{
  return omp_get_nested();
}

void omp_set_schedule_(omp_sched_t kind, int modifier)
{
  omp_set_schedule(kind, modifier);
}

void omp_get_schedule_(omp_sched_t *kind, int *modifier)
{
  omp_get_schedule(kind, modifier);
}

void omp_set_max_active_levels_(int max_active_levels)
{
  omp_set_max_active_levels(max_active_levels);
}

int omp_get_max_active_levels_(void)
{
  return omp_get_max_active_levels();
}

int omp_get_level_(void)
{
  return omp_get_level();
}

int
omp_get_ancestor_thread_num_(int level)
{
  return omp_get_ancestor_thread_num(level);
}

int omp_get_team_size_(int level)
{
  return omp_get_team_size(level);
}

int omp_get_active_level_(void)
{
  return omp_get_active_level();
}

void omp_init_lock_(omp_lock_t *s)
{
  omp_init_lock(s);
}

void omp_destroy_lock_(omp_lock_t *arg)
{
  omp_destroy_lock(arg);
}

int omp_test_lock_(omp_lock_t *arg)
{
  return omp_test_lock(arg);
}

void omp_init_nest_lock_(omp_nest_lock_t *arg)
{
  omp_init_nest_lock(arg);
}

void omp_destroy_nest_lock_(omp_nest_lock_t *arg)
{
  omp_destroy_nest_lock(arg);
}

void omp_set_nest_lock_(omp_nest_lock_t *arg)
{
  omp_set_nest_lock(arg);
}

void omp_unset_nest_lock_(omp_nest_lock_t *arg)
{
  omp_unset_nest_lock(arg);
}

int omp_test_nest_lock_(omp_nest_lock_t *arg)
{
  return omp_test_nest_lock(arg);
}

int omp_get_cancellation_()
{
  return omp_get_cancellation();
}

omp_proc_bind_t omp_get_proc_bind_()
{
  return omp_get_proc_bind();
}

int omp_get_num_places_()
{
  return omp_get_num_places();
}

int omp_get_place_num_procs_(int placenum)
{
  return omp_get_place_num_procs(placenum);
}

void omp_get_place_proc_ids_(int place_num, int *ids)
{
  omp_get_place_proc_ids(place_num, ids);
}

int omp_get_place_num_()
{
  return omp_get_place_num();
}


int omp_get_partition_num_places_()
{
  return omp_get_partition_num_places();
}

void omp_get_partition_place_nums_(int *place_nums)
{
  omp_get_partition_place_nums(place_nums);
}

void
omp_set_default_device_(int device_num)
{
  omp_set_default_device(device_num);
}

int omp_get_default_device_(void)
{
  return omp_get_default_device();
}

int
omp_get_num_devices_(void)
{
  return omp_get_num_devices();
}

int omp_get_num_teams_(void)
{
  return omp_get_num_teams();
}

int omp_get_team_num_(void)
{
  return omp_get_team_num();
}

int omp_is_initial_device_(void)
{
  return omp_is_initial_device();
}

int omp_get_initial_device_(void)
{
  return omp_get_initial_device();
}

int omp_get_max_task_priority_(void)
{
  return omp_get_max_task_priority();
}

void omp_init_nest_lock_with_hint_(omp_nest_lock_t *arg,
                                   omp_lock_hint_t hint)
{
  omp_init_nest_lock_with_hint(arg, hint);
}

double omp_get_wtime_(void)
{
  return omp_get_wtime();
}

double omp_get_wtick_(void)
{
  return omp_get_wtick();
}

extern kmp_int32 __kmpc_global_thread_num(void *id);
kmp_int32
__kmpc_global_thread_num_(void *id)
{
  return __kmpc_global_thread_num(id);
}

extern void __kmpc_critical(ident_t *id, kmp_int32 tid,
                            kmp_critical_name *sem);
void __kmpc_critical_(ident_t *id, kmp_int32 tid,
                      kmp_critical_name *sem)
{
  __kmpc_critical(id, tid, sem);
}

extern void __kmpc_end_critical(ident_t *id, kmp_int32 tid,
                                kmp_critical_name *sem);
void __kmpc_end_critical_(ident_t *id, kmp_int32 tid,
                          kmp_critical_name *sem)
{
  __kmpc_end_critical(id, tid, sem);
}

extern void* __kmpc_threadprivate_cached(ident_t *loc, kmp_int32 tid,
                                         void *data, size_t size,
                                         void ***cache);
void* __kmpc_threadprivate_cached_(ident_t *loc, kmp_int32 tid,
                                   void *data, size_t size,
                                   void ***cache)
{
  return __kmpc_threadprivate_cached(loc, tid, data, size, cache);
}

extern void __kmpc_barrier(ident_t *id, kmp_int32 tid);
void __kmpc_barrier_(ident_t *id, kmp_int32 tid)
{
  __kmpc_barrier(id, tid);
}

extern void* __kmpc_threadprivate(ident_t *id, kmp_int32 tid,
                                  void *data, size_t size);
void* __kmpc_threadprivate_(ident_t *id, kmp_int32 tid, void *data,
                            size_t size)
{
  return __kmpc_threadprivate(id, tid, data, size);
}

extern void __kmpc_fork_call(ident_t *loc, kmp_int32 argc,
                             void *microtask, ...);

extern void __kmpc_for_static_init_4u(ident_t *loc,
                                      kmp_int32 gtid,
                                      kmp_int32 schedtype,
                                      kmp_int32 *plastiter,
                                      kmp_int64 *plower,
                                      kmp_int64 *pupper,
                                      kmp_int64 *pstride,
                                      kmp_int64 incr,
                                      kmp_int64 chunk);

extern void __kmpc_for_static_init_8u(ident_t *loc,
                                      kmp_int32 gtid,
                                      kmp_int32 schedtype,
                                      kmp_int32 *plastiter,
                                      kmp_int64 *plower,
                                      kmp_int64 *pupper,
                                      kmp_int64 *pstride,
                                      kmp_int64 incr,
                                      kmp_int64 chunk);

extern void __kmpc_for_static_init_4(ident_t *loc,
                                     kmp_int32 gtid,
                                     kmp_int32 schedtype,
                                     kmp_int32 *plastiter,
                                     kmp_int64 *plower,
                                     kmp_int64 *pupper,
                                     kmp_int64 *pstride,
                                     kmp_int64 incr,
                                     kmp_int64 chunk);

extern void __kmpc_for_static_init_8(ident_t *loc,
                                     kmp_int32 gtid,
                                     kmp_int32 schedtype,
                                     kmp_int32 *plastiter,
                                     kmp_int64 *plower,
                                     kmp_int64 *pupper,
                                     kmp_int64 *pstride,
                                     kmp_int64 incr,
                                     kmp_int64 chunk);

#ifdef __cplusplus
} /* extern "C" */
#endif

