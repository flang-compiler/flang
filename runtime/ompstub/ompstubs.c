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

#include <stdio.h>
#include "komp.h"

#ifndef TARGET_WIN

#ifndef TARGET_OSX

/* The Linux way */
#define OMPSTUB(type, fun, ...) \
extern type fun(__VA_ARGS__) __attribute__((weak)); \
\
type fun(__VA_ARGS__)

#else /* TARGET_OSX */

/* The OSX way */
#define OMPSTUB(type, fun, ...) \
type fun(__VA_ARGS__) __attribute__((weak)); \
\
type fun(__VA_ARGS__)

#endif /* TARGET_OSX */

#else /* TARGET_WIN */

#define OMPSTUB(type, fun, ...) \
type fun(__VA_ARGS__)

#endif /* TARGET_WIN */

OMPSTUB(int, omp_get_thread_limit, void)
{
  return 1;
}

OMPSTUB(int, omp_get_thread_limit_, void)
{
  return 1;
}

OMPSTUB(int, omp_in_parallel, void)
{
  return 0;
}

OMPSTUB(int, omp_in_parallel_, void)
{
  return 0;
}

OMPSTUB(void, omp_set_num_threads, int num_threads)
{
}

OMPSTUB(void, omp_set_num_threads_, int *num_threads)
{
}

OMPSTUB(void, omp_set_lock, omp_lock_t *sem)
{
}

OMPSTUB(void, omp_set_lock_, omp_lock_t *sem)
{
}

OMPSTUB(void, omp_unset_lock, omp_lock_t *sem)
{
}

OMPSTUB(void, omp_unset_lock_, omp_lock_t *sem)
{
}

OMPSTUB(int, omp_get_num_procs, void)
{
  return 1;
}

OMPSTUB(int, omp_get_num_procs_, void)
{
  return 1;
}

OMPSTUB(int, omp_get_num_threads, void)
{
  return 1;
}

OMPSTUB(int, omp_get_num_threads_, void)
{
  return 1;
}

OMPSTUB(int, omp_get_max_threads, void)
{
  return 1;
}

OMPSTUB(int, omp_get_max_threads_, void)
{
  return 1;
}

OMPSTUB(int, omp_get_thread_num, void)
{
  return 0;
}

OMPSTUB(int, omp_get_thread_num_, void)
{
  return 0;
}

OMPSTUB(void, omp_set_dynamic, int dynamic_threads)
{
}

OMPSTUB(void, omp_set_dynamic_, int *dynamic_threads)
{
}

OMPSTUB(int, omp_get_dynamic, void)
{
  return 0;
}

OMPSTUB(int, omp_get_dynamic_, void)
{
  return 0;
}

OMPSTUB(void, omp_set_nested, int nested)
{
}

OMPSTUB(void, omp_set_nested_, int *nested)
{
}

OMPSTUB(int, omp_get_nested, void)
{
  return 0;
}

OMPSTUB(int, omp_get_nested_, void)
{
  return 0;
}

OMPSTUB(void, omp_set_schedule, omp_sched_t kind, int modifier)
{
}

OMPSTUB(void, omp_set_schedule_, omp_sched_t *kind, int *modifier)
{
}

OMPSTUB(void, omp_get_schedule, omp_sched_t *kind, int *modifier)
{
  *kind = omp_sched_static;
  *modifier = 0;
}

OMPSTUB(void, omp_get_schedule_, omp_sched_t *kind, int *modifier)
{
  *kind = omp_sched_static;
  *modifier = 0;
}

OMPSTUB(void, omp_set_max_active_levels, int max_active_levels)
{
}

OMPSTUB(void, omp_set_max_active_levels_, int *max_active_levels)
{
}

OMPSTUB(int, omp_get_max_active_levels, void)
{
  return 0;
}

OMPSTUB(int, omp_get_max_active_levels_, void)
{
  return 0;
}

OMPSTUB(int, omp_get_level, void)
{
  return 0;
}

OMPSTUB(int, omp_get_level_, void)
{
  return 0;
}

OMPSTUB(int, omp_get_ancestor_thread_num, int level)
{
  if (level == 0) {
    return 0;
  }
  return -1;
}

OMPSTUB(int, omp_get_ancestor_thread_num_, int *level)
{
  if (*level == 0) {
    return 0;
  }
  return -1;
}

OMPSTUB(int, omp_get_team_size, int level)
{
  if (level == 0) {
    return 1;
  }
  return -1;
}

OMPSTUB(int, omp_get_team_size_, int *level)
{
  if (*level == 0) {
    return 1;
  }
  return -1;
}

OMPSTUB(int, omp_get_active_level, void)
{
  return 0;
}

OMPSTUB(int, omp_get_active_level_, void)
{
  return 0;
}

OMPSTUB(void, omp_init_lock, omp_lock_t *s)
{
}

OMPSTUB(void, omp_init_lock_, omp_lock_t *s)
{
}

OMPSTUB(void, omp_destroy_lock, omp_lock_t *arg)
{
}

OMPSTUB(void, omp_destroy_lock_, omp_lock_t *arg)
{
}

OMPSTUB(int, omp_test_lock, omp_lock_t *arg)
{
  return 0;
}

OMPSTUB(int, omp_test_lock_, omp_lock_t *arg)
{
  return 0;
}

OMPSTUB(void, omp_init_nest_lock, omp_nest_lock_t *arg)
{
}

OMPSTUB(void, omp_init_nest_lock_, omp_nest_lock_t *arg)
{
}

OMPSTUB(void, omp_destroy_nest_lock, omp_nest_lock_t *arg)
{
}

OMPSTUB(void, omp_destroy_nest_lock_, omp_nest_lock_t *arg)
{
}

OMPSTUB(void, omp_set_nest_lock, omp_nest_lock_t *arg)
{
}

OMPSTUB(void, omp_set_nest_lock_, omp_nest_lock_t *arg)
{
}

OMPSTUB(void, omp_unset_nest_lock, omp_nest_lock_t *arg)
{
}

OMPSTUB(void, omp_unset_nest_lock_, omp_nest_lock_t *arg)
{
}

OMPSTUB(int, omp_test_nest_lock, omp_nest_lock_t *arg)
{
  return 0;
}

OMPSTUB(int, omp_test_nest_lock_, omp_nest_lock_t *arg)
{
  return 0;
}

OMPSTUB(int, omp_get_cancellation, void)
{
  return 0;
}

OMPSTUB(int, omp_get_cancellation_, void)
{
  return 0;
}

OMPSTUB(omp_proc_bind_t, omp_get_proc_bind_, void)
{
  return 0;
}

OMPSTUB(omp_proc_bind_t, omp_get_proc_bind, void)
{
  return 0;
}

OMPSTUB(int, omp_get_num_places, void)
{
  return 0;
}

OMPSTUB(int, omp_get_num_places_, void)
{
  return 0;
}

OMPSTUB(int, omp_get_place_num_procs, int placenum)
{
  return 0;
}

OMPSTUB(int, omp_get_place_num_procs_, int placenum)
{
  return 0;
}

OMPSTUB(void, omp_get_place_proc_ids, int place_num, int *ids)
{
  return;
}

OMPSTUB(void, omp_get_place_proc_ids_, int place_num, int *ids)
{
  return;
}

OMPSTUB(int, omp_get_place_num, void)
{
  return -1;
}

OMPSTUB(int, omp_get_place_num_, void)
{
  return -1;
}

OMPSTUB(int, omp_get_partition_num_places, void)
{
  return 0;
}

OMPSTUB(int, omp_get_partition_num_places_, void)
{
  return 0;
}

OMPSTUB(void, omp_get_partition_place_nums, int *place_nums)
{
}

OMPSTUB(void, omp_get_partition_place_nums_, int *place_nums)
{
}

OMPSTUB(void, omp_set_default_device, int device_num)
{
}

OMPSTUB(void, omp_set_default_device_, int device_num)
{
}

OMPSTUB(int, omp_get_default_device, void)
{
  return 0;
}

OMPSTUB(int, omp_get_default_device_, void)
{
  return 0;
}

OMPSTUB(int, omp_get_num_devices, void)
{
  return 0;
}

OMPSTUB(int, omp_get_num_devices_, void)
{
  return 0;
}

OMPSTUB(int, omp_get_num_teams, void)
{
  return 1;
}

OMPSTUB(int, omp_get_num_teams_, void)
{
  return 1;
}

OMPSTUB(int, omp_get_team_num, void)
{
  return 0;
}

OMPSTUB(int, omp_get_team_num_, void)
{
  return 0;
}

OMPSTUB(int, omp_is_initial_device, void)
{
  return 1;
}

OMPSTUB(int, omp_is_initial_device_, void)
{
  return 1;
}

OMPSTUB(int, omp_get_initial_device, void)
{
  return -10;
}

OMPSTUB(int, omp_get_initial_device_, void)
{
  return -10;
}

OMPSTUB(int, omp_get_max_task_priority, void)
{
  return 0;
}

OMPSTUB(int, omp_get_max_task_priority_, void)
{
  return 0;
}

OMPSTUB(void, omp_init_nest_lock_with_hint, omp_nest_lock_t *arg, omp_lock_hint_t hint)
{
  omp_init_nest_lock(arg);
}

OMPSTUB(void, omp_init_nest_lock_with_hint_, omp_nest_lock_t *arg, omp_lock_hint_t hint)
{
  omp_init_nest_lock(arg);
}


OMPSTUB(double, omp_get_wtime, void)
{
  /* This function does not provide a working
   * wallclock timer. Replace it with a version
   * customized for the target machine.
   */
  return 0.0;
}

OMPSTUB(double, omp_get_wtime_, void)
{
  /* This function does not provide a working
   * wallclock timer. Replace it with a version
   * customized for the target machine.
   */
  return 0.0;
}

OMPSTUB(double, omp_get_wtick, void)
{
  /* This function does not provide a working
   * clock tick function. Replace it with
   * a version customized for the target machine.
   */
  return 365. * 86400.;
}

OMPSTUB(double, omp_get_wtick_, void)
{
  /* This function does not provide a working
   * clock tick function. Replace it with
   * a version customized for the target machine.
   */
  return 365. * 86400.;
}

OMPSTUB(kmp_int32, __kmpc_global_thread_num, void *id)
{
  return 0;
}

OMPSTUB(kmp_int32, __kmpc_global_thread_num_, void *id)
{
  return 0;
}

OMPSTUB(void, __kmpc_critical, ident_t *id, kmp_int32 tn, kmp_critical_name *sem)
{
}

OMPSTUB(void, __kmpc_critical_, ident_t *id, kmp_int32 *tn, kmp_critical_name *sem)
{
}

OMPSTUB(void, __kmpc_end_critical, ident_t *id, kmp_int32 tn, kmp_critical_name *sem)
{
}

OMPSTUB(void, __kmpc_end_critical_, ident_t *id, kmp_int32 *tn, kmp_critical_name *sem)
{
}

OMPSTUB(void *, __kmpc_threadprivate_cached, ident_t *id, kmp_int32 tn, void *data, size_t size,
                            void ***cache)
{
  return (void *)0;
}

OMPSTUB(void *, __kmpc_threadprivate_cached_, ident_t *id, kmp_int32 *tn, void *data,
                             size_t *size, void ***cache)
{
  return (void *)0;
}

OMPSTUB(void, __kmpc_barrier, ident_t *id, kmp_int32 tn)
{
}

OMPSTUB(void, __kmpc_barrier_, ident_t *id, kmp_int32 *tn)
{
}

OMPSTUB(void *, __kmpc_threadprivate, ident_t *id, kmp_int32 tn, void *data, size_t size)
{
  return (void *)0;
}

OMPSTUB(void *, __kmpc_threadprivate_, ident_t *id, kmp_int32 tn, void *data, size_t size)
{
  return (void *)0;
}

OMPSTUB(void, __kmpc_fork_call, ident_t *loc, kmp_int32 argc, void *microtask, ...)
{
}

OMPSTUB(void, __kmpc_for_static_init_8, ident_t *loc, kmp_int32 gtid, kmp_int32 schedtype, kmp_int32 *plastiter,
                      kmp_int64 *plower, kmp_int64 *pupper,
                      kmp_int64 *pstride, kmp_int64 incr, kmp_int64 chunk )
{
}
