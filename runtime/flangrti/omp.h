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

#ifndef _PGOMP_H
#define _PGOMP_H
/* simple lock */

typedef struct omp_lock_t {
  void * _lk;
} omp_lock_t;

typedef struct omp_nest_lock_t {
  void * _lk;
} omp_nest_lock_t;

typedef enum omp_sched_t {
  omp_sched_static = 1,
  omp_sched_dynamic = 2,
  omp_sched_guided = 3,
  omp_sched_auto = 4
} omp_sched_t;

extern void omp_set_num_threads(int n);
extern int omp_get_thread_num(void);
extern int omp_get_num_procs(void);
extern int omp_get_num_threads(void);
extern int omp_get_max_threads(void);
extern int omp_in_parallel(void);
extern int omp_in_final(void);
extern void omp_set_dynamic(int n);
extern int omp_get_dynamic(void);
extern void omp_set_nested(int n);
extern int omp_get_nested(void);
extern void omp_init_lock(omp_lock_t *s);
extern void omp_destroy_lock(omp_lock_t *s);
extern void omp_set_lock(omp_lock_t *s);
extern void omp_unset_lock(omp_lock_t *s);
extern int omp_test_lock(omp_lock_t *s);
extern void omp_init_nest_lock(omp_nest_lock_t *s);
extern void omp_destroy_nest_lock(omp_nest_lock_t *s);
extern void omp_set_nest_lock(omp_nest_lock_t *s);
extern void omp_unset_nest_lock(omp_nest_lock_t *s);
extern int omp_test_nest_lock(omp_nest_lock_t *s);
extern double omp_get_wtime(void);
extern double omp_get_wtick(void);
extern long omp_get_stack_size(void);
extern void omp_set_stack_size(long l);
extern int omp_get_thread_limit(void);
extern void omp_set_max_active_levels(int);
extern int omp_get_max_active_levels(void);
extern int omp_get_level(void);
extern int omp_get_ancestor_thread_num(int);
extern int omp_get_team_size(int);
extern int omp_get_active_level(void);
extern void omp_set_schedule(omp_sched_t, int);
extern void omp_get_schedule(omp_sched_t *, int *);
extern int omp_get_initial_device();
extern int omp_get_default_device();
extern void omp_set_default_device(int);
#include <stdlib.h>
extern void* omp_target_alloc(size_t, int);
extern void omp_target_free(void*, int);
extern int omp_target_memcpy(void*, void*, size_t, size_t, size_t, int, int);

typedef int _Atomic_word;
extern void _mp_atomic_add(int *, int);
extern void _mp_exchange_and_add(int *, int);

#endif /*_PGOMP_H*/
