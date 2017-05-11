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

/* mp-safe wrappers for malloc, etc. */

#include <stdlib.h>
extern void _mp_p(long*);
extern void _mp_v(long*);

static long sem = 0;

void *
_mp_malloc(size_t n)
{
  void *p;

  _mp_p(&sem);
  p = malloc(n);
  _mp_v(&sem);
  return (p);
}

void *
_mp_calloc(size_t n, size_t t)
{
  void *p;

  _mp_p(&sem);
  p = calloc(n, t);
  _mp_v(&sem);
  return (p);
}

void *
_mp_realloc(void *p, size_t n)
{
  void *q;

  _mp_p(&sem);
  q = realloc(p, n);
  _mp_v(&sem);
  return (q);
}

void
_mp_free(void *p)
{
  if (p == 0)
    return;
  _mp_p(&sem);
  free(p);
  _mp_v(&sem);
}
