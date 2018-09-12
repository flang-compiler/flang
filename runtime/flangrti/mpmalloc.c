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

#ifdef TARGET_LINUX
#include <features.h>
#endif

#include <stdlib.h>
#include "llcrit.h"
#ifndef __GNU_LIBRARY__
MP_SEMAPHORE(static, sem);
#endif

void *
_mp_malloc(size_t n)
{
  void *p;

#ifndef __GNU_LIBRARY__
  _mp_p(&sem);
#endif
  p = malloc(n);
#ifndef __GNU_LIBRARY__
  _mp_v(&sem);
#endif
  return (p);
}

void *
_mp_calloc(size_t n, size_t t)
{
  void *p;

#ifndef __GNU_LIBRARY__
  _mp_p(&sem);
#endif
  p = calloc(n, t);
#ifndef __GNU_LIBRARY__
  _mp_v(&sem);
#endif
  return (p);
}

void *
_mp_realloc(void *p, size_t n)
{
  void *q;

#ifndef __GNU_LIBRARY__
  _mp_p(&sem);
#endif
  q = realloc(p, n);
#ifndef __GNU_LIBRARY__
  _mp_v(&sem);
#endif
  return (q);
}

void
_mp_free(void *p)
{
  if (p == 0)
    return;
#ifndef __GNU_LIBRARY__
  _mp_p(&sem);
#endif
  free(p);
#ifndef __GNU_LIBRARY__
  _mp_v(&sem);
#endif
}
