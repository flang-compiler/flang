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

#include <stdio.h>
#include <stdlib.h>

#if (defined(WIN32) || defined(WIN64))
extern void *_aligned_malloc();
extern void _aligned_free();
#else
extern int posix_memalign();
#endif

void *
__aligned_malloc(size_t sz, size_t aln)
{
  char *q;
  size_t need;

/*
 * MINALN must be a multiple of sizeof(ptr) and sufficient for aligned
 * accesses
 */
#define MINALN 16

  if (!aln || aln < MINALN)
    aln = MINALN;
  else {
    /* make sure aln is a power of two */
    int s;
    s = 0;
    while ((aln & 1) == 0) {
      s++;
      aln >>= 1;
    }
    aln = 1 << s;
  }
  need = sz + MINALN;
#if (defined(WIN32) || defined(WIN64))
  q = _aligned_malloc(need, aln);
  if (!q)
    return NULL;
#else
  if (posix_memalign((void**)&q, aln, need))
    return NULL;
#endif
  return q;
}
void
__aligned_free(void *p)
{
#if (defined(WIN32) || defined(WIN64))
  _aligned_free(p);
#else
  free(p);
#endif
  return;
}

