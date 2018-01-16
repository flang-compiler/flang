/*
 * Copyright (c) 1993-2018, NVIDIA CORPORATION.  All rights reserved.
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
 *  \brief customized storage allocation routines for compiler.
 */
#include "gbldefs.h"
#include "global.h"
#include "error.h"

#if DEBUG
#define TRACE(str, d) \
  if (DBGBIT(7, 1))   \
  fprintf(stderr, str, d)
#else
#define TRACE(a, b)
#endif

char *
sccalloc(BIGUINT64 nbytes)
{
  char *p;

  TRACE("sccalloc called to get %ld bytes\n", nbytes);
  p = malloc(nbytes);
  if (p == NULL)
    errfatal(7);
#if DEBUG
  if (DBGBIT(0, 0x20000)) {
    char *q, cc;
    unsigned int s;
    /* fill with junk */
    cc = 0xa6;
    for (s = nbytes, q = p; s; --s, ++q) {
      *q = cc;
      cc = (cc << 1) | (cc >> 7);
    }
  }
#endif
  TRACE("sccalloc returns %p\n", (void *)p);
  return p;
}

/*****************************************************************/

void
sccfree(char *ap)
{
  TRACE("sccfree called to free %p\n", (void *)ap);
  free(ap);
}

/**********************************************************/

char *
sccrelal(char *pp, BIGUINT64 nbytes)
{
  char *q;
  TRACE("sccrelal called to realloc %p\n", (void *)pp);
  q = realloc(pp, nbytes);
  if (q == NULL)
    errfatal(7);
  TRACE("sccrelal returns %p\n", (void *)q);
  return q;
}

#if DEBUG

#include <string.h>

void
bjunk(void *p, BIGUINT64 n)
{
  memset(p, -99, n);
}

#endif
