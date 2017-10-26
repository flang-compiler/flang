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

#include "stdioInterf.h"
#include "fioMacros.h"

/* strided-version of bcopy */
/* handles overlaps correctly if strides are positive */
/* requires explicit data item length, not type */
/* the length does not necessary equal the alignment, since complex
   types can be aligned on their component length boundaries */

/* for C90, alignment is determined by the byte offset in the leftmost
   3 bits of a pointer */

#define ALIGNMASK(typ) (sizeof(typ) - 1)

void
__fort_bcopysl(char *to, char *fr, size_t cnt, size_t tostr, size_t frstr,
              size_t len)
{
  size_t i, j;
  unsigned long n;
#if !defined(WIN64)
  long k;
#else
  long long k;
#endif

  if (tostr == 1 && frstr == 1) {
    __fort_bcopy(to, fr, cnt * len);
    return;
  }

  n = (unsigned long)to | (unsigned long)fr;

  if (to < fr) {
    if ((n & ALIGNMASK(double)) == 0) {
      if (len == 2 * sizeof(double)) {
        tostr *= 2;
        frstr *= 2;
        for (i = j = 0; cnt > 0; cnt--, i += tostr, j += frstr) {
          ((double *)to)[i] = ((double *)fr)[j];
          ((double *)to)[i + 1] = ((double *)fr)[j + 1];
        }
        return;
      }
      if (len == sizeof(double)) {
        for (i = j = 0; cnt > 0; cnt--, i += tostr, j += frstr) {
          ((double *)to)[i] = ((double *)fr)[j];
        }
        return;
      }
    }

    if ((n & ALIGNMASK(int)) == 0) {
      if (len == 2 * sizeof(int)) {
        tostr *= 2;
        frstr *= 2;
        for (i = j = 0; cnt > 0; cnt--, i += tostr, j += frstr) {
          ((int *)to)[i] = ((int *)fr)[j];
          ((int *)to)[i + 1] = ((int *)fr)[j + 1];
        }
        return;
      }
      if (len == sizeof(int)) {
        for (i = j = 0; cnt > 0; cnt--, i += tostr, j += frstr) {
          ((int *)to)[i] = ((int *)fr)[j];
        }
        return;
      }
    }

    if ((n & ALIGNMASK(short)) == 0) {
      if (len == 2 * sizeof(short)) {
        tostr *= 2;
        frstr *= 2;
        for (i = j = 0; cnt > 0; cnt--, i += tostr, j += frstr) {
          ((short *)to)[i] = ((short *)fr)[j];
          ((short *)to)[i + 1] = ((short *)fr)[j + 1];
        }
        return;
      }
      if (len == sizeof(short)) {
        for (i = j = 0; cnt > 0; cnt--, i += tostr, j += frstr) {
          ((short *)to)[i] = ((short *)fr)[j];
        }
        return;
      }
    }

    tostr *= len;
    frstr *= len;
    for (i = j = 0; cnt > 0; cnt--, i += tostr, j += frstr) {
      for (k = 0; k < len; k++) {
        to[i + k] = fr[j + k];
      }
    }
    return;
  }

  if (to > fr || tostr != frstr) {
    i = (cnt - 1) * tostr;
    j = (cnt - 1) * frstr;

    if ((n & ALIGNMASK(double)) == 0) {
      if (len == 2 * sizeof(double)) {
        tostr *= 2;
        frstr *= 2;
        i *= 2;
        j *= 2;
        for (; cnt > 0; cnt--, i -= tostr, j -= frstr) {
          ((double *)to)[i + 1] = ((double *)fr)[j + 1];
          ((double *)to)[i] = ((double *)fr)[j];
        }
        return;
      }
      if (len == sizeof(double)) {
        for (; cnt > 0; cnt--, i -= tostr, j -= frstr) {
          ((double *)to)[i] = ((double *)fr)[j];
        }
        return;
      }
    }

    if ((n & ALIGNMASK(int)) == 0) {
      if (len == 2 * sizeof(int)) {
        tostr *= 2;
        frstr *= 2;
        i *= 2;
        j *= 2;
        for (; cnt > 0; cnt--, i -= tostr, j -= frstr) {
          ((int *)to)[i + 1] = ((int *)fr)[j + 1];
          ((int *)to)[i] = ((int *)fr)[j];
        }
        return;
      }
      if (len == sizeof(int)) {
        for (; cnt > 0; cnt--, i -= tostr, j -= frstr) {
          ((int *)to)[i] = ((int *)fr)[j];
        }
        return;
      }
    }

    if ((n & ALIGNMASK(short)) == 0) {
      if (len == 2 * sizeof(short)) {
        tostr *= 2;
        frstr *= 2;
        i *= 2;
        j *= 2;
        for (; cnt > 0; cnt--, i -= tostr, j -= frstr) {
          ((short *)to)[i + 1] = ((short *)fr)[j + 1];
          ((short *)to)[i] = ((short *)fr)[j];
        }
        return;
      }
      if (len == sizeof(short)) {
        for (; cnt > 0; cnt--, i -= tostr, j -= frstr) {
          ((short *)to)[i] = ((short *)fr)[j];
        }
        return;
      }
    }

    tostr *= len;
    frstr *= len;
    i *= len;
    j *= len;
    for (; cnt > 0; cnt--, i -= tostr, j -= frstr) {
      for (k = len - 1; k >= 0; k--) {
        to[i + k] = fr[j + k];
      }
    }
  }
}
