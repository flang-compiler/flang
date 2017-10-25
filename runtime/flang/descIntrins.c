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

/* clang-format off */

/* FIXME: is this necessary for Flang?*/

/** \file
 * These routines are substituted for intrinsic procedures which are
 * passed as actual arguments.  The intrinsics don't expect
 * descriptors -- these routines do.
 */

#include <string.h>
#include "fioMacros.h"
/* macros for entries */
#if defined(_WIN32) && !defined(_WIN32) && !defined(UXOBJS) && !defined(CROBJS)
#pragma global - x 121 0x20000
#define ENTFTN_MS(UC) WIN_EXP __attribute__((stdcall)) UC
#elif defined(_WIN32) && defined(_WIN32)
#define ENTFTN_MS I8
#endif

/*
      integer function ENTFTN(len)(string)
      character*(*) string
*/

__INT_T
ENTFTN(LEN, len)
(DCHAR(string) DCLEN(string))
{
  return CLEN(string);
}

/* Version of ENTFTN(len) that never takes a descriptor. */
__INT_T
ENTFTN(LENX, lenx)(DCHAR(string) DCLEN(string)) { return CLEN(string); }

__INT8_T
ENTFTN(KLEN, klen)
(DCHAR(string) DCLEN(string))
{
  return CLEN(string);
}

/* Version of ENTFTN(klenx) that never takes a descriptor. */
__INT8_T
ENTFTN(KLENX, klenx)(DCHAR(string) DCLEN(string)) { return CLEN(string); }

/*
      Per the standard, the procedural version of index does not accept
      the back argument:

      integer function ENTFTN(index)(string, substring)
      character*(*) string, substring
*/
__INT_T
ENTFTN(INDEX, index)
(DCHAR(string), DCHAR(substring) DCLEN(string) DCLEN(substring))
{
  int i, n;

  n = CLEN(string) - CLEN(substring);
  if (n < 0)
    return 0;

  if (CLEN(substring) == 0)
    return 1;
  for (i = 0; i <= n; ++i) {
    if (CADR(string)[i] == CADR(substring)[0] &&
        strncmp(CADR(string) + i, CADR(substring), CLEN(substring)) == 0)
      return i + 1;
  }
  return 0;
}

/** \brief version of index that takes no descriptor */
__INT_T
ENTFTN(INDEXX, indexx)
(DCHAR(string), DCHAR(substring) DCLEN(string) DCLEN(substring))
{
  return ENTFTN(INDEX, index)(CADR(string), CADR(substring),
                              CLEN(string), CLEN(substring));
}

__INT8_T
ENTFTN(KINDEX, kindex)
(DCHAR(string), DCHAR(substring) DCLEN(string) DCLEN(substring))
{
  int i, n;

  n = CLEN(string) - CLEN(substring);
  if (n < 0)
    return 0;

  if (CLEN(substring) == 0)
    return 1;
  for (i = 0; i <= n; ++i) {
    if (CADR(string)[i] == CADR(substring)[0] &&
        strncmp(CADR(string) + i, CADR(substring), CLEN(substring)) == 0)
      return i + 1;
  }
  return 0;
}

/** \brief version of index that takes no descriptor */
__INT8_T
ENTFTN(KINDEXX, kindexx)
(DCHAR(string), DCHAR(substring) DCLEN(string) DCLEN(substring))
{
  return ENTFTN(KINDEX, kindex)(CADR(string), CADR(substring),
                                CLEN(string), CLEN(substring));
}

#if defined(TARGET_WIN)

/** \brief Version of pg_len that never takes a descriptor.
 * For cref , mixedstrlen
 */
__INT_T
ENTFTN(LENX, lenx_cr)(DCHAR(string) DCLEN(string)) { return CLEN(string); }

/* For cref , nomixedstrlen*/
__INT_T
ENTFTN(LENX, lenx_cr_nm)(DCHAR(string) DCLEN(string)) { return CLEN(string); }

/** \brief for cref, mixedstrlen*/
__INT8_T
ENTFTN(KLENX, klenx_cr)(DCHAR(string) DCLEN(string)) { return CLEN(string); }

/* for cref, nomixedstrlen*/
__INT8_T
ENTFTN(KLENX, klenx_cr_nm)(DCHAR(string) DCLEN(string)) { return CLEN(string); }

/* For cref, mixed strlen*/
__INT_T
ENTFTN(INDEXX, indexx_cr)
(DCHAR(string) DCLEN(string), DCHAR(substring) DCLEN(substring))
{
  return ENTFTN(INDEX, index)(CADR(string), CADR(substring),
                              CLEN(string), CLEN(substring));
}

/** \brief For cref, nomixed strlen*/
__INT_T
ENTFTN(INDEXX, indexx_cr_nm)
(DCHAR(string), DCHAR(substring) DCLEN(string) DCLEN(substring))
{
  return ENTFTN(INDEX, index)(CADR(string), CADR(substring),
                              CLEN(string), CLEN(substring));
}

/** \brief* for cref, mixedstrlen */
__INT8_T
ENTFTN(KINDEXX, kindexx_cr)
(DCHAR(string) DCLEN(string), DCHAR(substring) DCLEN(substring))
{
  return ENTFTN(KINDEX, kindex)(CADR(string), CADR(substring),
                                CLEN(string), CLEN(substring));
}

/** \brief* for cref, nomixedstrlen */
__INT8_T
ENTFTN(KINDEXX, kindexx_cr_nm)
(DCHAR(string), DCHAR(substring) DCLEN(string) DCLEN(substring))
{
  return ENTFTN(KINDEX, kindex)(CADR(string), CADR(substring),
                                CLEN(string), CLEN(substring));
}

#endif

#if defined(_WIN32)

/* functions here follow the msfortran/mscall conventions */

__INT_T
ENTFTN_MS(PGHPF_LEN)
(DCHAR(string) DCLEN(string))
{
  return CLEN(string);
}

/** \brief Version of pg_len that never takes a descriptor.
 * This is necessary for pghpf -Mf90
 */
__INT_T
ENTFTN_MS(PGHPF_LENX)(DCHAR(string) DCLEN(string)) { return CLEN(string); }

/* Version of pg_len that never takes a descriptor.
 * This is necessary for pghpf -Mf90, -Miface=nomixed_str_len_arg */
__INT_T
ENTFTN_MS(PGHPF_LENX_NM)(DCHAR(string) DCLEN(string)) { return CLEN(string); }

__INT8_T
ENTFTN_MS(PGHPF_KLEN)
(DCHAR(string) DCLEN(string))
{
  return CLEN(string);
}

/* Version of pg_lenx that never takes a descriptor.
 * This is necessary for pghpf -Mf90 */
__INT8_T
ENTFTN_MS(PGHPF_KLENX)(DCHAR(string) DCLEN(string)) { return CLEN(string); }

/** \brief Version of lenx that never takes a descriptor.  */
__INT8_T
ENTFTN_MS(PGHPF_KLENX_NM)(DCHAR(string) DCLEN(string)) { return CLEN(string); }

/*
      Per the standard, the procedural version of index does not accept
      the back argument:

      integer function index(string, substring)
      character*(*) string, substring
*/
/*   pghpf versions are passed descriptors; pgf90 versions are not. */
__INT_T
ENTFTN_MS(PGHPF_INDEX)
(DCHAR(string) DCLEN(string), DCHAR(substring) DCLEN(substring))
{
  int i, n;

  n = CLEN(string) - CLEN(substring);
  if (n < 0)
    return 0;

  if (CLEN(substring) == 0)
    return 1;
  for (i = 0; i <= n; ++i) {
    if (CADR(string)[i] == CADR(substring)[0] &&
        strncmp(CADR(string) + i, CADR(substring), CLEN(substring)) == 0)
      return i + 1;
  }
  return 0;
}

/** \brief version of index that takes no descriptor, used in pghpf
 * -Mf90
 */
__INT_T
ENTFTN_MS(PGHPF_INDEXX)
(DCHAR(string) DCLEN(string), DCHAR(substring) DCLEN(substring))
{
  return ENTFTN(INDEX, index)(CADR(string), CADR(substring),
                              CLEN(string), CLEN(substring));
}

/** \brief Version of index that takes no descriptor, used in pghp 
 * -Miface=nomixedstrlen
 */
__INT_T
ENTFTN_MS(PGHPF_INDEXX_NM)
(DCHAR(string), DCHAR(substring) DCLEN(string) DCLEN(substring))
{
  return ENTFTN(INDEX, index)(CADR(string), CADR(substring),
                              CLEN(string), CLEN(substring));
}

__INT8_T
ENTFTN_MS(PGHPF_KINDEX)
(DCHAR(string) DCLEN(string), DCHAR(substring) DCLEN(substring))
{
  int i, n;

  n = CLEN(string) - CLEN(substring);
  if (n < 0)
    return 0;

  if (CLEN(substring) == 0)
    return 1;
  for (i = 0; i <= n; ++i) {
    if (CADR(string)[i] == CADR(substring)[0] &&
        strncmp(CADR(string) + i, CADR(substring), CLEN(substring)) == 0)
      return i + 1;
  }
  return 0;
}

/** \brief version of index that takes no descriptor, used in pghpf
 * -Mf90 */
__INT8_T
ENTFTN_MS(PGHPF_KINDEXX)
(DCHAR(string) DCLEN(string), DCHAR(substring) DCLEN(substring))
{
  return ENTFTN(KINDEX, kindex)(CADR(string), CADR(substring),
                                CLEN(string), CLEN(substring));
}

/** \brief version of index that takes no descriptor,
 * -Miface=nomixedstrlen used in pghpf 
 */
__INT8_T
ENTFTN_MS(PGHPF_KINDEXX_NM)
(DCHAR(string), DCHAR(substring) DCLEN(string) DCLEN(substring))
{
  return ENTFTN(KINDEX, kindex)(CADR(string), CADR(substring),
                                CLEN(string), CLEN(substring));
}

#endif
