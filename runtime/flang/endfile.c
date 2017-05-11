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

/** \file
 * \brief
 * Implement Fortran ENDFILE statement.
 */

#include "global.h"

static int
_f90io_endfile(__INT_T *unit, __INT_T *bitv, __INT_T *iostat)
{
  FIO_FCB *f;

  __fortio_errinit03(*unit, *bitv, iostat, "ENDFILE");
  if (ILLEGAL_UNIT(*unit))
    return __fortio_error(FIO_EUNIT);

  /*	call rwinit to get FCB pointer, do error checking, and truncate
      file if necessary:  */

  f = __fortio_rwinit(*unit, FIO_UNFORMATTED, 0L, 2 /*endfile*/);
  if (f == NULL)
    return ERR_FLAG;

  f->eof_flag = TRUE;
  return 0; /* no error occurred */
}

__INT_T
ENTF90IO(ENDFILE, endfile)(unit, bitv, iostat) __INT_T *unit;
__INT_T *bitv;
__INT_T *iostat;
{
  int s = 0;

  __fort_status_init(bitv, iostat);
  if (LOCAL_MODE || GET_DIST_LCPU == GET_DIST_IOPROC)
    s = _f90io_endfile(unit, bitv, iostat);
  __fortio_errend03();
  return DIST_STATUS_BCST(s);
}

__INT_T
ENTCRF90IO(ENDFILE, endfile)(unit, bitv, iostat) __INT_T *unit;
__INT_T *bitv;
__INT_T *iostat;
{
  int s = 0;
  s = _f90io_endfile(unit, bitv, iostat);
  __fortio_errend03();
  return s;
}
