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
 * \brief Implements Fortran REWIND statement.
 */

#include "global.h"
#include "async.h"
#include "stdioInterf.h"

static int
_f90io_rewind(__INT_T *unit, __INT_T *bitv, __INT_T *iostat)
{
  FIO_FCB *f;

  __fortio_errinit03(*unit, *bitv, iostat, "REWIND");
  if (ILLEGAL_UNIT(*unit)) /* check for illegal unit number */
    return __fortio_error(FIO_EUNIT);

  f = __fortio_find_unit(*unit);

  if (f) {
    if (f->acc == FIO_DIRECT) /* can't rewind direct access file */
      /* treat rewind of direct acc. file as no-op to avoid complaints */
      return 0 /*__fortio_error(FIO_EDIRECT)*/;

    /* check for outstanding async i/o */

    if (f->asy_rw) { /* stop any async i/o */
      f->asy_rw = 0;
      if (Fio_asy_disable(f->asyptr) == -1) {
        return (__fortio_error(__io_errno()));
      }
    }

    /* append carriage return (maybe) */

    if (f->nonadvance) {
      f->nonadvance = FALSE;
#if defined(WINNT)
      if (__fortio_binary_mode(f->fp))
        __io_fputc('\r', f->fp);
#endif
      __io_fputc('\n', f->fp);
      if (__io_ferror(f->fp))
        return __io_errno();
    }

    if (__io_fseek(f->fp, 0L, SEEK_SET) != 0)
      return __fortio_error(__io_errno());

    f->nextrec = 1;
    f->coherent = 0;
    f->eof_flag = FALSE;
    f->truncflag = TRUE;
    f->skip = 0;
  }

  return 0; /* no error occurred */
}

__INT_T
ENTF90IO(REWIND, rewind)(__INT_T *unit, __INT_T *bitv, __INT_T *iostat)
{
  int s = 0;

  __fort_status_init(bitv, iostat);
  if (LOCAL_MODE || GET_DIST_LCPU == GET_DIST_IOPROC)
    s = _f90io_rewind(unit, bitv, iostat);
  __fortio_errend03();
  return DIST_STATUS_BCST(s);
}

int ENTCRF90IO(REWIND, rewind)(__INT_T *unit, __INT_T *bitv, __INT_T *iostat)
{
  int s = 0;
  s = _f90io_rewind(unit, bitv, iostat);
  __fortio_errend03();
  return s;
}
