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
 * \brief Implements Fortran FLUSH statement.
 */

#include "global.h"
#include "async.h"

int ENTF90IO(FLUSH, flush)(unit, bitv, iostat) __INT_T *unit;
__INT_T *bitv;
__INT_T *iostat;
{
  FIO_FCB *f;
  int s = 0;

  __fort_status_init(bitv, iostat);
  __fortio_errinit03(*unit, *bitv, iostat, "FLUSH");
  if (ILLEGAL_UNIT(*unit)) {/* check for illegal unit number */
    s = __fortio_error(FIO_EUNIT);
    __fortio_errend03();
    return s;
  }

  f = __fortio_find_unit(*unit);

  if (f) {

    /* check for outstanding async i/o */

    if (f->asy_rw) {/* stop any async i/o */
      f->asy_rw = 0;
      if (Fio_asy_disable(f->asyptr) == -1) {
        s = (__fortio_error(__io_errno()));
        __fortio_errend03();
        return s;
      }
    }

    if (__io_fflush(f->fp) != 0) {
      s = __fortio_error(__io_errno());
      __fortio_errend03();
      return s;
    }
  }

  __fortio_errend03();
  return 0; /* no error occurred */
}
