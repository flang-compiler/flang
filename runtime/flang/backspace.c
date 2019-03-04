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
 * \brief Implements Fortran BACKSPACE statement.  */

#include "global.h"
#include "async.h"
#include "stdioInterf.h"

/** \brief this must match RCWSZ defined in unf.c - is the number of bytes used
    to represent the length that is stored with an unformatted record */
#define RCWSZ sizeof(int)

static int
_f90io_backspace(__INT_T *unit, __INT_T *bitv, __INT_T *iostat, int swap_bytes)
{
  FIO_FCB *f;
  FILE *fp;

  __fortio_errinit03(*unit, *bitv, iostat, "BACKSPACE");
  if (ILLEGAL_UNIT(*unit))
    return __fortio_error(FIO_EUNIT);

  f = __fortio_find_unit(*unit);

  if (f == NULL) /* is unit connected?  */
    return 0;    /* for VMS compat, treat as non-error */

  /* check for outstanding async i/o */

  if (f->asy_rw) { /* stop any async i/o */
    f->asy_rw = 0;
    if (Fio_asy_disable(f->asyptr) == -1) {
      return (__fortio_error(__io_errno()));
    }
  }

  /* no backspace for direct access files:  */
  if (f->acc == FIO_DIRECT)
    return __fortio_error(FIO_EDIRECT);

  /*  if eof record has just been written, toggle eof_flag and return: */
  if (f->eof_flag) {
    f->eof_flag = FALSE;
    return 0;
  }

  if (f->binary)
    return 0; /* MODE=binary is a nop */

  if (f->byte_swap)
    swap_bytes = 1;
  else if (f->native)
    swap_bytes = 0;

  /* append carriage return (maybe) */

  if (f->nonadvance) {
    f->nonadvance = FALSE;
    FIO_FCB_INVALIDATE_GETC_BUFFER(f, return __io_errno());
#if defined(WINNT)
    if (__fortio_binary_mode(f->__io_fp))
      __io_fputc('\r', f->__io_fp);
#endif
    __io_fputc('\n', f->__io_fp);
    if (__io_ferror(f->__io_fp))
      return __io_errno();
  }

  /* if already at the beginning just return without error */
  /* if (f->nextrec < 2) */
  if (FIO_FCB_FTELL(f) == 0) /* use ftell in case file opened 'append' */
    return 0;

  if (f->form == FIO_UNFORMATTED) { /* CASE 1: unformatted file */
    int reclen;
  /*  variable length record is stored as   length:record:length
      so back up over trailing length field, read the size, then
      back up over the record and both length fields  */

    FIO_FCB_INVALIDATE_GETC_BUFFER(f, return __fortio_error(__io_errno()));
    fp = f->__io_fp;

  rec_continued:
    if (__io_fseek(fp, -((seekoffx_t)RCWSZ), SEEK_CUR) != 0)
      return __fortio_error(__io_errno());

    if (__io_fread(&reclen, RCWSZ, 1, fp) != 1)
      return __fortio_error(__io_errno());

    /*  NOTE: reclen in FCB and in file is always in units of bytes */
    if (swap_bytes)
      __fortio_swap_bytes((char *)&reclen, __INT, 1);
    if (__io_fseek(fp, -((reclen & 0x7fffffff) + (seekoffx_t)(2 * RCWSZ)),
                    SEEK_CUR) != 0)
      return __fortio_error(__io_errno());
    if (reclen & 0x80000000)
      goto rec_continued;
    f->coherent = 0; /* avoid unnecessary seek later on */
  } else {           /* CASE 2: formatted file */
    int ch;
    seekoffx_t pos;
    assert(f->form == FIO_FORMATTED);
    pos = FIO_FCB_FTELL(f) - 1L;
    assert(pos >= 0);
    while (TRUE) {
      if (pos > 0)
        --pos;
      FIO_FCB_FSEEK_SET(f, pos, return __fortio_error(__io_errno()));

      if (pos == 0)
        ch = 0;
      else
        FIO_FCB_BUFFERED_GETC(ch, f, return __fortio_error(__io_errno()));
      if (pos == 0 || ch == '\n') {
        /* must set coherent flag to 'read' in case the next operation
           on this file is a write: */
        f->coherent = 2 /*read*/;
        break;
      }
    }
  }

  --f->nextrec;
  f->truncflag = TRUE; /* if next operation is write, truncate */
  return 0;            /* no error occurred */
}

__INT_T
ENTF90IO(BACKSPACE, backspace)(unit, bitv, iostat) __INT_T *unit;
__INT_T *bitv;
__INT_T *iostat;
{
  int s = 0;

  __fort_status_init(bitv, iostat);
  if (LOCAL_MODE || GET_DIST_LCPU == GET_DIST_IOPROC)
    s = _f90io_backspace(unit, bitv, iostat, 0);
  __fortio_errend03();
  return DIST_STATUS_BCST(s);
}

__INT_T
ENTCRF90IO(BACKSPACE, backspace)(unit, bitv, iostat) __INT_T *unit;
__INT_T *bitv;
__INT_T *iostat;
{
  int s = 0;
  s = _f90io_backspace(unit, bitv, iostat, 0);
  __fortio_errend03();
  return s;
}

__INT_T
ENTF90IO(SWBACKSPACE, swbackspace)(unit, bitv, iostat) __INT_T *unit;
__INT_T *bitv;
__INT_T *iostat;
{
  int s = 0;

  __fort_status_init(bitv, iostat);
  if (LOCAL_MODE || GET_DIST_LCPU == GET_DIST_IOPROC)
    s = _f90io_backspace(unit, bitv, iostat, 1);
  __fortio_errend03();
  return DIST_STATUS_BCST(s);
}

__INT_T
ENTCRF90IO(SWBACKSPACE, swbackspace)(unit, bitv, iostat) __INT_T *unit;
__INT_T *bitv;
__INT_T *iostat;
{
  int s = _f90io_backspace(unit, bitv, iostat, 1);
  __fortio_errend03();
  return s;
}
