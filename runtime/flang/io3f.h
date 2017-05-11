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

/*   io3f.h - interface to the I/O support of f90 and f77 */

#include "global.h"

/*
 * 3f code uses f77 functions/globals - just define macros to access
 * the f90 equivalents.
 */
#define __fio_close(f, s) __fortio_close(f, s)
#define __fio_find_unit(u) __fortio_find_unit(u)
#define fio_fileno(f) __fort_getfd(f)
#ifndef WINNT
#define pgi_fio fioFcbTbl
#endif

#if defined(WINNT)
#define __PC_DOS 1
#else
#define __PC_DOS 0
#endif

#define FIO_FCB_ASYPTR(f) __fortio_fiofcb_asyptr(f)
#define FIO_FCB_ASY_RW(f) __fortio_fiofcb_asy_rw(f)
#define FIO_FCB_SET_ASY_RW(f, a) __fortio_set_asy_rw(f, a)
#define FIO_FCB_STDUNIT(f) __fortio_fiofcb_stdunit(f)
#define FIO_FCB_FP(f) __fortio_fiofcb_fp(f)
#define FIO_FCB_FORM(f) __fortio_fiofcb_form(f)
#define FIO_FCB_NAME(f) __fortio_fiofcb_name(f)
#define FIO_FCB_NEXT(f) __fortio_fiofcb_next(f)
