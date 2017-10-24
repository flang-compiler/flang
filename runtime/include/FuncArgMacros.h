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

/**
 * \file
 * \brief pghpfent.h - Fortran RTE name build and  entry symbol macros
 */

/* TODO FOR FLANG: resolve/merge w/ent3f.h??? */

#ifndef _PGHPFENT_H_
#define _PGHPFENT_H_

/* Alternate Fortran entry symbol formats */

#if defined(WIN64)
#if defined(DESC_I8)
#define ENTF90IO(UC, LC) pgf90io_##LC##_i8
#define ENTF90(UC, LC) pgf90_##LC##_i8
#define ENTFTN(UC, LC) pghpf_##LC##_i8
#define ENTRY(UC, LC) LC##_i8
#define ENTCRF90IO(UC, LC) pgcrf90io_##LC##_i8
#define ENTFTNIO(UC, LC) pghpfio_##LC##64
#define ENTCRFTNIO(UC, LC) pgcrhpfio_##LC##_i8
#define F90_MATMUL(s) pg_mm_##s##_i8_
#else
#define ENTF90IO(UC, LC) pgf90io_##LC
#define ENTF90(UC, LC) pgf90_##LC
#define ENTFTN(UC, LC) pghpf_##LC
#define ENTRY(UC, LC) LC
#define ENTCRF90IO(UC, LC) pgcrf90io_##LC
#define ENTFTNIO(UC, LC) pghpfio_##LC
#define ENTCRFTNIO(UC, LC) pgcrhpfio_##LC
#define F90_MATMUL(s) pg_mm_##s##_
#endif
#define ENTCRF90(UC, LC) pgcrf90_##LC
#define ENTCRFTN(UC, LC) pgcrhpf_##LC
#define ENTCOMN(UC, LC) pghpf_##LC##_

#elif defined(WIN32)
#define ENTF90(UC, LC) pgf90_##LC
#define ENTF90IO(UC, LC) pgf90io_##LC
#define ENTFTN(UC, LC) pghpf_##LC
#define ENTFTNIO(UC, LC) pghpfio_##LC
#define ENTRY(UC, LC) LC
#define ENTCRF90IO(UC, LC) pgcrf90io_##LC
#define ENTCRFTNIO(UC, LC) pgcrhpfio_##LC
#define ENTCRF90(UC, LC) pgcrf90_##LC
#define ENTCRFTN(UC, LC) pgcrhpf_##LC
#define ENTCOMN(UC, LC) pghpf_##LC
#define F90_MATMUL(s) pg_mm_##s##_

#elif defined(WINNT)
#define ENTF90(UC, LC) pgf90_##LC
#define ENTF90IO(UC, LC) pgf90io_##LC
#define ENTFTN(UC, LC) pghpf_##LC
#define ENTFTNIO(UC, LC) pghpfio_##LC
#define ENTRY(UC, LC) LC
#define ENTCRF90IO(UC, LC) pgcrf90io_##LC
#define ENTCRFTNIO(UC, LC) pgcrhpfio_##LC
#define ENTCRF90(UC, LC) pgcrf90_##LC
#define ENTCRFTN(UC, LC) pgcrhpf_##LC
#define ENTCOMN(UC, LC) pghpf_win_##LC
#define F90_MATMUL(s) pg_mm_##s##_

#else
#define ENTF90IO(UC, LC) f90io_##LC
#define ENTF90(UC, LC) f90_##LC
#define ENTFTN(UC, LC) fort_##LC
#define ENTRY(UC, LC) LC
#define ENTCRF90IO(UC, LC) crf90io_##LC	/* FIXME: HPF, delete all with this prefix*/
#define ENTFTNIO(UC, LC) ftnio_##LC
#define ENTCRFTNIO(UC, LC) crftnio_##LC	/* FIXME: HPF, delete all with this prefix*/
#define F90_MATMUL(s) f90_mm_##s##_

#define ENTCRF90(UC, LC) crf90_##LC	/* FIXME: HPF, delete all with this prefix*/
#define ENTCRFTN(UC, LC) crftn_##LC	/* FIXME: HPF, delete all with this prefix*/ 
#define ENTCOMN(UC, LC) ftn_##LC##_	/* FIXME: common blocks */

#endif

#if defined(DESC_I8)
#define I8(s) s##_i8
#define I8_(s) s##i8_
#else
#define I8(s) s
#define I8_(s) s
#endif

/* macros to put character length arguments in their place.
   DCHAR declares a character pointer argument.
   DCLEN declares a character length argument. Since DCLEN may have an
   empty definition, no commas should be used before or after a DCLEN
   reference in a dummy argument list.
   CADR gets the character pointer.
   CLEN gets the character length.  */

#define DCHAR(ARG) char *ARG##_adr
#define DCLEN(ARG) , int ARG##_len
#define CADR(ARG) (ARG##_adr)
#define CLEN(ARG) (ARG##_len)

/* #if defined(WIN64) || defined(WIN32) */
#if defined(PGDLL) && defined(_DLL) &&                                         (defined(TARGET_WIN) || defined(WIN64) || defined(WIN32))
#define WIN_EXP __declspec(dllexport)
#define WIN_IMP extern __declspec(dllimport)
#else
#define WIN_EXP
#define WIN_IMP extern
#endif

#define CORMEM ENTCOMN(0L, 0l)
#define LINENO ENTCOMN(LINENO, lineno)

#define LOCAL_MODE 0

/* SUBGROUP_MODE used to indicate communication between a subset of
 * processors ...
 * Used in __fort_global_reduce (reduct.c)
 */

#define SUBGROUP_MODE 0

/* declare a variable private to a thread (taskcommon) */

#define PRIVGLOB(type, var) type var
#define PRIVSTAT(type, var) static type var
#define PRIVXTRN(type, var) extern type var

#endif
