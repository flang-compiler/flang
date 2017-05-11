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

#ifndef _ASYNC_H
#define _ASYNC_H

/** \file
 * Externally visiable Fortran asynchronous IO routines (from async.c)
 */

struct asy;

#define FIO_EEOF 217

/** \brief
 * Asynchronous fseek
 */
int Fio_asy_fseek(struct asy *asy, long offset, int whence);

/** \brief
 * Enable asynchronous IO, disable stdio
 */
int Fio_asy_enable(struct asy *asy);

/** \brief
 * Disable asynchronous IO, enable stdio
 */
int Fio_asy_disable(struct asy *asy);

/** \brief
 * Initialize a file for asynchronous IO, called from open
 */
int Fio_asy_open(FILE *fp, struct asy **pasy);

/** \brief 
 *  Start an asynch read
 */
int Fio_asy_read(struct asy *asy, void *adr, long len);

/** \brief
 * Start an asynch write 
 */
int Fio_asy_write(struct asy *asy, void *adr, long len);

/** \\brief
 * For vectored i/o, start reads or writes
 */
int Fio_asy_start(struct asy *asy);

/** \brief
 * close asynch i/o called from close
 */
int Fio_asy_close(struct asy *asy);

#endif /* _ASYNC_H */
