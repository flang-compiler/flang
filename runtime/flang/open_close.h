/*
 * Copyright (c) 2016-2018, NVIDIA CORPORATION.  All rights reserved.
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
 * \brief
 * Declaration of open.c and close.c functions visible to other runtime library
 * routines.
 */

#include "fortDt.h" /* for __INT8_T */

/* From open.c */
/** \brief Low level open routine; called from within fiolib to open a file.
 */
int __fortio_open(int unit, int action_flag, int status_flag, int dispose_flag,
                  int acc_flag, int blank_flag, int form_flag, int delim_flag,
                  int pos_flag, int pad_flag, __INT8_T reclen, char *name,
                  __CLEN_T namelen);

/* From close.c */
/** \brief Low level close routine;  called from within fiolib to close a file.
 */

/* FIXME: when FIO_FCB removed from global.h replace this */
struct fcb;
int __fortio_close(struct fcb *, int);
/*int __fortio_close(FIO_FCB *, int);*/

/** \brief Runtime library IO cleanup routine; do whatever in neccessary to
 * clean up open files */
void __fortio_cleanup(void);

