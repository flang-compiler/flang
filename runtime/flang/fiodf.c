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

/* fiodf.h - define global data for Fortran I/O */

#include "global.h"

/* define global variables for fortran I/O (members of struct fioFcbTbls): */

FIO_TBL fioFcbTbls = {0};

#ifdef WINNT
FIO_FCB *
__get_hpfio_fcbs(void)
{
  return fioFcbTbls.fcbs;
}
#endif

/* define array giving sizes in bytes of the different data types: */

short __fortio_type_size[] = {
    1,  /* (byte) */
    2,  /* signed short */
    2,  /* unsigned short */
    4,  /* signed int */
    4,  /* unsigned int */
    4,  /* signed long int */
    4,  /* unsigned long int */
    4,  /* float */
    8,  /* double */
    8,  /* (float complex) */
    16, /* (double complex) */
    1,  /* signed char */
    1,  /* unsigned char */
    16, /* long double */
    1,  /* (string) */
    8,  /* long long */
    8,  /* unsigned long long */
    1,  /* byte logical */
    2,  /* short logical */
    4,  /* logical */
    8,  /* logical*8 */
    4,  /* typeless */
    8,  /* double typeless */
    2,  /* ncharacter - kanji */
};
