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

/* defs.c -- definitions of globals that were originally in files that
 *           had to be split into file.c and file_i8.c versions.
 */

#include "stdioInterf.h"
#include "fioMacros.h"

#include "fort_vars.h"

/* dist.c */
__INT_T f90DummyGenBlock = 0;
__INT_T *f90DummyGenBlockPtr = &f90DummyGenBlock;

/* dbug.c */
/*
 * explicitly initialize for OSX since the object of this file will not be
 * linked in from the library.  The link will not see the .comm for
 * __fort_test (i.e., its definition!).
 */
__fort_vars_t   __fort_vars __attribute__((aligned(128))) = {
    .debug      = 0,
    .zmem       = 0,
    .debugn     = 0,
    .ioproc     = 0,
    .lcpu       = 0,
    .np2        = 0,
    .pario      = 0,
    .quiet      = 0,
    .tcpus      = 0,
    .test       = 0,
    .heapz      = 0,
    .heap_block = 0,
    .tids       = NULL,
    .red_what   = NULL,
};
