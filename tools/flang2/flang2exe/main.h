/*
 * Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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

/**
   \file
   \brief main exports
 */

#ifndef FORTRAN_COMPILER_MAIN_H_
#define FORTRAN_COMPILER_MAIN_H_

#include "gbldefs.h"
#include "error.h"
#include "lz.h"

// FIXME -- move these prototypes
void schedule(void); // cgmain
void acc_add_global(void); // acclin
// FIXME -- end of misplaced prototypes

/* actual exports from module */

/**
   \brief ...
 */
char *user_string(void);

/**
   \brief ...
 */
int bu_auto_inline(void);

/**
   \brief ...
 */
int export_cgraph_sub(lzhandle *fd);

/**
   \brief ...
 */
int main(int argc, char *argv[]);

/**
   \brief FIXME Comments say this belongs in upper.c
 */
void add_llvm_uplevel_symbol(int sptr);

/**
   \brief ...
 */
void finish(void);

/**
   \brief FIXME Comments say this belongs in upper.c
 */
void fixup_llvm_uplevel_symbol(void);

#endif // FORTRAN_COMPILER_MAIN_H_
