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

#ifndef LL_ABI_H_
#define LL_ABI_H_

#include "gbldefs.h"
#include "global.h"
#include "symtab.h"
#include "llutil.h"
#include "ll_structure.h"

/**
   \brief ...
 */
unsigned ll_abi_classify_va_arg_dtype(DTYPE dtype, unsigned *num_gp,
                                      unsigned *num_fp);

/**
   \brief ...
 */
void ll_abi_classify_arg_dtype(LL_ABI_Info *abi, LL_ABI_ArgInfo *arg,
                               DTYPE dtype);

/**
   \brief ...
 */
void ll_abi_classify_return_dtype(LL_ABI_Info *abi, DTYPE dtype);

/**
   \brief ...
 */
void ll_abi_compute_call_conv(LL_ABI_Info *abi, int func_sptr, int jsra_flags);

#endif
