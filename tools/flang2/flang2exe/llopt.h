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

#ifndef LLOPT_H_
#define LLOPT_H_

#include "gbldefs.h"
#include "llutil.h"

/**
   \brief ...
 */
bool block_branches_to(int bih, int target);

/**
   \brief ...
 */
bool funcHasNoDepChk(void);

/**
   \brief ...
 */
void maybe_undo_recip_div(INSTR_LIST *mul);

/**
   \brief ...
 */
void optimize_block(INSTR_LIST *last_block_instr);

/**
   \brief ...
 */
void redundantLdLdElim(void);

/**
   \brief ...
 */
void widenAddressArith(void);

#endif // LLOPT_H_
