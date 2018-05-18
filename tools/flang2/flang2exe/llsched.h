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

#ifndef LLSCHED_H_
#define LLSCHED_H_

#include "gbldefs.h"
#include "llutil.h"

/**

/**
   \brief ...
 */
int enhanced_conflict(int nme1, int nme2);

/**
   \brief ...
 */
void check_circular_dep(INSTR_LIST *istart);

/**
   \brief ...
 */
void sched_block_breadth_first(INSTR_LIST *istart, int level);

/**
   \brief ...
 */
void sched_block(INSTR_LIST *istart, INSTR_LIST *iend);

/**
   \brief ...
 */
void sched_instructions(INSTR_LIST *istart);

#endif // LLSCHED_H_
