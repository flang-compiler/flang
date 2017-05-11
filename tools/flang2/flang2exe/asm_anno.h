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

/** \file
 * \brief Interface to annotation module
 *
 * Define interface to Annotation module: which supports assembly file
 * annotation (-Manno switch).
 */

/* Annotation is supported by the following 3 functions - generally
   called from the Scheduler/Code Generator module,
   iff "flg.anno" is true:  */

extern void annomod_init(void);
/*	should be called at the beginning of processing for each user
    function (but not before "merge_blocks()" is called). */

extern void annomod_asm(int blkno);
/*	should be called before emitting assembly code for each basic block */

extern void annomod_end(void);
/*	should be called after all code is emitted for the current user
    function, but before "assem_end_func()" is called. */
