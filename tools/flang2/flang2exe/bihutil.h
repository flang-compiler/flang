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

#ifndef BIHUTIL_H_
#define BIHUTIL_H_

#include "gbldefs.h"
#include <stdio.h>

/**
   \brief ...
 */
bool any_asm(void);

/**
   \brief ...
 */
int addbih(int after);

/**
   \brief ...
 */
int addnewbih(int after, int flags, int fih);

/**
   \brief ...
 */
int exp_addbih(int after);

/**
   \brief ...
 */
int merge_bih(int curbih);

/**
   \brief ...
 */
void bih_cleanup(void);

/**
   \brief ...
 */
void bih_init(void);

/**
   \brief ...
 */
void *ccff_bih_info(int msgtype, char *msgid, int bihx, const char *message,
                    ...);

/**
   \brief ...
 */
void delbih(int bihx);

/**
   \brief ...
 */
void dump_blocks(FILE *ff, int bih, char *fmt, int fihflag);

/**
   \brief ...
 */
void dump_one_block(FILE *ff, int bih, char *fmt);

/**
   \brief ...
 */
void merge_bih_flags(int to_bih, int fm_bih);

/**
   \brief ...
 */
void merge_blks(int b1, int b2);

/**
   \brief ...
 */
void merge_rgset(int tobih, int frombih, bool reuse_to);

/**
   \brief ...
 */
void split_extended(void);

/**
   \brief ...
 */
void *subccff_bih_info(void *xparent, int msgtype, char *msgid, int bihx,
                       const char *message, ...);

/**
   \brief ...
 */
void unsplit(void);

/**
   \brief ...
*/
bool contains_par_blocks(void);

#endif
