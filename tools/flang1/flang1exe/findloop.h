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

#ifndef FINDLOOP_H_
#define FINDLOOP_H_

#include "universal.h"
#include <stdio.h>

/**
   \brief ...
 */
bool contains_loop(int lp1, int lp2);

/**
   \brief ...
 */
bool is_childloop(int lp1, int lp2);

/**
   \brief ...
 */
bool is_dominator(int v, int w);

/**
   \brief ...
 */
bool is_post_dominator(int v, int w);

/**
   \brief ...
 */
bool is_tail_aexe(int lp);

/**
   \brief ...
 */
bool overlapping_loops(int lp1, int lp2);

/**
   \brief ...
 */
void __dump_loop(FILE *ff, int lp);

/**
   \brief ...
 */
void dump_loops(void);

/**
   \brief ...
 */
void __dump_region(FILE *ff, int lp);

/**
   \brief ...
 */
void dump_region(int lp);

/**
   \brief ...
 */
void findloop(int hlopt_bv);

/**
   \brief ...
 */
void findlooptopsort(void);

/**
   \brief ...
 */
void reorderloops(void);

/**
   \brief ...
 */
void sortloops(void);

#endif // FINDLOOP_H_
