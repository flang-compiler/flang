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

/** \file transfrm.h
    \brief macros, definitons, and prototypes for Fortran transformation module
*/

#ifndef FE_TRANSFRM_H
#define FE_TRANSFRM_H

typedef struct tlist {
  struct tlist *next;
  int item;
  int flag;
} TLIST;

int get_init_idx(int i, int dtype);
void rewrite_deallocate(int ast, int std);
void trans_process_align(void);
void transform(void);

#endif /* FE_TRANSFRM_H */
