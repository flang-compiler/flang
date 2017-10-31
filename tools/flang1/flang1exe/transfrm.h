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

void transform(void);
void reset_init_idx(void);
int get_init_idx(int i, int dtype);
int get_finfo(int forall, int a);
LOGICAL vector_member(int memast);
int normalize_forall(int forall_ast, int asgn_ast, int inlist);
int make_forall(int shape, int astmem, int mask_ast, int lc);
void init_tbl(void);
void free_tbl(void);
int get_tbl(void);
void trans_process_align(void);
LOGICAL is_bad_dtype(int dtype);
LOGICAL is_array_type(int sptr);
int mk_conformable_test(int dest, int src, int optype);
int mk_allocate(int ast);
int mk_deallocate(int ast);
void rewrite_deallocate(int ast, int std);
void gen_dealloc_if_allocated(int ast, int std);

#endif /* FE_TRANSFRM_H */
