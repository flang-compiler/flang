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

/**
   \file
   Functions for dealing with lexical scopes and the lifetimes of
   scoped variables.
 */

#ifndef SCOPE_H_
#define SCOPE_H_

#if DEBUG + 0

#define ICHECK(x)                                \
  assert((x), "CHECK(" #x "): false at "__FILE__ \
              ":",                               \
         __LINE__, ERR_Informational)

#else

#define ICHECK(x)

#endif

LOGICAL scope_contains(int outer, int inner);
LOGICAL is_scope_label(int label_sptr);
LOGICAL is_scope_label_ili(int ilix);
LOGICAL is_scope_labels_only_bih(int bihx);
void scope_verify(void);
int insert_begin_scope_label(int block_sptr);
int insert_end_scope_label(int block_sptr);

/*
 * Scope tracking.
 *
 * During inlining, as we're scanning the current function ILMs top to bottom,
 * call track_scope_label(label_sptr) whenever an IM_LABEL is seen in order to
 * keep track of the current scope.
 *
 * The global variable current_scope points to the currently open scope.
 */
extern int current_scope;

void track_scope_reset();
void track_scope_label(int label);
void find_scope_labels(int numilms);

/*
 * Inliner support.
 *
 * The functions and global variables below are used by the C and Fortran
 * inliners.
 */

/*
 * While inlining a callee (see func_sptr below), this is an sptr to a new
 * ST_BLOCK representing the callee function scope.
 */
extern int new_callee_scope;

void create_inlined_scope(int callee_sptr);
void begin_inlined_scope(int callee_sptr);
void end_inlined_scope(void);
void end_inlined_scopes(int new_open_count);
void cancel_inlined_scope(void);
void remove_scope_labels(void);

#endif /* SCOPE_H_ */
