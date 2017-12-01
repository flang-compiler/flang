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
 *  \brief OpenMP utility routines for LLVM compilers
 */

#ifndef __LLMPUTIL_H__
#define __LLMPUTIL_H__

/** Uplevel data structure containing a list of shared variables for the region
 * nest that this uplevel belongs to.  The shared variables in this structure
 * are represented as a list of unique sptrs.
 */
typedef struct {
  int *vals;      /* Array containing shared var sptrs */
  int vals_size;  /* Total allocated slots in vals */
  int vals_count; /* Traditionally "available" or vals_avl */
  int dtype;      /* The true dtype containing fields and their offsets */
  /* TODO: Consider using a hashset to speed-unique lookup */
} LLUplevel;

/** First private variable:
 * This information is necessary to generate code that allows a particular task
 * to know which variables are stored in its allocated memory (task memory).
 * First privates for the task have to be stored in that memory.
 */
typedef struct _llprivate_t {
  int shared_sptr;  /**< Represents the caller's copy */
  int private_sptr; /**< Represents the callee's copy (local) */
} LLFirstPrivate;

/** Task data structure containing a list of private variables
 * for the task.
 */
typedef struct {
  int scope_sptr; /**< Outlined task's scope sptr (BMPSCOPE ST_BLOCK) */
  int task_sptr;  /**< Outlined function representing the task */
  LLFirstPrivate *privs; /**< Array of private sptrs for this task */
  int privs_count;
  int privs_size;
  int actual_size; /**< Bytes in task: First priv size + base task size */
} LLTask;

/* Create an LLUplevel instance
 * stblock_sptr: Block where this region nest begins.
 *               This is used as a key into the global list of all uplevels.
 */
extern LLUplevel *llmp_create_uplevel(int stblock_sptr);

/* Obtain a previously created uplevel */
extern LLUplevel *llmp_get_uplevel(int stblock_sptr);

/* Set the dtype (actual struct of member pointers) */
extern void llmp_uplevel_set_dtype(LLUplevel *up, int dtype);

extern void llmp_reset_uplevel(void);

/* Uniquely add a shared variable */
extern int llmp_add_shared_var(LLUplevel *up, int shared_sptr);

/* Retrieve an LLUplevel instance by key */
extern LLUplevel *llmp_create_uplevel_bykey(int key);

/* Return a new key (index) into our table of all uplevels */
extern int llmp_get_next_key(void);

/* Create a task object that can be searched for later using 'scope_sptr' */
extern LLTask *llmp_create_task(int scope_sptr);

/* Obtain a previously created task object, where scope_sptr is the BMPSCOPE
 * scope sptr containing the task.
 */
extern LLTask *llmp_get_task(int scope_sptr);

/* Return the task base size without any private values being stored. */
extern int llmp_task_get_base_task_size(void);

/* Return the task's total size including task metadata and priv vars */
extern int llmp_task_get_size(LLTask *task);

/* Set the task function sptr */
extern void llmp_task_set_fnsptr(LLTask *task, int task_sptr);

/* Return a task a object associated to 'task_sptr' */
extern LLTask *llmp_task_get_by_fnsptr(int task_sptr);

/* Add a private sptr to the task object.
 * priv:   sptr to the private copy of the private variable.
 *         ADDRESSP is called to set the offset to the kmpc task
 *         object where this private data will live during program
 *         execution.
 */
extern int llmp_task_add_private(LLTask *task, int shared, int priv);

/* Create a task object if it does not already exist for 'scope_sptr'.
 * Add a private sptr to the task object.
 * shared, priv: See llmp_task_add_private
 */
extern void llmp_task_add(int scope_sptr, int shared, int priv);

/* Returns the sptr of the 'private' (local to the callee) copy of the
 * private variable represented by 'sptr'.
 */
extern int llmp_task_get_private(const LLTask *task, int sptr, int incl);

void llmp_concur_add_shared_var(int stblock_sptr, int shared_sptr);
void llmp_add_shared_var_charlen(LLUplevel *up, int shared_sptr);

extern int llmp_task_add_loopvar(LLTask*, int, int);
extern LLTask* llGetTask(int);
extern int llTaskAllocSptr();
extern INT llmp_task_get_privoff(int, const LLTask *);
extern LOGICAL is_llvm_local_private(int);

#endif /* __LLMPUTIL_H__ */
