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

/* llmputil.c: OpenMP utility routines for our LLVM compilers */

#include "gbldefs.h"
#include "global.h"
#include "error.h"
#include "llmputil.h"
#include "symtab.h"

/* Global container of uplevel pointers */
static struct {
  LLUplevel *base; /* Pointer to the allocated array of items */
  int size;        /* Total size including unused items */
  int avl;         /* Total items in use */
} llmp_all_uplevels;

/* Global container of task pointers */
static struct {
  LLTask *base; /* Pointer to the allocated array of items */
  int size;     /* Total size including unused items */
  int avl;      /* Total items in use */
} llmp_all_tasks;

static LLUplevel *
get_uplevel(int stblock_sptr)
{
  int key;
  LLUplevel *up;
  assert(STYPEG(stblock_sptr) == ST_BLOCK, "Uplevel key must be an ST_BLOCK",
         stblock_sptr, 4);

  /* Index */
  key = PARSYMSG(stblock_sptr);

  /* Locate uplevel pointer */
  up = NULL;
  if (key <= llmp_all_uplevels.avl)
    up = (LLUplevel *)(&llmp_all_uplevels.base[key]);

  assert(up && key, "Could not locate uplevel instance for stblock",
         stblock_sptr, 4);

  return up;
}

LLUplevel *
llmp_create_uplevel(int stblock_sptr)
{
  int key;
  LLUplevel *up;

  assert(STYPEG(stblock_sptr) == ST_BLOCK, "Uplevel key must be an ST_BLOCK",
         stblock_sptr, 4);

  /* Avoid processing an already created uplevel */
  if (PARSYMSG(stblock_sptr))
    return get_uplevel(stblock_sptr);

  /* Make room if necessary */
  if (llmp_all_uplevels.avl == 0) {
    llmp_all_uplevels.avl = 2;
    key = 1;
  } else {
    key = llmp_all_uplevels.avl;
    ++llmp_all_uplevels.avl;
  }
  NEED(llmp_all_uplevels.avl, llmp_all_uplevels.base, LLUplevel,
       llmp_all_uplevels.size, llmp_all_uplevels.size + 8);

  up = (LLUplevel *)(&llmp_all_uplevels.base[key]);
  memset(up, 0, sizeof(LLUplevel));

  /* Add key and map it to stblock */
  PARSYMSP(stblock_sptr, key);

  return up;
}

LLUplevel *
llmp_get_uplevel(int stblock_sptr)
{
  return get_uplevel(stblock_sptr);
}

void
llmp_uplevel_set_dtype(LLUplevel *up, int dtype)
{
  up->dtype = dtype;
}

/* Uniquely add shared_sptr to up */
int
llmp_add_shared_var(LLUplevel *up, int shared_sptr)
{
  int i;
  const int idx = up->vals_count;

  /* Unique add: I really wanted to make this a hashset... */
  for (i = 0; i < up->vals_count; ++i){
    if (shared_sptr == 0)
      break;
    if (up->vals[i] == shared_sptr)
      return 0;
  }

  ++up->vals_count;
  NEED(up->vals_count, up->vals, int, up->vals_size, up->vals_size + 8);
  up->vals[idx] = shared_sptr;
  return 1;
}

/* add 0 as placeholder for character len sptr for shared_sptr */
void
llmp_add_shared_var_charlen(LLUplevel *up, int shared_sptr)
{
  int i;
  const int idx = up->vals_count;

  /* Unique add: I really wanted to make this a hashset... */
  for (i = 0; i < up->vals_count; ++i)
    if (up->vals[i] == shared_sptr) {
      ++up->vals_count;
      NEED(up->vals_count, up->vals, int, up->vals_size, up->vals_size + 8);
      up->vals[idx] = 0;
    }
}

/* Return a new key (index) into our table of all uplevels */
int
llmp_get_next_key(void)
{
  int key;
  if (llmp_all_uplevels.avl == 0) {
    llmp_all_uplevels.avl = 2;
    key = 1;
  } else {
    key = llmp_all_uplevels.avl;
    ++llmp_all_uplevels.avl;
  }
  NEED(llmp_all_uplevels.avl, llmp_all_uplevels.base, LLUplevel,
       llmp_all_uplevels.size, llmp_all_uplevels.size + 8);
  return key;
}

/* Return the uplevel for a specific key (index into our table of uplevels) */
LLUplevel *
llmp_create_uplevel_bykey(int key)
{
  LLUplevel *up;

  assert(key <= llmp_all_uplevels.avl, "Invalid uplevel key", key, 4);

  up = (LLUplevel *)(&llmp_all_uplevels.base[key]);
  memset(up, 0, sizeof(LLUplevel));

  return up;
}

void
llmp_reset_uplevel(void)
{
  int i, j;
  LLUplevel *up;
  if (llmp_all_uplevels.avl) {
    for (i = 1; i < llmp_all_uplevels.avl; ++i) {
      up = (LLUplevel *)(&llmp_all_uplevels.base[i]);
      if (up->vals_count)
        FREE(up->vals);
    }
    FREE(llmp_all_uplevels.base);
    memset(&llmp_all_uplevels, 0, sizeof(llmp_all_uplevels));
  }
}

LLTask *
llmp_get_task(int scope_sptr)
{
  int i;
  for (i = 0; i < llmp_all_tasks.avl; ++i) {
    LLTask *task = (LLTask *)&llmp_all_tasks.base[i];
    if (task->scope_sptr == scope_sptr)
      return task;
  }
  return NULL;
}

LLTask *
llmp_create_task(int scope_sptr)
{
  int key;
  LLTask *task;

  NEED(llmp_all_tasks.avl + 1, llmp_all_tasks.base, LLTask, llmp_all_tasks.size,
       llmp_all_tasks.size + 4);

  task = (LLTask *)(&llmp_all_tasks.base[llmp_all_tasks.avl]);
  ++llmp_all_tasks.avl;
  memset(task, 0, sizeof(LLTask));
  task->actual_size = llmp_task_get_base_task_size();
  task->scope_sptr = scope_sptr;
  return task;
}

/* Return the size of an empty KMPC task (no shared variables):
 * Pointer + Pointer + int32: see kmp.h
 */
int
llmp_task_get_base_task_size(void)
{
#ifdef TARGET_WIN
  return sizeof(void *) + sizeof(void *) + sizeof(int);
#else
  return sizeof(void *) + sizeof(void *) + sizeof(int32_t);
#endif
}

/* Return the size of a KMPC equivalent task (base + size of first privates) */
int
llmp_task_get_size(LLTask *task)
{
  return task->actual_size;
}

/* Set the fnsptr that belongs to the outlined task */
void
llmp_task_set_fnsptr(LLTask *task, int task_sptr)
{
  task->task_sptr = task_sptr;
}

/* Return the task object associated with 'task_sptr' */
LLTask *
llmp_task_get_by_fnsptr(int task_sptr)
{
  int i;
  LLTask *task;

  for (i = 0; i < llmp_all_tasks.avl; ++i) {
    LLTask *task = (LLTask *)&llmp_all_tasks.base[i];
    if (task->task_sptr == task_sptr)
      return task;
  }

  return NULL;
}

void
llmp_task_add_firstprivate(LLTask *task, int shared_sptr, int private_sptr)
{
  LLFirstPrivate *fp;
  int idx = task->firstprivs_count;

  NEED(++task->firstprivs_count, task->firstprivs, LLFirstPrivate,
       task->firstprivs_size, task->firstprivs_size + 4);

  /* Create the firstprivate object */
  fp = (LLFirstPrivate *)&(task->firstprivs[idx]);
  fp->shared_sptr = shared_sptr;
  fp->private_sptr = private_sptr;

/* Bump up the size of the task to contain private_sptr */
  task->actual_size += size_of_sym(shared_sptr);
}

void
llmp_task_add(int scope_sptr, int shared_sptr, int private_sptr)
{
  LLTask *task;
  assert(scope_sptr && STYPEG(scope_sptr) == ST_BLOCK,
         "Task key must be a scope sptr (ST_BLOCK)", scope_sptr, 4);

  task = llmp_get_task(scope_sptr);
  if (!task)
    task = llmp_create_task(scope_sptr);
  llmp_task_add_firstprivate(task, shared_sptr, private_sptr);
}

int
llmp_task_get_firstprivate(const LLTask *task, int sptr)
{
  int i;

  for (i = 0; i < task->firstprivs_count; ++i) {
    const int sh = task->firstprivs[i].shared_sptr;
    const int pr = task->firstprivs[i].private_sptr;
    if (sptr == sh || sptr == pr)
      return pr;
  }

  return 0;
}

void
llmp_concur_add_shared_var(int stblock_sptr, int shared_sptr)
{
  int dtype;
  LLUplevel *up;

  up = llmp_create_uplevel(stblock_sptr);
  (void)llmp_add_shared_var(up, shared_sptr);
}
