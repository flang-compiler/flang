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

#ifndef EXPSMP_H_
#define EXPSMP_H_

#include "gbldefs.h"
#include "error.h"
#include "global.h"
#include "symtab.h"
#include "expand.h"
#include "llmputil.h"

/**
   \brief ...
 */
int add_mp_barrier2(void);

/**
   \brief ...
 */
int add_mp_lcpu(void);

/**
   \brief ...
 */
int add_mp_ncpus3(void);

/**
   \brief ...
 */
int add_mp_ncpus(void);

/**
   \brief ...
 */
int add_mp_penter(int ispar);

/**
   \brief ...
 */
int add_mp_pexit(void);

/// Insert semaphore wait (enter critical section)
int add_mp_p(SPTR semaphore);

/// Insert semaphore signal (end critical section)
int add_mp_v(SPTR semaphore);

/**
   \brief ...
 */
int get_threadprivate_origsize(int sym);

/**
   \brief ...
 */
SPTR lcpu_temp(SC_KIND sc);

/**
   \brief ...
 */
SPTR llTaskAllocSptr(void);

/**
   \brief ...
 */
int _make_mp_get_threadprivate(int data_ili, int size_ili, int cache_ili);

/**
   \brief ...
 */
SPTR ncpus_temp(SC_KIND sc);

/**
   \brief ...
 */
LLTask *llGetTask(int scope);

/**
   \brief ...
 */
void clear_tplnk(void);

/**
   \brief ...
 */
void exp_mp_func_prologue(void);

/**
   \brief ...
 */
void exp_smp_fini(void);

/**
   \brief ...
 */
void exp_smp(ILM_OP opc, ILM *ilmp, int curilm);

/**
   \brief ...
 */
void exp_smp_init(void);

/**
   \brief ...
 */
void section_create_endblock(SPTR endLabel);

/// \brief ...
LLTask* llGetTask(int scope);

#endif // EXPSMP_H_
