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

#ifndef EXP_FTN_H_
#define EXP_FTN_H_

#include "gbldefs.h"
#include "error.h"
#include "global.h"
#include "symtab.h"
#include "expand.h"

/**
   \brief ...
 */
int create_array_ref(int nmex, SPTR sptr, DTYPE dtype, int nsubs, int *subs,
                     int ilix, int sdscilix, int inline_flag, int *pnme);

/**
   \brief ...
 */
int exp_get_sdsc_len(int s, int base, int basenm);

/**
   \brief ...
 */
int get_sdsc_element(SPTR sdsc, int indx, int membase, int membase_nme);

/**
   \brief ...
 */
SPTR frte_func(SPTR (*pf)(const char *), const char *root);

/**
   \brief ...
 */
void exp_ac(ILM_OP opc, ILM *ilmp, int curilm);

/**
   \brief ...
 */
void exp_array(ILM_OP opc, ILM *ilmp, int curilm);

/**
   \brief ...
 */
void exp_bran(ILM_OP opc, ILM *ilmp, int curilm);

/**
   \brief ...
 */
void exp_misc(ILM_OP opc, ILM *ilmp, int curilm);

/**
   \brief ...
 */
void exp_restore_mxcsr(void);

#endif // EXP_FTN_H_
