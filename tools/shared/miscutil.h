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

#ifndef MISCUTIL_H_
#define MISCUTIL_H_

#include "gbldefs.h"
#include "sharedefs.h"
#include <stdio.h>

/**
   \brief ...
 */
bool is_xflag_bit(int indx);

/**
   \brief ...
 */
char *literal_string(char *oldstr, int userlen, bool isStringW);

/**
   \brief ...
 */
char *mkfname(char *oldname, char *oldsuf, char *newsuf);

/**
   \brief ...
 */
int license_prc2(void);

/**
   \brief ...
 */
int license_prc(void);

/**
   \brief ...
 */
int stg_next_freelist(STG *stg);

/**
   \brief ...
 */
int stg_next(STG *stg, int n);

/**
   \brief ...
 */
void fprintf_str_esc_backslash(FILE *f, char *str);

/**
   \brief ...
 */
void set_xflag(int indx, INT val);

/**
   \brief ...
 */
void set_yflag(int indx, INT val);

/**
   \brief ...
 */
void stg_add_freelist(STG *stg, int r);

/**
   \brief ...
 */
void stg_alloc_sidecar(STG *basestg, STG *stg, int dtsize, char *name);

/**
   \brief ...
 */
void stg_alloc(STG *stg, int dtsize, int size, char *name);

/**
   \brief ...
 */
void stg_clear_all(STG *stg);

/**
   \brief ...
 */
void stg_clear_force(STG *stg, int r, int n, bool force);

/**
   \brief ...
 */
void stg_clear(STG *stg, int r, int n);

/**
   \brief ...
 */
void stg_delete_sidecar(STG *basestg, STG *stg);

/**
   \brief ...
 */
void stg_delete(STG *stg);

/**
   \brief ...
 */
void stg_need(STG *stg);

/**
   \brief ...
 */
void stg_set_freelink(STG *stg, int offset);

#endif // MISCUTIL_H_
