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

#ifndef LLASSEM_COMMON_H_
#define LLASSEM_COMMON_H_

#include "gbldefs.h"
#include "global.h"
#include "symtab.h"
#include "ll_structure.h"

/**
   \brief ...
 */
ISZ_T put_skip(ISZ_T old, ISZ_T New);

/**
   \brief ...
 */
char *put_next_member(char *ptr);

/**
   \brief ...
 */
int add_member_for_llvm(SPTR sym, int prev, DTYPE dtype, ISZ_T size);

/**
   \brief ...
 */
DTYPE mk_struct_for_llvm_init(const char *name, int size);

/**
   \brief ...
 */
LL_Value *gen_ptr_offset_val(int offset, LL_Type *ret_type, char *ptr_nm);

/**
   \brief ...
 */
void add_init_routine(char *initroutine);

/**
   \brief ...
 */
void emit_init(DTYPE tdtype, ISZ_T tconval, ISZ_T *addr, ISZ_T *repeat_cnt,
               ISZ_T loc_base, ISZ_T *i8cnt, int *ptrcnt, char **cptr);

/**
   \brief ...
 */
void init_daz(void);

/**
   \brief ...
 */
void init_flushz(void);

/**
   \brief ...
 */
void init_huge_tlb(void);

/**
   \brief ...
 */
void init_ktrap(void);

/**
   \brief ...
 */
void init_Mcuda_compiled(void);

/**
   \brief ...
 */
void put_addr(SPTR sptr, ISZ_T off, DTYPE dtype);

/**
   \brief ...
 */
void put_i32(int val);

/**
   \brief ...
 */
void put_int4(int val);

/**
   \brief ...
 */
void put_short(int val);

/**
   \brief ...
 */
void put_string_n(char *p, ISZ_T len, int size);

#endif // LLASSEM_COMMON_H_
