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

#ifndef LL_FTN_H_
#define LL_FTN_H_

#include "gbldefs.h"
#include "ll_structure.h"

/**
   \brief ...
 */
ISZ_T get_socptr_offset(int sptr);

/**
   \brief ...
 */
bool has_multiple_entries(int sptr);

/**
   \brief ...
 */
bool is_fastcall(int ilix);

/**
   \brief ...
 */
char *get_entret_arg_name(void);

/**
   \brief ...
 */
char *get_llvm_ifacenm(int sptr);

/**
   \brief ...
 */
char *get_local_overlap_var(void);

/**
   \brief ...
 */
int get_entries_argnum(void);

/**
   \brief ...
 */
int get_iface_sptr(int sptr);

/**
   \brief ...
 */
int get_master_sptr(void);

/**
   \brief ...
 */
int get_return_type(int func_sptr);

/**
   \brief ...
 */
int is_iso_cptr(int d_dtype);

/**
   \brief ...
 */
int mk_charlen_address(int sptr);

/**
   \brief ...
 */
LL_Type *get_ftn_lltype(int sptr);

/**
   \brief ...
 */
LL_Type *get_local_overlap_vartype(void);

/**
   \brief ...
 */
void assign_array_lltype(int dtype, int size, int sptr);

/**
   \brief ...
 */
void fix_llvm_fptriface(void);

/**
   \brief ...
 */
void get_local_overlap_size(void);

/**
   \brief ...
 */
void ll_process_routine_parameters(int func_sptr);

/**
   \brief ...
 */
void print_entry_subroutine(LL_Module *module);

/**
   \brief ...
 */
void reset_equiv_var(void);

/**
   \brief ...
 */
void reset_master_sptr(void);

/**
   \brief ...
 */
void stb_process_routine_parameters(void);

/**
   \brief ...
 */
void store_llvm_localfptr(void);

/**
   \brief ...
 */
void write_llvm_lltype(int sptr);

/**
   \brief ...
 */
void write_local_overlap(void);

/**
   \brief ...
 */
void write_master_entry_routine(void);

#endif // LL_FTN_H_
