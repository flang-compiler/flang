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

#ifndef F_EXP_RTE_H_
#define F_EXP_RTE_H_

#include "gbldefs.h"
#include "global.h"
#include "symtab.h"
#include "expand.h"

/**
   \brief ...
 */
bool bindC_function_return_struct_in_registers(int func_sym);

/**
   \brief ...
 */
int charaddr(SPTR sym);

/**
   \brief ...
 */
int charlen(SPTR sym);

/**
   \brief ...
 */
int exp_alloca(ILM *ilmp);

/**
   \brief ...
 */
int gen_arg_ili(void);

/**
   \brief ...
 */
SPTR getdumlen(void);

/// Create a symbol representing the length of a passed-length character
/// argument in the host subprogram.
SPTR gethost_dumlen(int arg, ISZ_T address);

/**
   \brief ...
 */
int is_passbyval_dummy(int sptr);

/**
   \brief ...
 */
int needlen(int sym, int func);

/**
   \brief ...
 */
void add_arg_ili(int ilix, int nme, int dtype);

/**
   \brief ...
 */
void chk_terminal_func(int entbih, int exitbih);

/**
   \brief ...
 */
void end_arg_ili(void);

/**
   \brief ...
 */
void exp_agoto(ILM *ilmp, int curilm);

/**
   \brief ...
 */
void expand_smove(int destilm, int srcilm, DTYPE dtype);

/**
   \brief ...
 */
void exp_build_agoto(int *tab, int mx);

/**
   \brief ...
 */
void exp_call(ILM_OP opc, ILM *ilmp, int curilm);

/**
   \brief ...
 */
void exp_cgoto(ILM *ilmp, int curilm);

/**
   \brief ...
 */
void exp_end(ILM *ilmp, int curilm, bool is_func);

/**
   \brief ...
 */
void exp_fstring(ILM_OP opc, ILM *ilmp, int curilm);

/**
   \brief ...
 */
void exp_header(SPTR sym);

/**
   \brief ...
 */
void exp_qjsr(char *ext, DTYPE res_dtype, ILM *ilmp, int curilm);

/**
   \brief ...
 */
void exp_remove_gsmove(void);

/**
   \brief ...
 */
void exp_reset_argregs(int ir, int fr);

/**
   \brief ...
 */
void exp_szero(ILM *ilmp, int curilm, int to, int from, int dtype);

/**
   \brief ...
 */
void exp_zqjsr(char *ext, int res_dtype, ILM *ilmp, int curilm);

/**
   \brief ...
 */
void init_arg_ili(int n);

/** \brief Checks to see if a procedure has character dummy arguments.
 *     
 *  \param func is the procedure we are checking.
 * 
 *  \return true if the procedure has character dummy arguments, else false.
 */
bool func_has_char_args(SPTR func);

/// \brief Process referenced symbols, assigning them locations
void AssignAddresses(void);
#endif
