/*
 * Copyright (c) 2015-2018, NVIDIA CORPORATION.  All rights reserved.
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

#ifndef OUTLINER_H_
#define OUTLINER_H_

/** \file
 * \brief Various definitions for the outliner module
 */

#include "gbldefs.h"
#include "global.h"
#include "symtab.h"
#include "ili.h"
#include <stdio.h>

extern FILE *par_file1;
extern FILE *par_file2;
extern FILE *par_curfile;

int ll_has_cuda_constructor(void);
void ll_save_cuda_constructor(void);

/**
   \brief ...
 */
bool ll_ilm_is_rewriting(void);

/**
   \brief ...
 */
char *ll_get_outlined_funcname(int fileno, int lineno);

/**
   \brief ...
 */
int ll_ad_outlined_func2(ILI_OP result_opc, ILI_OP call_opc, int sptr,
                         int nargs, int *args);

/**
   \brief ...
 */
SPTR ll_create_task_sptr(void);

/**
   \brief ...
 */
int ll_get_gtid_addr_ili(void);

/**
   \brief ...
 */
int ll_get_gtid_val_ili(void);

/**
   \brief ...
 */
SPTR ll_get_gtid(void);

/**
   \brief ...
 */
SPTR ll_get_hostprog_arg(int func_sptr, int whicharg);

/**
   \brief ...
 */
SPTR ll_get_shared_arg(SPTR func_sptr);

/**
   \brief ...
 */
int ll_get_uplevel_arg(void);

/**
   \brief ...
 */
int ll_get_uplevel_offset(int sptr);

/**
   \brief ...
 */
SPTR ll_get_uplevel_sym(void);

/**
   \brief ...
 */
int ll_has_more_outlined(void);

/**
   \brief ...
 */
int ll_load_outlined_args(int scope_blk_sptr, SPTR callee_sptr, bool clone);

/**
   \brief ...
 */
int ll_make_outlined_call2(int func_sptr, int uplevel_ili);

/**
   \brief ...
 */
int ll_make_outlined_call(int func_sptr, int arg1, int arg2, int arg3);

/**
   \brief Create function and parameter list for an outlined function
   \param stblk_sptr  references the arguments for the function to be outlined
   \param scope_sptr  references the scope
 */
SPTR ll_make_outlined_func(SPTR stblk_sptr, SPTR scope_sptr);

/**
   \brief ...
 */
int ll_make_outlined_garg(int nargs, int *argili, DTYPE *arg_dtypes);

/**
   \brief ...
 */
int ll_make_outlined_gjsr(int func_sptr, int nargs, int arg1, int arg2, int arg3);

/**
   \brief ...
 */
int ll_make_outlined_task_call(int func_sptr, SPTR task_sptr);

/**
   \brief Create function and parameter list for an outlined task
   \param stblk_sptr  references the arguments for the task to be outlined
 */
SPTR ll_make_outlined_task(SPTR stblk_sptr, SPTR scope_sptr);

/**
   \brief ...
 */
int *ll_make_sections_args(SPTR lbSym, SPTR ubSym, SPTR stSym, SPTR lastSym);

/**
   \brief ...
 */
DTYPE ll_make_uplevel_type(SPTR stblk_sptr);

/**
   \brief ...
 */
int llProcessNextTmpfile(void);

/**
   \brief ...
 */
int ll_reset_parfile(void);

/**
   \brief ...
 */
int ll_rewrite_ilms(int lineno, int ilmx, int len);

/**
   \brief ...
 */
int ll_save_gtid_val(int bih);

/**
   \brief ...
 */
SPTR llvmAddConcurEntryBlk(int bih);

/**
   \brief ...
 */
int llvmGetExpbCurIlt(void);

/**
   \brief ...
 */
int llvm_get_unique_sym(void);

/**
   \brief ...
 */
int llvm_ilms_rewrite_mode(void);

/**
   \brief Dump the list of variables for the parallel regions specified by
   'sptr'.

   These variables should be used to make the uplevel struct when making a call
   to this outlined region.
 */
void dump_parsyms(int sptr);

/**
   \brief ...
 */
void finish_taskdup_routine(int curilm, int fnsptr, INT offset);

/**
   \brief ...
 */
void ilm_outlined_pad_ilm(int curilm);

/**
   \brief ...
 */
void ll_make_ftn_outlined_params(int func_sptr, int paramct, DTYPE *argtype);

/**
   \brief ...
 */
void ll_open_parfiles(void);

/**
   \brief ...
 */
void ll_reset_gtid(void);

/**
   \brief ...
 */
void ll_reset_outlined_func(void);

/**
   \brief ...
 */
void ll_set_outlined_currsub(void);

/**
   \brief ...
 */
void ll_unlink_parfiles(void);

/**
   \brief ...
 */
void llvmAddConcurExitBlk(int bih);

/**
   \brief ...
 */
void llvmSetExpbCurIlt(void);

/**
   \brief ...
 */
void llvm_set_unique_sym(int sptr);

/**
   \brief ...
 */
void ll_write_ilm_end(void);

/**
   \brief ...
 */
void ll_write_ilm_header(int outlined_sptr, int curilm);

/**
   \brief ...
 */
void llWriteNopILM(int lineno, int ilmx, int len);

/**
   \brief ...
 */
void restartRewritingILM(int curilm);

/**
   \brief ...
 */
void setOutlinedPragma(int func_sptr, int saved);

/**
   \brief ...
 */
void setRewritingILM(void);

/**
   \brief ...
 */
void start_taskdup(int task_fnsptr, int curilm);

/**
   \brief ...
 */
void stop_taskdup(int task_fnsptr, int curilm);

/**
   \brief ...
 */
void unsetRewritingILM(void);

/**
   \brief ...
 */
void update_acc_with_fn(int fnsptr);

/**
   \brief Return size of shared variable for task
 */
ISZ_T getTaskSharedSize(SPTR scope_sptr);

#ifdef OMP_OFFLOAD_LLVM
/**
   \brief Create an outlining function, which has function parameter for each symbol.
 */
SPTR ll_make_outlined_ompaccel_func(SPTR, SPTR, LOGICAL);

/**
   \brief Create an function call to the outlininin function.
 */
int ll_make_outlined_ompaccel_call(SPTR, SPTR);

/**
   \brief Disable symbol replacement at ILM while generating device code
 */
void ompaccel_symreplacer(bool);

/**
   \brief It is called when there is breduction ilm to avoid host reduction which is implemented by critical regions
 */
void ompaccel_notify_reduction(bool);

/**
   \brief
 */
bool ompaccel_is_reduction_region();

/**
   \brief
 */
int mk_function_call(DTYPE, int, DTYPE *, int *, SPTR);
#endif

#endif /* OUTLINER_H_ */
