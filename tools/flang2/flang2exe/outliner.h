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
 * \brief Various definitions for the outliner module
 */

#ifndef __OUTLINER_H__
#define __OUTLINER_H__

/* Dump the list of variables for the parallel regions specified by 'sptr'.
 * These variables should be used to make the uplevel struct when making a call
 * to this outlined region.
 */
void dump_parsyms(int sptr);
char *ll_get_outlined_funcname(int, int);

int ll_reset_parfile(void);
int ll_rewrite_ilms(int, int, int);
void ll_write_ilm_header(int, int);
void ll_write_ilm_end(void);
void ilm_outlined_pad_ilm(int);
void llWriteNopILM(int, int, int);
LOGICAL ll_ilm_is_rewriting(void);
void setRewritingILM();
void unsetRewritingILM();

int ll_has_cuda_constructor(void);
void ll_save_cuda_constructor(void);

int ll_make_outlined_call(int, int, int, int);
int ll_make_outlined_call2(int, int);
int ll_make_outlined_func(int, int);
int ll_make_outlined_task(int, int);
int ll_make_outlined_task_call(int, int);
int *ll_make_sections_args(int, int, int, int);
void ll_make_ftn_outlined_params(int, int, int *);
int ll_get_hostprog_arg(int, int);

int llvm_ilms_rewrite_mode(void);
int llvm_get_unique_sym(void);
void llvm_set_unique_sym(int);
int ll_get_gtid_val_ili(void);
int ll_get_gtid_addr_ili(void);
void ll_reset_gtid();
int ll_get_gtid();
int ll_save_gtid_val(int);

/* Routines for handling uplevel arguments */
int ll_load_outlined_args(int, int, LOGICAL);
int ll_get_uplevel_sym();

extern int ll_ad_outlined_func2(int, int, int, int, int *);
extern int ll_make_outlined_garg(int, int *, int *);
extern int ll_get_uplevel_arg(void);
extern int ll_get_uplevel_offset(int sptr);
extern int ll_get_shared_arg(int func_sptr);
extern void ll_reset_outlined_func(void);
extern void ll_open_parfiles(void);
extern void ll_set_outlined_currsub(void);
extern int ll_has_more_outlined(void);
extern void ll_unlink_parfiles(void);
extern void restartRewritingILM(int);

extern int llvmAddConcurEntryBlk(int);
extern void llvmAddConcurExitBlk(int);

extern void update_acc_with_fn(int);

extern void start_taskdup(int, int );
extern void stop_taskdup(int, int );
extern void finish_taskdup_routine(int, int, INT);

#endif /* __OUTLINER_H__ */
