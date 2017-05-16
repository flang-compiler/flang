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

/**
   \file
   \brief Debug info generation for LLVM IR.
 */

#ifndef LLDEBUG_H__
#define LLDEBUG_H__

#include "ll_structure.h"

/* Allocate and initialize debug info generation for module. */
void lldbg_init(LLVMModuleRef module);

/* Free all memory used by db. Don't call this directly, it is called from
 * ll_destroy_module. */
void lldbg_free(LL_DebugInfo *);

void lldbg_init_arrays(LL_DebugInfo *);
void lldbg_update_arrays(LL_DebugInfo *db, int last_dtype, int new_size);

/* Create a metadata node for the current compile unit. This function is
 * idempotent. */
LL_MDRef lldbg_emit_compile_unit(LL_DebugInfo *);

/* Create a metadata node for the current subprogram and store it in the
 * LL_DebugInfo struct.
 *
 * A function pointer to the corresponding LLVM function must be set later by
 * lldbg_set_func_ptr().
 */
void lldbg_emit_subprogram(LL_DebugInfo *, int sptr, int ret_dtype, int findex,
                           LOGICAL targetNVVM);

/* Create a metadata node for the current outlined subprogram and store it in
 * the LL_DebugInfo struct.
 *
 * A function pointer to the corresponding LLVM function must be set later by
 * lldbg_set_func_ptr().
 */
void lldbg_emit_outlined_subprogram(LL_DebugInfo *, int sptr, int findex,
                                    const char *func_name, int startlineno,
                                    LOGICAL targetNVVM);

/* Provide a function pointer to the curent subprogram. */
void lldbg_set_func_ptr(LL_DebugInfo *, LL_Value *func_ptr);

LL_MDRef lldbg_subprogram(LL_DebugInfo *);

/* Emit a metadata node for a local variable in the current function, and
 * return a reference to it.
 * The returned reference can be used as the last argument to llvm.dbg.declare
 * or llvm.dbg.value. */
LL_MDRef lldbg_emit_local_variable(LL_DebugInfo *, int, int, int);

/* Emit a metadata node for a formal parameter to the current function.
 * The returned reference can be used as the last argument to llvm.dbg.declare
 * or llvm.dbg.value. */
LL_MDRef lldbg_emit_param_variable(LL_DebugInfo *, int, int, int);

void lldbg_emit_global_variable(LL_DebugInfo *, int sptr, ISZ_T off, 
                                int findex, LL_Value *var_ptr);

LL_MDRef lldbg_emit_empty_expression_mdnode(LL_DebugInfo *);
LL_MDRef lldbg_emit_expression_mdnode(LL_DebugInfo *, unsigned, ...);
int lldbg_encode_expression_arg(LL_DW_OP_t op, int value);

void lldbg_emit_line(LL_DebugInfo *, int lineno);
void lldbg_emit_lv_list(LL_DebugInfo *);
void lldbg_emit_outlined_parameter_list(LL_DebugInfo *, int, int *, int);
void lldbg_emit_cmblk_variables(LL_DebugInfo *, int, int, char *, int);
LL_MDRef lldbg_emit_ptr_param_variable(LL_DebugInfo *, int, int, int);
void lldbg_emit_accel_texture_variable(LL_DebugInfo *, char *, int, char *,
                                       char *, int, int, int);
void lldbg_emit_accel_global_variable(LL_DebugInfo *, int sptr, int findex,
                                      LL_Value *var_ptr, int addrspace,
                                      int is_local);
void lldbg_emit_accel_function_static_variables(LL_DebugInfo *, int, int,
                                                char *, int);
LL_MDRef lldbg_get_line(LL_DebugInfo *db);
LL_MDRef lldbg_cons_line(LL_DebugInfo *db);

/* Get the metadata node representing the line where the variable sptr was
 * defined. */
LL_MDRef lldbg_get_var_line(LL_DebugInfo *, int sptr);

struct INSTR_TAG;
void lldbg_register_value_call(LL_DebugInfo *, struct INSTR_TAG *, int);

char *lldbg_alloc(INT);

/* Write out metadata definitions to the current LLVM file. */
void write_metadata_defs(LL_DebugInfo *);

#endif /* LLDEBUG_H__ */
