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

/**
   \brief Allocate and initialize debug info generation for module
   \param module
 */
void lldbg_init(LLVMModuleRef module);

/**
   \brief Free all memory used by \p db
   \param db

   Don't call this directly, it is called from ll_destroy_module.
 */
void lldbg_free(LL_DebugInfo *db);

/**
   \brief Initialize dtype arrays
   \param db
 */
void lldbg_init_arrays(LL_DebugInfo *db);

/**
   \brief Make room for new dtypes
   \param db         The debug info
   \param lastDType  dtype from which to bzero when extended
   \param newSz      the new size of dtype_array
 */
void lldbg_update_arrays(LL_DebugInfo *db, int last_dtype, int new_size);

/**
   \brief Create a metadata node for the current compile unit
   \param db

   This function is idempotent.
 */
LL_MDRef lldbg_emit_compile_unit(LL_DebugInfo *db);

/**
   \brief Create a metadata node for the current subprogram
   \param db
   \param sptr
   \param ret_dtype
   \param findex

   Side-effect: stores the metadata node in the LL_DebugInfo struct.

   A function pointer to the corresponding LLVM function must be set later by
   lldbg_set_func_ptr().
 */
void lldbg_emit_subprogram(LL_DebugInfo *db, int sptr, int ret_dtype,
                           int findex, LOGICAL);

/**
   \brief Create a metadata node for the outlined subprogram \p sptr
   \param db
   \param sptr
   \param findex
   \param func_name
   \param startlineno
   
   Side-effect: store the metadata in the LL_DebugInfo struct.
   
 
   A function pointer to the corresponding LLVM function must be set later by
   lldbg_set_func_ptr().
 */
void lldbg_emit_outlined_subprogram(LL_DebugInfo *db, int sptr, int findex,
                                    const char *func_name, int startlineno,
                                    LOGICAL);

/**
   \brief Provide a function pointer to the curent subprogram
 */
void lldbg_set_func_ptr(LL_DebugInfo *db, LL_Value *func_ptr);

void lldbg_reset_dtype_array(LL_DebugInfo *, const int off);

/**
   \brief Get the \c DISubprogram for the current procedure
   \param db  the debug info object

   Note this has a side-effect: it clears the cached metadata.  This is to
   prevent the next function from re-using this one's DISubprogram.
 */
LL_MDRef lldbg_subprogram(LL_DebugInfo *db);

/**
   \brief Emit a metadata node for a local variable in the current function
   \return a reference to the variable

   The returned reference can be used as the last argument to \c
   llvm.dbg.declare or \c llvm.dbg.value.
 */
LL_MDRef lldbg_emit_local_variable(LL_DebugInfo *db, int sptr, int findex,
                                   int emit_dummy_as_local);

/**
   \brief Emit DILocalVariable for \p sptr parameter

   Emits a metadata node for a formal parameter to the current function.  The
   returned reference can be used as the last argument to \c llvm.dbg.declare
   or \c llvm.dbg.value.
 */
LL_MDRef lldbg_emit_param_variable(LL_DebugInfo *db, int sptr, int findex,
                                   int parnum, bool unnamed);

/**
   \brief Emit a metadata node for a global variable.
 
   Note that all LLVM globals are referenced as pointers, so \p value should
   have a pointer type.
 */
void lldbg_emit_global_variable(LL_DebugInfo *db, int sptr, ISZ_T off, 
                                int findex, LL_Value *var_ptr);

/**
   \brief Emit empty expression mdnode

   Metadata for \code{!DIExpression()}.
 */
LL_MDRef lldbg_emit_empty_expression_mdnode(LL_DebugInfo *db);

/**
   \brief Emit expression mdnode
   \param db   pointer to the debug info
   \param cnt  the number of arguments to the expression

   Each argument to the DIExpression needs to be encoded using the function
   lldbg_encode_expression_arg().

   Metadata for \code{!DIExpression(} \e op [, \e op ]* \code{)}
 */
LL_MDRef lldbg_emit_expression_mdnode(LL_DebugInfo *db, unsigned cnt, ...);

/**
   \brief Encode an argument to lldbg_emit_expression_mdnode()
 */
int lldbg_encode_expression_arg(LL_DW_OP_t op, int value);

void lldbg_emit_line(LL_DebugInfo *, int lineno);
void lldbg_emit_lv_list(LL_DebugInfo *);
void lldbg_emit_outlined_parameter_list(LL_DebugInfo *, int, int *, int);
void lldbg_emit_cmblk_variables(LL_DebugInfo *, int, int, char *, int);
LL_MDRef lldbg_emit_ptr_param_variable(LL_DebugInfo *, int, int, int);

/**
   \brief Get metadata node representing the current line for \c !dbg
 */
LL_MDRef lldbg_get_line(LL_DebugInfo *db);

/**
   \brief Always produce \c !dbg metadata for current location

   This produces location info even when none exists.
 */
LL_MDRef lldbg_cons_line(LL_DebugInfo *db);

/**
   \brief Construct debug information at end of routine
   \param db    debug info instance
   \param func  current function symbol
 */
void lldbg_function_end(LL_DebugInfo *db, int currFunc);

/**
   \brief Get the metadata node representing the line for a var definition
   \param sptr  The variable to lookup
 */
LL_MDRef lldbg_get_var_line(LL_DebugInfo *db, int sptr);

struct INSTR_TAG;
void lldbg_register_value_call(LL_DebugInfo *db, struct INSTR_TAG *instr,
                               int sptr);

char *lldbg_alloc(INT size);

/**
   \brief Write out metadata definitions to the current LLVM file
 */
void write_metadata_defs(LL_DebugInfo *db);

// used by lldebug.c
char *get_llvm_mips_sname(int sptr);

void lldbg_cleanup_missing_bounds(LL_DebugInfo *db, int findex);

#endif /* LLDEBUG_H__ */
