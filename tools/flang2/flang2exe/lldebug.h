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

/**
   \file
   \brief Debug info generation for LLVM IR.
 */

#ifndef LLDEBUG_H_
#define LLDEBUG_H_

#include "gbldefs.h"
#include "ll_structure.h"
#include "llutil.h"

typedef struct {
  LL_MDRef mdnode; /**< mdnode for block */
  int sptr;        /**< block sptr */
  int startline;
  int endline;
  int keep;
  LL_MDRef *line_mdnodes; /**< mdnodes for block lines */
  LL_MDRef null_loc;
} BLKINFO;

typedef struct {
  LL_MDRef mdnode;
  INSTR_LIST *instr;
  int sptr;
} PARAMINFO;

struct sptr_to_mdnode_map {
  int sptr;
  LL_MDRef mdnode;
  struct sptr_to_mdnode_map *next;
};

#define BLK_STACK_SIZE 1024
#define PARAM_STACK_SIZE 1024

struct LL_DebugInfo {
  LL_Module *module;           /**< Pointer to the containing LL_Module */
  LL_MDRef llvm_dbg_sp;        /**< List of subprogram mdnodes */
  LL_MDRef llvm_dbg_gv;        /**< List of global variables mdnodes */
  LL_MDRef llvm_dbg_retained;  /**< List of retained type mdnodes */
  LL_MDRef llvm_dbg_enum;      /**< List of enum mdnodes */
  LL_MDRef llvm_dbg_imported;  /**< List of imported entity mdnodes */
  LL_MDRef *llvm_dbg_lv_array; /**< List of formal parameters to routine */
  char producer[1024];
  LL_MDRef comp_unit_mdnode;
  LL_MDRef *file_array;
  int file_array_sz;
  LL_MDRef cur_subprogram_mdnode;
  unsigned cur_subprogram_func_ptr_offset;
  LL_MDRef cur_parameters_mdnode;
  LL_MDRef cur_module_mdnode;
  LL_MDRef cur_cmnblk_mdnode;
  int cur_subprogram_lineno;
  LL_MDRef cur_subprogram_null_loc;
  LL_MDRef cur_line_mdnode;
  PARAMINFO param_stack[PARAM_STACK_SIZE];
  LL_MDRef *dtype_array;
  int dtype_array_sz;
  LL_MDRef texture_type_mdnode;

  BLKINFO cur_blk;
  BLKINFO *blk_tab;
  int blk_tab_size;
  int blk_idx;
  char *cur_module_name;

  int param_idx;
  int routine_count;
  int routine_idx;

  struct sptr_to_mdnode_map *sptrs_to_mdnodes;
  hashmap_t subroutine_mdnodes;
  hashset_t entity_func_added;

  unsigned scope_is_global : 1;
};

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
void lldbg_emit_subprogram(LL_DebugInfo *db, SPTR sptr, DTYPE ret_dtype,
                           int findex, bool targetNVVM);

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
                                    bool targetNVVM);

void lldbg_emit_cmblk_variables(LL_DebugInfo *, int, int, char *, int);

struct INSTR_TAG;

/// \brief Write out metadata definitions to the current LLVM file
void write_metadata_defs(LL_DebugInfo *db);

/**
   \brief ...
 */
char *lldbg_alloc(INT size);

/// \brief Encode an argument to lldbg_emit_expression_mdnode()
int lldbg_encode_expression_arg(LL_DW_OP_t op, int value);

/**
   \brief Always produce \c !dbg metadata for current location

   This produces location info even when none exists.
 */
LL_MDRef lldbg_cons_line(LL_DebugInfo *db);

/**
   \brief Create a metadata node for the current compile unit
   \param db

   This function is idempotent.
 */
LL_MDRef lldbg_emit_compile_unit(LL_DebugInfo *db);

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
   \brief Emit a metadata node for a local variable in the current function
   \return a reference to the variable

   The returned reference can be used as the last argument to \c
   llvm.dbg.declare or \c llvm.dbg.value.
 */
LL_MDRef lldbg_emit_local_variable(LL_DebugInfo *db, SPTR sptr, int findex,
                                   int emit_dummy_as_local);

/**
   \brief ...
 */
LL_MDRef lldbg_emit_module_mdnode(LL_DebugInfo *db, int sptr);

/**
   \brief Emit DILocalVariable for \p sptr parameter

   Emits a metadata node for a formal parameter to the current function.  The
   returned reference can be used as the last argument to \c llvm.dbg.declare
   or \c llvm.dbg.value.
 */
LL_MDRef lldbg_emit_param_variable(LL_DebugInfo *db, SPTR sptr, int findex,
                                   int parnum, bool unnamed);

/**
   \brief ...
 */
LL_MDRef lldbg_emit_ptr_param_variable(LL_DebugInfo *db, SPTR sptr, int findex,
                                       int parnum);

/// \brief Get metadata node representing the current line for \c !dbg
LL_MDRef lldbg_get_line(LL_DebugInfo *db);

/**
   \brief Get the metadata node representing the line for a var definition
   \param sptr  The variable to lookup
 */
LL_MDRef lldbg_get_var_line(LL_DebugInfo *db, int sptr);

/**
   \brief Get the \c DISubprogram for the current procedure
   \param db  the debug info object

   Note this has a side-effect: it clears the cached metadata.  This is to
   prevent the next function from re-using this one's DISubprogram.
 */
LL_MDRef lldbg_subprogram(LL_DebugInfo *db);

/**
   \brief Emit DICommonBlock mdnode
 */
LL_MDRef lldbg_emit_common_block_mdnode(LL_DebugInfo *db, SPTR sptr);

/**
   \brief ...
 */
void lldbg_create_cmblk_mem_mdnode_list(SPTR sptr, SPTR gblsym);

/**
   \brief ...
 */
void lldbg_cleanup_missing_bounds(LL_DebugInfo *db, int findex);

/**
   \brief ...
 */
void lldbg_emit_accel_global_variable(LL_DebugInfo *db, SPTR sptr, int findex,
                                      LL_Value *var_ptr, int addrspace,
                                      int is_local);

/**
   \brief Emit a metadata node for a global variable.

   Note that all LLVM globals are referenced as pointers, so \p value should
   have a pointer type.
 */
void lldbg_emit_global_variable(LL_DebugInfo *db, SPTR sptr, BIGINT off,
                                int findex, LL_Value *value);

/**
   \brief ...
 */
void lldbg_emit_line(LL_DebugInfo *db, int lineno);

/**
   \brief ...
 */
void lldbg_emit_lv_list(LL_DebugInfo *db);

/**
   \brief ...
 */
void lldbg_emit_outlined_parameter_list(LL_DebugInfo *db, int findex,
                                        DTYPE *param_dtypes, int num_args);

/**
   \brief Free all memory used by \p db
   \param db

   Don't call this directly, it is called from ll_destroy_module.
 */
void lldbg_free(LL_DebugInfo *db);

/**
   \brief Construct debug information at end of routine
   \param db    debug info instance
   \param func  current function symbol
 */
void lldbg_function_end(LL_DebugInfo *db, int func);

/**
   \brief Initialize dtype arrays
   \param db
 */
void lldbg_init_arrays(LL_DebugInfo *db);

/**
   \brief Allocate and initialize debug info generation for module
   \param module
 */
void lldbg_init(LL_Module *module);

/**
   \brief ...
 */
void lldbg_register_value_call(LL_DebugInfo *db, INSTR_LIST *instr, int sptr);

/**
   \brief ...
 */
void lldbg_reset_dtype_array(LL_DebugInfo *db, const int off);

/// \brief Provide a function pointer to the curent subprogram
void lldbg_set_func_ptr(LL_DebugInfo *db, LL_Value *func_ptr);

/**
   \brief Make room for new dtypes
   \param db         The debug info
   \param lastDType  dtype from which to bzero when extended
   \param newSz      the new size of dtype_array
 */
void lldbg_update_arrays(LL_DebugInfo *db, int lastDType, int newSz);

/**
   \brief ...
 */
void lldbg_emit_imported_entity(LL_DebugInfo *db, int entity_sptr,
                                int func_sptr, int is_mod);

/// \brief Initialize the DIFLAG values
/// The values may vary depending on the LLVM branch being used
void InitializeDIFlags(const LL_IRFeatures *feature);

void lldbg_reset_module(LL_DebugInfo *db);

#endif /* LLDEBUG_H_ */
