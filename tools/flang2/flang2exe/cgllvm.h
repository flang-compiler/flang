/*
 * Copyright (c) 2010-2018, NVIDIA CORPORATION.  All rights reserved.
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
 * \brief Include file for ILI to LLVM translation
 */

#ifndef CGLLVM_H__
#define CGLLVM_H__

#include "llutil.h"

void cprintf(char *s, const char *format, INT *val);

#define SNAME(sptr) (sptr_array[sptr])
#define LLTYPE(sptr) (sptr_type_array[sptr])
#define LLTYPE_kind(sptr) (sptr_type_array[sptr]->kind)
#define LLTYPE_size(sptr) (sptr_type_array[sptr]->size)

#define AGGREGATE_STYPE(s) \
  ((s) == ST_STRUCT || (s) == ST_UNION || (s) == ST_ARRAY)
#define AGGREGATE_DTYPE(d) \
  ((DTY(d)) == TY_STRUCT || (DTY(d)) == TY_UNION || (DTY(d)) == TY_ARRAY)
#define COMPLEX_DTYPE(d) ((DTY(d)) == TY_CMPLX || (DTY(d)) == TY_DCMPLX)
#define VECTOR_DTYPE(d) ((DTY(d)) == TY_VECT)

#define LLCCF_NEG                                                         \
  {                                                                       \
    LLCCF_NONE, LLCCF_TRUE, LLCCF_UNE, LLCCF_ULE, LLCCF_ULT, LLCCF_UGE,   \
        LLCCF_UGT, LLCCF_UNE, LLCCF_UNO, LLCCF_ONE, LLCCF_OLE, LLCCF_OLT, \
        LLCCF_OGE, LLCCF_OGT, LLCCF_OEQ, LLCCF_ORD, LLCCF_FALSE           \
  }

/*  functions defined in cgmain.c file:  */

void schedule(void);
void process_global_lifetime_debug(void);
OPERAND *gen_llvm_expr(int ilix, LL_Type *expected_type);
void clear_deletable_flags(int ilix);
TMPS *gen_extract_insert(int, LL_Type *, TMPS *, LL_Type *, TMPS *, LL_Type *,
                         int);
OPERAND *gen_call_to_builtin(int, char *, OPERAND *, LL_Type *, INSTR_LIST *,
                             int);
INSTR_LIST *llvm_info_last_instr(void);
/* Use MSZ_TO_BYTES to detect presence of MSZ */
#ifdef MSZ_TO_BYTES
OPERAND *gen_address_operand(int, int, bool, LL_Type *, MSZ);
DTYPE msz_dtype(MSZ msz);
#endif
const char *char_type(int dtype, int sptr);
void update_external_function_declarations(const char *, char *, unsigned);
void cg_fetch_clen_parampos(SPTR *len, int *param, SPTR sptr);

extern LL_Module *cpu_llvm_module;

typedef enum STMT_Type {
  STMT_NONE = 0,
  STMT_RET = 1,
  STMT_EXPR = 2,
  STMT_LABEL = 3,
  STMT_BR = 4,
  STMT_ST = 5,
  STMT_CALL = 6,
  STMT_SMOVE = 7,
  STMT_SZERO = 8,
  STMT_DECL = 9,
  STMT_LAST = 10
} STMT_Type;

#define BITOP(i) ((i) >= I_SHL && (i) <= I_XOR)
#define BINOP(i) ((i) >= I_ADD && (i) <= I_FREM)
#define CONVERT(i) ((i) >= I_TRUNC && (i) <= I_BITCAST)
#define PICALL(i) ((i) == I_PICALL)

#define CMP_FLT 0
#define CMP_INT 1
#define CMP_USG 2

typedef enum {
  MATCH_NO = -1,
  MATCH_OK = 0,
  MATCH_MEM = 1,
  MATCH_LAST = 2
} MATCH_Kind;

/* TMP flags */
#define CARRAY_TMP 1

/* external declaration flags */
#define EXF_INTRINSIC 1
#define EXF_STRUCT_RETURN 2
#define EXF_VARARG 4

#define IS_OLD_STYLE_CAND(s) (DEFDG(sptr) || CCSYMG(sptr))

extern char **sptr_array;
extern LL_Type **sptr_type_array;

char *get_llvm_name(int sptr);		/* see llassem*.c */
char *get_llvm_sname(int sptr);
char *get_llvm_mips_sname(int sptr);

void cg_llvm_init(void);
void cg_llvm_end(void);
void cg_llvm_fnend(void);
void llvm_ctor_add(const char *);
void llvm_ctor_add_with_priority(const char *name, int priority);
void llvm_dtor_add(const char *);
void llvm_dtor_add_with_priority(const char *name, int priority);
void llvm_write_ctors(void);

extern FILE *par_file1;
extern FILE *par_file2;

int get_return_type(int func_sptr);
int cg_get_type(int n, int v1, int v2);
void build_routine_and_parameter_entries(int func_sptr, LL_ABI_Info *abi,
                                         LL_Module *module);
bool strict_match(LL_Type *, LL_Type *);
bool is_cg_llvm_init(void);
void process_formal_arguments(LL_ABI_Info *);
void write_external_function_declarations(int);

OPERAND *mk_alloca_instr(LL_Type *ptrTy);
INSTR_LIST *mk_store_instr(OPERAND *val, OPERAND *addr);

int get_return_type(int);
#ifdef TARGET_LLVM_X8664
LL_Type *maybe_fixup_x86_abi_return(LL_Type *sig);
#endif

/* ll_ftn.c */
void store_llvm_localfptr(void);
void stb_process_routine_parameters(void);
LOGICAL has_multiple_entries(int sptr);
int get_entries_argnum(void);
void get_local_overlap_size(void);
void write_master_entry_routine(void);
char *get_llvm_ifacenm(int sptr);
int get_iface_sptr(int sptr);
int is_iso_cptr(int d_dtype);
extern void ll_process_routine_parameters(int sptr);
void fix_llvm_fptriface(void);
char *get_entret_arg_name(void);

/* vpar.c */
void llvmRewriteConcurIli(int bbih, int ebih, int display);
void vpar(void);

#endif /* CGLLVM_H__ */
