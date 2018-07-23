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

#ifndef ILIUTIL_H_
#define ILIUTIL_H_

#include "gbldefs.h"
#include "error.h"
#include "global.h"
#include "symtab.h"
#include "ilm.h"
#include "ilmtp.h"
#include "ili.h"
#include "mth.h"

/* exported variables */

extern bool share_proc_ili;
extern bool share_qjsr_ili;

/* exported functions */

/**
   \brief ...
 */
ATOMIC_INFO atomic_decode(int encoding);

#ifdef __cplusplus
inline MSZ GetAtomicDecodeMsz(int encoding) {
  return static_cast<MSZ>(atomic_decode(encoding).msz);
}
#else
#define GetAtomicDecodeMsz(E) atomic_decode(E).msz
#endif

/**
   \brief ...
 */
ATOMIC_INFO atomic_info(int ilix);

/**
   \brief ...
 */
BIGINT get_isz_conili(int ili);

/**
   \brief ...
 */
bool cc_includes_equality(CC_RELATION cc);

/**
   \brief ...
 */
bool ccs_are_complementary(CC_RELATION cc1, CC_RELATION cc2);

/**
   \brief ...
 */
bool cmpxchg_is_weak(int ilix);

/**
   \brief ...
 */
bool _find_ili(int ilix, int find_this);

/**
   \brief ...
 */
bool find_ili(int tree, int it);

/**
   \brief ...
 */
bool func_in(int ilix);

/**
   \brief ...
 */
bool is_floating_comparison_opcode(ILI_OP opc);

/**
   \brief ...
 */
bool is_integer_comparison_opcode(ILI_OP opc);

/**
   \brief ...
 */
bool is_llvm_local(int sptr, int funcsptr);

/**
   \brief ...
 */
bool is_llvm_local_private(int sptr);

/**
   \brief ...
 */
bool is_omp_atomic_ld(int ilix);

/**
   \brief ...
 */
bool is_omp_atomic_st(int ilix);

/**
   \brief ...
 */
bool is_unsigned_opcode(ILI_OP opc);

/**
   \brief ...
 */
bool qjsr_in(int ilix);

/**
   \brief ...
 */
CC_RELATION combine_ieee_ccs(CC_RELATION binary_cc, CC_RELATION zero_cc);

/**
   \brief ...
 */
CC_RELATION combine_int_ccs(CC_RELATION binary_cc, CC_RELATION zero_cc);

/**
   \brief ...
 */
CC_RELATION commute_cc(CC_RELATION cc);

/**
   \brief ...
 */
CC_RELATION complement_ieee_cc(CC_RELATION cc);

/**
   \brief ...
 */
CC_RELATION complement_int_cc(CC_RELATION cc);

/**
   \brief ...
 */
char *dump_msz(MSZ ms);

/**
   \brief ...
 */
char *fast_math(char *root, int widthc, int typec, char *oldname);

/**
   \brief ...
 */
char *gnr_math(char *root, int widthc, int typec, char *oldname, int masked);

/**
   \brief ...
 */
char *make_math(MTH_FN fn, SPTR *fptr, int vectlen, bool mask, DTYPE res_dt, int nargs, int arg1_dt_, ...);

/**
   \brief ...
 */
char *make_math_name(MTH_FN fn, int vectlen, bool mask, DTYPE res_dt);

/**
   \brief ...
 */
char *make_math_name_vabi(MTH_FN fn, int vectlen, bool mask, DTYPE res_dt);

/**
   \brief ...
 */
char *relaxed_math(char *root, int widthc, int typec, char *oldname);

/**
   \brief ...
 */
char *scond(int c);

/**
   \brief ...
 */
CMPXCHG_MEMORY_ORDER cmpxchg_memory_order(int ilix);

/**
   \brief ...
 */
DTYPE ili_get_vect_dtype(int ilix);

/**
   \brief ...
 */
ILI_OP ldopc_from_stopc(ILI_OP stopc);

/**
   \brief ...
 */
int ad1ili(ILI_OP opc, int opn1);

/**
   \brief ...
 */
int ad2func_kint(ILI_OP opc, char *name, int opn1, int opn2);

/**
   \brief ...
 */
int ad2ili(ILI_OP opc, int opn1, int opn2);

/**
   \brief ...
 */
int ad3ili(ILI_OP opc, int opn1, int opn2, int opn3);

/**
   \brief ...
 */
int ad4ili(ILI_OP opc, int opn1, int opn2, int opn3, int opn4);

/**
   \brief ...
 */
int ad5ili(ILI_OP opc, int opn1, int opn2, int opn3, int opn4, int opn5);

/**
   \brief ...
 */
int ad_aconi(BIGINT val);

/**
   \brief ...
 */
int ad_acon(int sym, BIGINT val);

/**
   \brief ...
 */
int ad_aconk(INT m32, INT l32);

/**
   \brief ...
 */
int ad_cmpxchg(ILI_OP opc, int ilix_val, int ilix_loc, int nme, int stc_atomic_info, int ilix_comparand, int ilix_is_weak, int ilix_success, int ilix_failure);

/**
   \brief ...
 */
int ad_cse(int ilix);

/**
   \brief ...
 */
int addili(ILI *ilip);

/**
   \brief ...
 */
int ad_free(int ilix);

/**
   \brief ...
 */
int ad_icon(INT val);

/**
   \brief ...
 */
int ad_kconi(BIGINT v);

/**
   \brief ...
 */
int ad_kcon(INT m32, INT l32);

/**
   \brief ...
 */
int ad_load(int stx);

/**
   \brief ...
 */
int alt_qjsr(int ilix);

/**
   \brief ...
 */
int atomic_encode(MSZ msz, SYNC_SCOPE scope, ATOMIC_ORIGIN origin);

/**
   \brief ...
 */
int atomic_encode_rmw(MSZ msz, SYNC_SCOPE scope, ATOMIC_ORIGIN origin, ATOMIC_RMW_OP op);

/**
   \brief ...
 */
int atomic_info_index(ILI_OP opc);

/**
   \brief ...
 */
int cmpxchg_loc(int ilix);

/**
   \brief ...
 */
int compl_br(int ilix, int lbl);

/**
   \brief ...
 */
int compute_address(int sptr);

/**
   \brief ...
 */
int gencallargs(void);

/**
   \brief ...
 */
int genregcallargs(void);

/**
   \brief ...
 */
int genretvalue(int ilix, ILI_OP resultopc);

/**
   \brief ...
 */
int get_ili_ns(ILI *ilip);

/**
   \brief ...
 */
int get_rewr_new_nme(int nmex);

/**
   \brief ...
 */
int has_cse(int ilix);

/**
   \brief ...
 */
int iadd_const_ili(BIGINT valconst, int valilix);

/**
   \brief ...
 */
int iadd_ili_ili(int leftx, int rightx);

/**
   \brief ...
 */
int idiv_ili_const(int valilix, BIGINT valconst);

/**
   \brief ...
 */
int idiv_ili_ili(int leftx, int rightx);

/**
   \brief ...
 */
int ikmove(int ilix);

/**
   \brief ...
 */
int ili_get_vect_arg_count(int ilix);

/**
   \brief ...
 */
int ili_isdeleted(int ili);

/**
   \brief ...
 */
int ili_opnd(int ilix, int n);

/**
   \brief ...
 */
int ili_subscript(int sub);

/**
   \brief ...
 */
int ili_throw_label(int ilix);

/**
   \brief ...
 */
int ili_traverse(int (*visit_f)(int), int ilix);

/**
   \brief ...
 */
int ilstckind(ILI_OP opc, int opnum);

/**
   \brief ...
 */
int imax_ili_ili(int leftx, int rightx);

/**
   \brief ...
 */
int imin_ili_ili(int leftx, int rightx);

/**
   \brief ...
 */
int imul_const_ili(BIGINT valconst, int valilix);

/**
   \brief ...
 */
int imul_ili_ili(int leftx, int rightx);

/**
   \brief ...
 */
int is_argili_opcode(ILI_OP opc);

/**
   \brief ...
 */
int is_cseili_opcode(ILI_OP opc);

/**
   \brief ...
 */
int is_daili_opcode(ILI_OP opc);

/**
   \brief ...
 */
int is_dfrili_opcode(ILI_OP opc);

/**
   \brief ...
 */
int is_freeili_opcode(ILI_OP opc);

/**
   \brief ...
 */
int is_mvili_opcode(ILI_OP opc);

/**
   \brief ...
 */
int is_rgdfili_opcode(ILI_OP opc);

/**
   \brief ...
 */
int isub_ili_ili(int leftx, int rightx);

/**
   \brief ...
 */
int jsrsearch(int ilix);

/**
   \brief ...
 */
int kimove(int ilix);

/**
   \brief ...
 */
int ll_ad_outlined_func(ILI_OP result_opc, ILI_OP call_opc, char *func_name, int narg, int arg1, int arg2, int arg3);

/**
   \brief ...
 */
int mk_address(int sptr);

/**
   \brief ...
 */
int mk_charlen_parref_sptr(int sptr);

/**
   \brief ...
 */
int mkfunc_avx(char *nmptr, int avxp);

/**
   \brief ...
 */
int qjsrsearch(int ilix);

/**
   \brief ...
 */
int rewr_ili(int tree, int old, int New);

/**
   \brief ...
 */
int rewr_ili_nme(int tree, int oldili, int newili, int oldnme, int newnme, int douse, int dodef);

/**
   \brief ...
 */
int save_rewr_count(void);

/**
   \brief ...
 */
int sel_aconv(int ili);

/**
   \brief ...
 */
int sel_decr(int ili, int isi8);

/**
   \brief ...
 */
int sel_icnst(BIGINT val, int isi8);

/**
   \brief ...
 */
int sel_iconv(int ili, int isi8);

/**
   \brief ...
 */
int simplified_cmp_ili(int cmp_ili);

/**
   \brief ...
 */
int uikmove(int ilix);

/**
   \brief ...
 */
MEMORY_ORDER memory_order(int ilix);

/**
   \brief ...
 */
MSZ mem_size(TY_KIND ty);

/**
   \brief ...
 */
void addcallarg(int ili, int nme, int dtype);

/**
   \brief ...
 */
void choose_multiplier_64(int N, UINT64 dd, int prec);

/**
   \brief ...
 */
void choose_multiplier(int N, unsigned dd, int prec);

/**
   \brief ...
 */
void _ddilitree(int i, int flag);

/**
   \brief ...
 */
void ddilitree(int i, int flag);

/**
   \brief ...
 */
void dmpilitree(int i);

/**
   \brief ...
 */
void dmpili(void);

/**
   \brief ...
 */
void dump_atomic_info(FILE *f, ATOMIC_INFO info);

/**
   \brief ...
 */
void dump_ili(FILE *f, int i);

/**
   \brief ...
 */
void garbage_collect(void (*mark_function)(int));

/**
   \brief ...
 */
void ili_cleanup(void);

/**
   \brief ...
 */
void ili_init(void);

/**
   \brief ...
 */
void ili_unvisit(void);

/**
   \brief ...
 */
void ili_visit(int ilix, int v);

/**
   \brief ...
 */
void initcallargs(int count);

/**
   \brief ...
 */
void inline_mulh(void);

/**
   \brief ...
 */
void ldst_msz(DTYPE dtype, ILI_OP *ld, ILI_OP *st, MSZ *siz);

/**
   \brief ...
 */
void llmk_math_name(char *buff, int fn, int vectlen, bool mask, DTYPE res_dt);

/**
   \brief ...
 */
void prilitree(int i);

/**
   \brief ...
 */
void restore_rewr_count(int c);

/**
   \brief ...
 */
void rewr_cln_ili(void);

/**
   \brief ...
 */
void rewr_these_ili(int oldili, int newili);

/**
   \brief ...
 */
void rewr_these_ili_nme(int oldili, int newili, int oldnme, int newnme);

#endif // ILIUTIL_H_
