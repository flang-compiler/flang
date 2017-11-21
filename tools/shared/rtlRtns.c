
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

/** \file Provides the front-end access to the run time library structure
 *  defined  in rte_rtns.h
 */

#include <string.h>
#include "gbldefs.h"
#include "global.h"
#include "error.h"
#include "rtlRtnsDesc.h"
#include "rtlRtns.h"

/* NOTE: within each section (E.g., NO RTN to END_OF_PFX_F90,
 * END_OF_PFX_F90+1 to ...) the ftnRtlRtns entries must be sorted on the
 * baseNm field.
 */
FtnRteRtn ftnRtlRtns[] = {
    {"NO RTN", "", FALSE, ""},
    {"achar", "", FALSE, ""},
    {"addr_1_dim_1st_elem", "", TRUE, ""},
    {"adjustl", "", FALSE, ""},
    {"adjustr", "", FALSE, ""},
    {"alloc", "", TRUE, "k"},
    {"alloc03", "", TRUE, ""},
    {"alloc03_chk", "", TRUE, ""},
    {"alloc04", "", TRUE, ""},
    {"alloc04_chk", "", TRUE, ""},
    {"alloc04_chkm", "", TRUE, ""},
    {"alloc04_chkp", "", TRUE, ""},
    {"alloc04m", "", TRUE, ""},
    {"alloc04p", "", TRUE, ""},
    {"allocated", "", TRUE, "k"},
    {"amodulev", "", FALSE, ""},
    {"amodulov", "", FALSE, ""},
    {"auto_alloc", "", TRUE, ""},
    {"auto_alloc04", "", TRUE, ""},
    {"auto_alloc04m", "", TRUE, ""},
    {"auto_alloc04p", "", TRUE, ""},
    {"auto_allocv", "", FALSE, ""},
    {"auto_calloc", "", TRUE, ""},
    {"auto_calloc04", "", TRUE, ""},
    {"auto_calloc04m", "", TRUE, ""},
    {"auto_calloc04p", "", TRUE, ""},
    {"auto_dealloc", "", TRUE, ""},
    {"auto_deallocm", "", TRUE, ""},
    {"auto_deallocp", "", TRUE, ""},
    {"c_f_procptr", "", FALSE, ""},
    {"c_f_ptr", "", TRUE, ""},
    {"calloc03", "", TRUE, ""},
    {"calloc04", "", TRUE, ""},
    {"calloc04m", "", TRUE, ""},
    {"calloc04p", "", TRUE, ""},
    {"ceiling", "", FALSE, "k"},
    {"ceilingv", "", FALSE, "k"},
    {"class_obj_size", "", TRUE, ""},
    {"cmd_arg_cnt", "", FALSE, "k"},
    {"cmplx16", "", FALSE, ""},
    {"cmplx32", "", FALSE, ""},
    {"cmplx8", "", FALSE, ""},
    {"conformable", "", FALSE, ""},
    {"conformable_dd", "", TRUE, ""},
    {"conformable_dn", "", TRUE, ""},
    {"conformable_nd", "", TRUE, ""},
    {"conformable_nn", "", TRUE, ""},
    {"copy_f77_argl", "", TRUE, ""},
    {"copy_f77_argsl", "", TRUE, ""},
    {"copy_f90_argl", "", TRUE, ""},
    {"dble", "", FALSE, ""},
    {"dceiling", "", FALSE, "k"},
    {"dceilingv", "", FALSE, "k"},
    {"dealloc", "", TRUE, ""},
    {"dealloc03", "", TRUE, ""},
    {"dealloc03m", "", TRUE, ""},
    {"dealloc03p", "", TRUE, ""},
    {"dealloc_mbr03", "", TRUE, ""},
    {"dealloc_mbr03m", "", TRUE, ""},
    {"dealloc_mbr03p", "", TRUE, ""},
    {"dealloc_poly03", "", TRUE, ""},
    {"dealloc_poly_mbr03", "", TRUE, ""},
    {"deallocx", "", TRUE, ""},
    {"dfloor", "", FALSE, "k"},
    {"dfloorv", "", FALSE, "k"},
    {"dmodulev", "", FALSE, ""},
    {"dmodulov", "", FALSE, ""},
    {"exit", "", FALSE, ""},
    {"expon", "", FALSE, "k"},
    {"expond", "", FALSE, "k"},
    {"expondx", "", FALSE, "k"},
    {"exponx", "", FALSE, "k"},
    {"extends_type_of", "", TRUE, "k"},
    {"finalize", "", TRUE, ""},
    {"floor", "", FALSE, "k"},
    {"floorv", "", FALSE, "k"},
    {"frac", "", FALSE, ""},
    {"fracd", "", FALSE, ""},
    {"fracdx", "", FALSE, ""},
    {"fracx", "", FALSE, ""},
    {"get_cmd", "", FALSE, ""},
    {"get_cmd_arg", "", FALSE, ""},
    {"get_env_var", "", FALSE, ""},
    {"hypot", "", FALSE, ""},
    {"hypotd", "", FALSE, ""},
    {"i8modulov", "", TRUE, ""},
    {"iachar", "", FALSE, "k"},
    {"ichar", "", FALSE, "k"},
    {"imodulov", "", FALSE, ""},
    {"index", "", FALSE, "k"},
    {"init_from_desc", "", TRUE, ""},
    {"init_unl_poly_desc", "", TRUE, ""},
    {"int", "", FALSE, ""},
    {"int1", "", FALSE, ""},
    {"int2", "", FALSE, ""},
    {"int4", "", FALSE, ""},
    {"int8", "", FALSE, ""},
    {"is_contiguous", "", TRUE, ""},
    {"is_iostat_end", "", FALSE, "k"},
    {"is_iostat_eor", "", FALSE, "k"},
    {"kexpondx", "", FALSE, ""},
    {"ksize", "", TRUE, ""},
    {"lb", "", TRUE, "k"},
    {"lb1", "", TRUE, ""},
    {"lb2", "", TRUE, ""},
    {"lb4", "", TRUE, ""},
    {"lb8", "", TRUE, ""},
    {"lba", "", TRUE, "k"},
    {"lba1", "", TRUE, ""},
    {"lba2", "", TRUE, ""},
    {"lba4", "", TRUE, ""},
    {"lba8", "", TRUE, ""},
    {"lbaz", "", TRUE, "k"},
    {"lbaz1", "", TRUE, ""},
    {"lbaz2", "", TRUE, ""},
    {"lbaz4", "", TRUE, ""},
    {"lbaz8", "", TRUE, ""},
    {"lbound", "", FALSE, "k"},
    {"lbound1", "", TRUE, ""},
    {"lbound2", "", TRUE, ""},
    {"lbound4", "", TRUE, ""},
    {"lbound8", "", TRUE, ""},
    {"lbounda", "", FALSE, "k"},
    {"lbounda1", "", FALSE, ""},
    {"lbounda2", "", FALSE, ""},
    {"lbounda4", "", FALSE, ""},
    {"lbounda8", "", FALSE, ""},
    {"lboundaz", "", FALSE, "k"},
    {"lboundaz1", "", FALSE, ""},
    {"lboundaz2", "", FALSE, ""},
    {"lboundaz4", "", FALSE, ""},
    {"lboundaz8", "", FALSE, ""},
    {"len", "", FALSE, "k"},
    {"lentrim", "", FALSE, "k"},
    {"loc", "", FALSE, ""},
    {"log1", "", FALSE, ""},
    {"log2", "", FALSE, ""},
    {"log4", "", FALSE, ""},
    {"log8", "", FALSE, ""},
    {"matmul_cplx16", "", TRUE, ""},
    {"matmul_cplx16mxv_t", "", TRUE, ""},
    {"matmul_cplx32", "", TRUE, ""},
    {"matmul_cplx8", "", TRUE, ""},
    {"matmul_cplx8mxv_t", "", TRUE, ""},
    {"matmul_int1", "", TRUE, ""},
    {"matmul_int2", "", TRUE, ""},
    {"matmul_int4", "", TRUE, ""},
    {"matmul_int8", "", TRUE, ""},
    {"matmul_log1", "", TRUE, ""},
    {"matmul_log2", "", TRUE, ""},
    {"matmul_log4", "", TRUE, ""},
    {"matmul_log8", "", TRUE, ""},
    {"matmul_real16", "", TRUE, ""},
    {"matmul_real4", "", TRUE, ""},
    {"matmul_real4mxv_t", "", TRUE, ""},
    {"matmul_real8", "", TRUE, ""},
    {"matmul_real8mxv_t", "", TRUE, ""},
    {"max", "", FALSE, "k"},
    {"mcopy1", "", FALSE, ""},
    {"mcopy2", "", FALSE, ""},
    {"mcopy4", "", FALSE, ""},
    {"mcopy8", "", FALSE, ""},
    {"mcopyz16", "", FALSE, ""},
    {"mcopyz4", "", FALSE, ""},
    {"mcopyz8", "", FALSE, ""},
    {"mergec", "", FALSE, ""},
    {"mergech", "", FALSE, ""},
    {"merged", "", FALSE, ""},
    {"mergedc", "", FALSE, ""},
    {"mergedt", "", FALSE, ""},
    {"mergei", "", FALSE, ""},
    {"mergei1", "", FALSE, ""},
    {"mergei2", "", FALSE, ""},
    {"mergei8", "", FALSE, ""},
    {"mergel", "", FALSE, ""},
    {"mergel1", "", FALSE, ""},
    {"mergel2", "", FALSE, ""},
    {"mergel8", "", FALSE, ""},
    {"mergeq", "", FALSE, ""},
    {"merger", "", FALSE, ""},
    {"min", "", FALSE, "k"},
    {"mmul_cmplx16", "", FALSE, ""},
    {"mmul_cmplx8", "", FALSE, ""},
    {"mmul_real4", "", FALSE, ""},
    {"mmul_real8", "", FALSE, ""},
    {"modulov", "", FALSE, ""},
    {"move_alloc", "", TRUE, ""},
    {"mp_bcs_nest", "", FALSE, ""},
    {"mp_ecs_nest", "", FALSE, ""},
    {"mset1", "", FALSE, ""},
    {"mset2", "", FALSE, ""},
    {"mset4", "", FALSE, ""},
    {"mset8", "", FALSE, ""},
    {"msetz16", "", FALSE, ""},
    {"msetz4", "", FALSE, ""},
    {"msetz8", "", FALSE, ""},
    {"mvbits", "", FALSE, ""},
    {"mzero1", "", FALSE, ""},
    {"mzero2", "", FALSE, ""},
    {"mzero4", "", FALSE, ""},
    {"mzero8", "", FALSE, ""},
    {"mzeroz16", "", FALSE, ""},
    {"mzeroz4", "", FALSE, ""},
    {"mzeroz8", "", FALSE, ""},
    {"nadjustl", "", FALSE, ""},
    {"nadjustr", "", FALSE, ""},
    {"name", "", FALSE, ""},
    {"nearest", "", FALSE, ""},
    {"nearestd", "", FALSE, ""},
    {"nearestdx", "", FALSE, ""},
    {"nearestx", "", FALSE, ""},
    {"nlen", "", TRUE, ""},
    {"nlentrim", "", FALSE, ""},
    {"nrepeat", "", FALSE, ""},
    {"nscan", "", FALSE, "k"},
    {"nstr_copy", "", FALSE, ""},
    {"nstr_copy_klen", "", FALSE, ""},
    {"nstr_index", "", FALSE, ""},
    {"nstr_index_klen", "", FALSE, ""},
    {"nstrcmp", "", FALSE, ""},
    {"nstrcmp_klen", "", FALSE, ""},
    {"ntrim", "", FALSE, ""},
    {"nverify", "", FALSE, "k"},
    {"pause", "", FALSE, ""},
    {"poly_asn", "", TRUE, ""},
    {"present", "", FALSE, "k"},
    {"present_ptr", "", FALSE, "k"},
    {"presentc", "", FALSE, "k"},
    {"ptr_alloc", "", FALSE, ""},
    {"ptr_alloc03", "", TRUE, ""},
    {"ptr_alloc04", "", TRUE, ""},
    {"ptr_alloc04m", "", FALSE, ""},
    {"ptr_alloc04p", "", FALSE, ""},
    {"ptr_calloc03", "", TRUE, ""},
    {"ptr_calloc04", "", TRUE, ""},
    {"ptr_calloc04m", "", FALSE, ""},
    {"ptr_calloc04p", "", FALSE, ""},
    {"ptr_src_alloc03", "", TRUE, ""},
    {"ptr_src_alloc04", "", TRUE, ""},
    {"ptr_src_alloc04m", "", TRUE, ""},
    {"ptr_src_alloc04p", "", TRUE, ""},
    {"ptr_src_calloc03", "", TRUE, ""},
    {"ptr_src_calloc04", "", TRUE, ""},
    {"ptr_src_calloc04m", "", TRUE, ""},
    {"ptr_src_calloc04p", "", TRUE, ""},
    {"ptrchk", "", FALSE, ""},
    {"ptrcp", "", FALSE, ""},
    {"real", "", FALSE, ""},
    {"real16", "", FALSE, ""},
    {"real4", "", FALSE, ""},
    {"real8", "", FALSE, ""},
    {"repeat", "", FALSE, ""},
    {"rrspacing", "", FALSE, ""},
    {"rrspacingd", "", FALSE, ""},
    {"rrspacingdx", "", FALSE, ""},
    {"rrspacingx", "", FALSE, ""},
    {"rtn_name", "", FALSE, ""},
    {"same_intrin_type_as", "", TRUE, "k"},
    {"same_type_as", "", TRUE, "k"},
    {"scale", "", FALSE, ""},
    {"scaled", "", FALSE, ""},
    {"scaledx", "", FALSE, ""},
    {"scalex", "", FALSE, ""},
    {"scan", "", FALSE, "k"},
    {"sect", "", TRUE, ""},
    {"sect1", "", TRUE, ""},
    {"sect1v", "", TRUE, ""},
    {"sect2", "", TRUE, ""},
    {"sect2v", "", TRUE, ""},
    {"sect3", "", TRUE, ""},
    {"sect3v", "", TRUE, ""},
    {"sel_char_kind", "", TRUE, "k"},
    {"sel_int_kind", "", TRUE, "k"},
    {"sel_real_kind", "", TRUE, "k"},
    {"set_intrin_type", "", TRUE, ""},
    {"set_type", "", TRUE, ""},
    {"setexp", "", FALSE, ""},
    {"setexpd", "", FALSE, ""},
    {"setexpdx", "", FALSE, ""},
    {"setexpx", "", FALSE, ""},
    {"shape", "", TRUE, "k"},
    {"shape1", "", TRUE, ""},
    {"shape2", "", TRUE, ""},
    {"shape4", "", TRUE, ""},
    {"shape8", "", TRUE, ""},
    {"show", "", FALSE, ""},
    {"size", "", TRUE, "k"},
    {"spacing", "", FALSE, ""},
    {"spacingd", "", FALSE, ""},
    {"spacingdx", "", FALSE, ""},
    {"spacingx", "", FALSE, ""},
    {"stop", "", FALSE, ""},
    {"stop08", "", FALSE, ""},
    {"str_copy", "", FALSE, ""},
    {"str_copy_klen", "", FALSE, ""},
    {"str_cpy1", "", FALSE, ""},
    {"str_free", "", FALSE, ""},
    {"str_index", "", FALSE, ""},
    {"str_index_klen", "", FALSE, ""},
    {"str_malloc", "", FALSE, ""},
    {"str_malloc_klen", "", FALSE, ""},
    {"strcmp", "", FALSE, ""},
    {"strcmp_klen", "", FALSE, ""},
    {"subchk", "", FALSE, ""},
    {"subchk64", "", FALSE, ""},
    {"template", "", TRUE, ""},
    {"template1", "", TRUE, ""},
    {"template1v", "", TRUE, ""},
    {"template2", "", TRUE, ""},
    {"template2v", "", TRUE, ""},
    {"template3", "", TRUE, ""},
    {"template3v", "", TRUE, ""},
    {"test_and_set_type", "", TRUE, ""},
    {"trim", "", FALSE, ""},
    {"ub", "", TRUE, "k"},
    {"ub1", "", TRUE, ""},
    {"ub2", "", TRUE, ""},
    {"ub4", "", TRUE, ""},
    {"ub8", "", TRUE, ""},
    {"uba", "", TRUE, "k"},
    {"uba1", "", TRUE, ""},
    {"uba2", "", TRUE, ""},
    {"uba4", "", TRUE, ""},
    {"uba8", "", TRUE, ""},
    {"ubaz", "", TRUE, "k"},
    {"ubaz1", "", TRUE, ""},
    {"ubaz2", "", TRUE, ""},
    {"ubaz4", "", TRUE, ""},
    {"ubaz8", "", TRUE, ""},
    {"ubound", "", FALSE, "k"},
    {"ubound1", "", TRUE, ""},
    {"ubound2", "", TRUE, ""},
    {"ubound4", "", TRUE, ""},
    {"ubound8", "", TRUE, ""},
    {"ubounda", "", FALSE, "k"},
    {"ubounda1", "", FALSE, ""},
    {"ubounda2", "", FALSE, ""},
    {"ubounda4", "", FALSE, ""},
    {"ubounda8", "", FALSE, ""},
    {"uboundaz", "", FALSE, "k"},
    {"uboundaz1", "", FALSE, ""},
    {"uboundaz2", "", FALSE, ""},
    {"uboundaz4", "", FALSE, ""},
    {"uboundaz8", "", FALSE, ""},
    {"verify", "", FALSE, "k"},
    {"END_OF_PFX_F90,", "", FALSE, ""},
    {"all", "", FALSE, ""},
    {"all_scatterx", "", FALSE, ""},
    {"alls", "", TRUE, ""},
    {"any", "", FALSE, ""},
    {"any_scatterx", "", FALSE, ""},
    {"anys", "", TRUE, ""},
    {"associated", "", TRUE, ""},
    {"associated_char", "", TRUE, ""},
    {"associated_t", "", TRUE, ""},
    {"associated_tchar", "", TRUE, ""},
    {"barrier", "", FALSE, ""},
    {"block_loop", "", FALSE, ""},
    {"comm_copy", "", FALSE, ""},
    {"comm_free", "", FALSE, ""},
    {"comm_gatherx", "", FALSE, ""},
    {"comm_scatterx", "", FALSE, ""},
    {"copy_out", "", TRUE, ""},
    {"count", "", FALSE, ""},
    {"counts", "", TRUE, ""},
    {"cpu_time", "", FALSE, ""},
    {"cpu_timed", "", FALSE, ""},
    {"cshift", "", FALSE, ""},
    {"cshiftc", "", FALSE, ""},
    {"cshifts", "", FALSE, ""},
    {"cshiftsc", "", FALSE, ""},
    {"cyclic_loop", "", FALSE, ""},
    {"dandt", "", TRUE, ""},
    {"date", "", FALSE, ""},
    {"datew", "", FALSE, ""},
    {"dotpr", "", TRUE, ""},
    {"eoshift", "", FALSE, ""},
    {"eoshiftc", "", FALSE, ""},
    {"eoshifts", "", FALSE, ""},
    {"eoshiftsa", "", FALSE, ""},
    {"eoshiftsac", "", FALSE, ""},
    {"eoshiftsc", "", FALSE, ""},
    {"eoshiftss", "", FALSE, ""},
    {"eoshiftssc", "", FALSE, ""},
    {"eoshiftsz", "", FALSE, ""},
    {"eoshiftszc", "", FALSE, ""},
    {"eoshiftz", "", FALSE, ""},
    {"eoshiftzc", "", FALSE, ""},
    {"extent", "", TRUE, ""},
    {"findloc", "", TRUE, "k"},
    {"findlocs", "", TRUE, "k"},
    {"findlocstr", "", TRUE, "k"},
    {"findlocstrs", "", TRUE, "k"},
    {"free", "", TRUE, ""},
    {"freen", "", FALSE, ""},
    {"ftime", "", TRUE, ""},
    {"ftimew", "", TRUE, ""},
    {"function_entry", "", FALSE, ""},
    {"function_exit", "", FALSE, ""},
    {"get_scalar", "", TRUE, ""},
    {"global_all", "", FALSE, ""},
    {"global_any", "", FALSE, ""},
    {"global_firstmax", "", FALSE, ""},
    {"global_firstmin", "", FALSE, ""},
    {"global_iall", "", FALSE, ""},
    {"global_iany", "", FALSE, ""},
    {"global_iparity", "", FALSE, ""},
    {"global_lastmax", "", FALSE, ""},
    {"global_lastmin", "", FALSE, ""},
    {"global_maxval", "", FALSE, ""},
    {"global_minval", "", FALSE, ""},
    {"global_parity", "", FALSE, ""},
    {"global_product", "", FALSE, ""},
    {"global_sum", "", FALSE, ""},
    {"globalize", "", FALSE, ""},
    {"iall_scatterx", "", FALSE, ""},
    {"iany_scatterx", "", FALSE, ""},
    {"idate", "", TRUE, ""},
    {"ilen", "", FALSE, ""},
    {"index", "", TRUE, "k"},
    {"indexx", "", TRUE, "k"},
    {"indexx_cr", "", TRUE, "k"},
    {"indexx_cr_nm", "", TRUE, "k"},
    {"init", "", FALSE, ""},
    {"instance", "", TRUE, ""},
    {"iparity_scatterx", "", FALSE, ""},
    {"islocal_idx", "", FALSE, ""},
    {"jdate", "", TRUE, ""},
    {"lastval", "", FALSE, ""},
    {"lbound1", "", FALSE, ""},
    {"lbound2", "", FALSE, ""},
    {"lbound4", "", FALSE, ""},
    {"lbound8", "", FALSE, ""},
    {"lbound", "", TRUE, "k"},
    {"lbounda1", "", TRUE, ""},
    {"lbounda2", "", TRUE, ""},
    {"lbounda4", "", TRUE, ""},
    {"lbounda8", "", TRUE, ""},
    {"lbounda", "", TRUE, "k"},
    {"lboundaz1", "", TRUE, ""},
    {"lboundaz2", "", TRUE, ""},
    {"lboundaz4", "", TRUE, ""},
    {"lboundaz8", "", TRUE, ""},
    {"lboundaz", "", TRUE, "k"},
    {"leadz", "", FALSE, ""},
    {"len", "", TRUE, "k"},
    {"lenx", "", TRUE, "k"},
    {"lenx_cr", "", TRUE, "k"},
    {"lenx_cr_nm", "", TRUE, "k"},
    {"line_entry", "", FALSE, ""},
    {"lineno", "", FALSE, ""},
    {"localize_bounds", "", FALSE, ""},
    {"localize_index", "", FALSE, ""},
    {"matmul", "", TRUE, ""},
    {"maxloc", "", TRUE, "k"},
    {"maxlocs", "", TRUE, "k"},
    {"maxval", "", TRUE, ""},
    {"maxval_scatterx", "", FALSE, ""},
    {"maxvals", "", TRUE, ""},
    {"member_base", "", FALSE, ""},
    {"minloc", "", TRUE, "k"},
    {"minlocs", "", TRUE, "k"},
    {"minval", "", TRUE, ""},
    {"minval_scatterx", "", FALSE, ""},
    {"minvals", "", TRUE, ""},
    {"np", "", FALSE, ""},
    {"nullify", "", TRUE, ""},
    {"nullify_char", "", TRUE, ""},
    {"nullifyx", "", TRUE, ""},
    {"number_of_processors", "", FALSE, "k"},
    {"olap_cshift", "", FALSE, ""},
    {"olap_eoshift", "", FALSE, ""},
    {"olap_shift", "", FALSE, ""},
    {"pack", "", TRUE, ""},
    {"packc", "", TRUE, ""},
    {"packz", "", TRUE, ""},
    {"packzc", "", TRUE, ""},
    {"parity_scatterx", "", FALSE, ""},
    {"permute_section", "", FALSE, ""},
    {"popcnt", "", FALSE, ""},
    {"poppar", "", FALSE, ""},
    {"processors", "", FALSE, ""},
    {"processors_rank", "", FALSE, "k"},
    {"product", "", FALSE, ""},
    {"product_scatterx", "", FALSE, ""},
    {"products", "", FALSE, ""},
    {"ptr_asgn", "", TRUE, ""},
    {"ptr_asgn_char", "", TRUE, ""},
    {"ptr_assign", "", TRUE, ""},
    {"ptr_assign_char", "", TRUE, ""},
    {"ptr_assign_charx", "", TRUE, ""},
    {"ptr_assignx", "", TRUE, ""},
    {"ptr_assn", "", TRUE, ""},
    {"ptr_assn_assumeshp", "", TRUE, ""},
    {"ptr_assn_char", "", TRUE, ""},
    {"ptr_assn_char_assumeshp", "", TRUE, ""},
    {"ptr_assn_charx", "", TRUE, ""},
    {"ptr_assn_dchar", "", TRUE, ""},
    {"ptr_assn_dchar_assumeshp", "", TRUE, ""},
    {"ptr_assn_dcharx", "", TRUE, ""},
    {"ptr_assnx", "", TRUE, ""},
    {"ptr_fix_assumeshp", "", TRUE, ""},
    {"ptr_fix_assumeshp1", "", TRUE, ""},
    {"ptr_fix_assumeshp2", "", TRUE, ""},
    {"ptr_fix_assumeshp3", "", TRUE, ""},
    {"ptr_in", "", TRUE, ""},
    {"ptr_in_char", "", TRUE, ""},
    {"ptr_offset", "", TRUE, ""},
    {"ptr_out", "", TRUE, ""},
    {"ptr_out_char", "", TRUE, ""},
    {"ptr_shape_assn", "", TRUE, ""},
    {"ptr_shape_assnx", "", TRUE, ""},
    {"qopy_in", "", TRUE, ""},
    {"realign", "", TRUE, ""},
    {"redistribute", "", TRUE, ""},
    {"reduce_descriptor", "", FALSE, ""},
    {"reshape", "", TRUE, ""},
    {"reshapec", "", TRUE, ""},
    {"rnum", "", TRUE, ""},
    {"rnumd", "", TRUE, ""},
    {"rseed", "", TRUE, ""},
    {"secnds", "", TRUE, ""},
    {"secndsd", "", TRUE, ""},
    {"shape", "", TRUE, "k"},
    {"size", "", TRUE, "k"},
    {"spread", "", FALSE, ""},
    {"spread_descriptor", "", FALSE, ""},
    {"spreadc", "", FALSE, ""},
    {"spreadcs", "", FALSE, ""},
    {"spreads", "", FALSE, ""},
    {"sum", "", TRUE, ""},
    {"sum_scatterx", "", FALSE, ""},
    {"sums", "", TRUE, ""},
    {"sysclk", "", TRUE, ""},
    {"template", "", TRUE, ""},
    {"transfer", "", TRUE, ""},
    {"type", "", FALSE, ""},
    {"typep", "", FALSE, ""},
    {"ubound1", "", FALSE, ""},
    {"ubound2", "", FALSE, ""},
    {"ubound4", "", FALSE, ""},
    {"ubound8", "", FALSE, ""},
    {"ubound", "", TRUE, "k"},
    {"ubounda1", "", TRUE, ""},
    {"ubounda2", "", TRUE, ""},
    {"ubounda4", "", TRUE, ""},
    {"ubounda8", "", TRUE, ""},
    {"ubounda", "", TRUE, "k"},
    {"uboundaz1", "", TRUE, ""},
    {"uboundaz2", "", TRUE, ""},
    {"uboundaz4", "", TRUE, ""},
    {"uboundaz8", "", TRUE, ""},
    {"uboundaz", "", TRUE, "k"},
    {"unpack", "", TRUE, ""},
    {"unpackc", "", TRUE, ""},
    {"END_OF_PFX_FTN", "", FALSE, ""},
    {"f90io_aux_init", "", FALSE, ""},
    {"f90io_backspace", "", FALSE, ""},
    {"f90io_begin", "", FALSE, ""},
    {"f90io_byte_read", "", FALSE, ""},
    {"f90io_byte_read64", "", FALSE, ""},
    {"f90io_byte_write", "", FALSE, ""},
    {"f90io_byte_write64", "", FALSE, ""},
    {"f90io_close", "", FALSE, ""},
    {"f90io_dts_fmtr", "", FALSE, ""},
    {"f90io_dts_fmtw", "", FALSE, ""},
    {"f90io_encode_fmt", "", FALSE, ""},
    {"f90io_encode_fmtv", "", FALSE, ""},
    {"f90io_end", "", FALSE, ""},
    {"f90io_endfile", "", FALSE, ""},
    {"f90io_flush", "", FALSE, ""},
    {"f90io_fmt_read", "", FALSE, ""},
    {"f90io_fmt_read64_a", "", FALSE, ""},
    {"f90io_fmt_read_a", "", FALSE, ""},
    {"f90io_fmt_write", "", FALSE, ""},
    {"f90io_fmt_write64_a", "", FALSE, ""},
    {"f90io_fmt_write_a", "", FALSE, ""},
    {"f90io_fmtr_end", "", FALSE, ""},
    {"f90io_fmtr_init", "", FALSE, ""},
    {"f90io_fmtr_init03", "", FALSE, ""},
    {"f90io_fmtr_init2003", "", FALSE, ""},
    {"f90io_fmtr_initv", "", FALSE, ""},
    {"f90io_fmtr_initv2003", "", FALSE, ""},
    {"f90io_fmtr_intern_init", "", FALSE, ""},
    {"f90io_fmtr_intern_inite", "", FALSE, ""},
    {"f90io_fmtr_intern_initev", "", FALSE, ""},
    {"f90io_fmtr_intern_initv", "", FALSE, ""},
    {"f90io_fmtw_end", "", FALSE, ""},
    {"f90io_fmtw_init", "", FALSE, ""},
    {"f90io_fmtw_init03", "", FALSE, ""},
    {"f90io_fmtw_initv", "", FALSE, ""},
    {"f90io_fmtw_intern_init", "", FALSE, ""},
    {"f90io_fmtw_intern_inite", "", FALSE, ""},
    {"f90io_fmtw_intern_initev", "", FALSE, ""},
    {"f90io_fmtw_intern_initv", "", FALSE, ""},
    {"f90io_get_newunit", "", FALSE, ""},
    {"f90io_inquire", "", FALSE, ""},
    {"f90io_inquire03", "", FALSE, ""},
    {"f90io_inquire03_2", "", FALSE, ""},
    {"f90io_inquire2003", "", FALSE, ""},
    {"f90io_iomsg", "", FALSE, ""},
    {"f90io_iomsg_", "", FALSE, ""},
    {"f90io_ldr", "", FALSE, ""},
    {"f90io_ldr64_a", "", FALSE, ""},
    {"f90io_ldr_a", "", FALSE, ""},
    {"f90io_ldr_end", "", FALSE, ""},
    {"f90io_ldr_init", "", FALSE, ""},
    {"f90io_ldr_init03", "", FALSE, ""},
    {"f90io_ldr_intern_init", "", FALSE, ""},
    {"f90io_ldr_intern_inite", "", FALSE, ""},
    {"f90io_ldw", "", FALSE, ""},
    {"f90io_ldw64_a", "", FALSE, ""},
    {"f90io_ldw_a", "", FALSE, ""},
    {"f90io_ldw_end", "", FALSE, ""},
    {"f90io_ldw_init", "", FALSE, ""},
    {"f90io_ldw_init03", "", FALSE, ""},
    {"f90io_ldw_intern_init", "", FALSE, ""},
    {"f90io_ldw_intern_inite", "", FALSE, ""},
    {"f90io_nml_read", "", TRUE, ""},
    {"f90io_nml_write", "", TRUE, ""},
    {"f90io_nmlr", "", TRUE, ""},
    {"f90io_nmlr_end", "", TRUE, ""},
    {"f90io_nmlr_init", "", TRUE, ""},
    {"f90io_nmlr_init03", "", TRUE, ""},
    {"f90io_nmlr_intern_init", "", TRUE, ""},
    {"f90io_nmlw", "", TRUE, ""},
    {"f90io_nmlw_end", "", TRUE, ""},
    {"f90io_nmlw_init", "", TRUE, ""},
    {"f90io_nmlw_init03", "", TRUE, ""},
    {"f90io_nmlw_intern_init", "", TRUE, ""},
    {"f90io_open03", "", FALSE, ""},
    {"f90io_open2003", "", FALSE, ""},
    {"f90io_open_async", "", FALSE, ""},
    {"f90io_open_cvt", "", FALSE, ""},
    {"f90io_open_share", "", FALSE, ""},
    {"f90io_print_init", "", FALSE, ""},
    {"f90io_rewind", "", FALSE, ""},
    {"f90io_sc_cd_fmt_write", "", FALSE, ""},
    {"f90io_sc_cd_ldw", "", FALSE, ""},
    {"f90io_sc_cf_fmt_write", "", FALSE, ""},
    {"f90io_sc_cf_ldw", "", FALSE, ""},
    {"f90io_sc_ch_fmt_write", "", FALSE, ""},
    {"f90io_sc_ch_ldw", "", FALSE, ""},
    {"f90io_sc_d_fmt_write", "", FALSE, ""},
    {"f90io_sc_d_ldw", "", FALSE, ""},
    {"f90io_sc_f_fmt_write", "", FALSE, ""},
    {"f90io_sc_f_ldw", "", FALSE, ""},
    {"f90io_sc_fmt_write", "", FALSE, ""},
    {"f90io_sc_i_fmt_write", "", FALSE, ""},
    {"f90io_sc_i_ldw", "", FALSE, ""},
    {"f90io_sc_l_fmt_write", "", FALSE, ""},
    {"f90io_sc_l_ldw", "", FALSE, ""},
    {"f90io_sc_ldw", "", FALSE, ""},
    {"f90io_src_info03", "", FALSE, ""},
    {"f90io_src_infox03", "", FALSE, ""},
    {"f90io_swbackspace", "", FALSE, ""},
    {"f90io_unf_async", "", FALSE, ""},
    {"f90io_unf_end", "", FALSE, ""},
    {"f90io_unf_init", "", FALSE, ""},
    {"f90io_unf_read", "", FALSE, ""},
    {"f90io_unf_read64_a", "", FALSE, ""},
    {"f90io_unf_read_a", "", FALSE, ""},
    {"f90io_unf_write", "", FALSE, ""},
    {"f90io_unf_write64_a", "", FALSE, ""},
    {"f90io_unf_write_a", "", FALSE, ""},
    {"f90io_usw_end", "", FALSE, ""},
    {"f90io_usw_init", "", FALSE, ""},
    {"f90io_usw_read", "", FALSE, ""},
    {"f90io_usw_read64_a", "", FALSE, ""},
    {"f90io_usw_read_a", "", FALSE, ""},
    {"f90io_usw_write", "", FALSE, ""},
    {"f90io_usw_write64_a", "", FALSE, ""},
    {"f90io_usw_write_a", "", FALSE, ""},
    {"f90io_wait", "", FALSE, ""},
    {"END_OF_IO", "", FALSE, ""},
    {"io_fmt_read", "", FALSE, ""},
    {"io_fmt_read64", "", FALSE, ""},
    {"io_fmt_write", "", FALSE, ""},
    {"io_fmt_write64", "", FALSE, ""},
    {"io_ldr", "", FALSE, ""},
    {"io_ldr64", "", FALSE, ""},
    {"io_ldw", "", FALSE, ""},
    {"io_ldw64", "", FALSE, ""},
    {"io_unf_read", "", FALSE, ""},
    {"io_unf_read64", "", FALSE, ""},
    {"io_unf_write", "", FALSE, ""},
    {"io_unf_write64", "", FALSE, ""},
    {"io_usw_read", "", FALSE, ""},
    {"io_usw_read64", "", FALSE, ""},
    {"io_usw_write", "", FALSE, ""},
    {"io_usw_write64", "", FALSE, ""},
    {"END_OF_FTNIO", "", FALSE, ""},
};

#ifdef DEBUG
void
dump_FtnRteRtn(FtnRtlEnum rteRtn)
{
  fprintf(stderr, "ftnRtlRtns[%d]:\n", rteRtn);
  fprintf(stderr, "  baseNm: %s\n", ftnRtlRtns[rteRtn].baseNm);
  fprintf(stderr, "  I8Descr: %s\n",
          ftnRtlRtns[rteRtn].I8Descr ? "TRUE" : "FALSE");
  if (strlen(ftnRtlRtns[rteRtn].largeRetValPrefix) != 0) {
    fprintf(stderr, "  largeRetValPrefix: %s\n",
            ftnRtlRtns[rteRtn].largeRetValPrefix);
  } else {
    fprintf(stderr, "  largeRetValPrefix: NULL\n");
  }
  if (strlen(ftnRtlRtns[rteRtn].fullNm) != 0) {
    fprintf(stderr, "  fullNm: %s\n", ftnRtlRtns[rteRtn].fullNm);
  } else {
    fprintf(stderr, "  fullNm: NULL\n");
  }
}
#endif

/** \brief given a FtnRtlEnum, return the RTL routine name */
char *
mkRteRtnNm(FtnRtlEnum rteRtn)
{
  assert( strcmp(ftnRtlRtns[END_OF_FTNIO].baseNm , "END_OF_FTNIO") == 0,
          "mkRteRtnNm: RTL name table and RTL name enum are out of sync",
          rteRtn, 3);
  assert(rteRtn > RTE_no_rtn && rteRtn < END_OF_FTNIO,
         "mkRteRtnNm: invalid rteRtn enum", rteRtn, 3);

  if (strlen(ftnRtlRtns[rteRtn].fullNm) == 0) {
    if (rteRtn < END_OF_PFX_F90) {
      strcat(ftnRtlRtns[rteRtn].fullNm, "f90_");
    } else if (rteRtn > END_OF_PFX_F90 && rteRtn < END_OF_PFX_FTN) {
      strcat(ftnRtlRtns[rteRtn].fullNm, "fort_");
    } else if (rteRtn > END_OF_PFX_FTN && rteRtn < END_OF_IO) {
      ftnRtlRtns[rteRtn].fullNm[0] = '\0'; /*no prefix */
    } else if (rteRtn > END_OF_IO && rteRtn < END_OF_FTNIO) {
      strcat(ftnRtlRtns[rteRtn].fullNm, "ftn");
    }

    if (XBIT(124, 0x10) &&
        strncmp("k", ftnRtlRtns[rteRtn].largeRetValPrefix, 1) == 0) {
      strcat(ftnRtlRtns[rteRtn].fullNm, ftnRtlRtns[rteRtn].largeRetValPrefix);
    }

    /* FIXME: what about (XBIT(68, 0x1) && XBIT(68, 0x2), see semfunc.c,
     *        transfrm.c
     */
    if (XBIT(124, 0x8) &&
        (strncmp("d", ftnRtlRtns[rteRtn].largeRetValPrefix, 1) == 0 ||
         strncmp("cd", ftnRtlRtns[rteRtn].largeRetValPrefix, 2) == 0)) {
      strcat(ftnRtlRtns[rteRtn].fullNm, ftnRtlRtns[rteRtn].largeRetValPrefix);
    }

    strcat(ftnRtlRtns[rteRtn].fullNm, ftnRtlRtns[rteRtn].baseNm);
  }
  assert(strlen(ftnRtlRtns[rteRtn].fullNm) > 0,
         "mkRteRtnNm: return NULL name\n", rteRtn, 3);
  return ftnRtlRtns[rteRtn].fullNm;
}

static void
stripI8DescrSuffix(char *inNm, char *outNm)
{
  int nmLen = strlen(inNm);

  if (nmLen <= 3) {
    outNm[0] = '\0';
    return;
  }

  assert(nmLen < MAXIDLEN, "stripI8DescrSuffix: name too big", nmLen, 3);

  if (XBIT(68, 0x1)) {
    nmLen -= 3; /* strip "_i8" */
  }
  strncpy(outNm, inNm, nmLen);
  outNm[nmLen] = '\0';
}

typedef struct {
  FtnRtlEnum rtlRtn;
  int ftype;
} F90TmplSectRtns;

static F90TmplSectRtns f90TmplSectRtns[] = {
    {RTE_sect, FTYPE_SECT},           {RTE_sect1, FTYPE_SECT},
    {RTE_sect1v, FTYPE_SECT},         {RTE_sect2, FTYPE_SECT},
    {RTE_sect2v, FTYPE_SECT},         {RTE_sect3, FTYPE_SECT},
    {RTE_sect3v, FTYPE_SECT},         {RTE_template, FTYPE_TEMPLATE},
    {RTE_template1, FTYPE_TEMPLATE1}, {RTE_template1v, FTYPE_TEMPLATE1V},
    {RTE_template2, FTYPE_TEMPLATE2}, {RTE_template2v, FTYPE_TEMPLATE2V},
    {RTE_template3, FTYPE_TEMPLATE3}, {RTE_template3v, FTYPE_TEMPLATE3V},
};

static int
setTmplSectRtnFtype(int i)
{
  int retFtype;

  retFtype = f90TmplSectRtns[i].ftype;
  /* assert ftnRtlRtns[i].I8Descr == TRUE */
  if ((XBIT(68, 0x1))) {
    retFtype |= FTYPE_I8;
  }
  return retFtype;
}

int
getF90TmplSectRtn(char *rtnNm)
{
  int l, h, m, r;
  char *tmplSectNm;
  int retFtype = 0;
  int compLen;
  char cpyRtnNm[MAXIDLEN];
  char cpyTmplSectNm[MAXIDLEN];

  stripI8DescrSuffix(rtnNm, cpyRtnNm);
  if (cpyRtnNm[0] == '\0') {
    /* name too short to be a RTL descriptor rtn */
    return 0;
  }

  l = 0;
  h = sizeof(f90TmplSectRtns) / sizeof(F90TmplSectRtns) - 1;

  /* The majority of this function's invocations will return 0,
   * check if routine names are are out of bounds
   */
  tmplSectNm = mkRteRtnNm(f90TmplSectRtns[l].rtlRtn);
  stripI8DescrSuffix(tmplSectNm, cpyTmplSectNm);
  r = strcmp(cpyRtnNm, cpyTmplSectNm);
  if (r == 0) {
    return setTmplSectRtnFtype(l);
  } else if (r < 0)
    return 0;

  tmplSectNm = mkRteRtnNm(f90TmplSectRtns[h].rtlRtn);
  stripI8DescrSuffix(tmplSectNm, cpyTmplSectNm);
  r = strcmp(cpyRtnNm, cpyTmplSectNm);
  if (r == 0) {
    return setTmplSectRtnFtype(h);
  } else if (r > 0)
    return 0;

  while (l <= h) {
    m = (h + l) / 2;

    tmplSectNm = mkRteRtnNm(f90TmplSectRtns[m].rtlRtn);
    stripI8DescrSuffix(tmplSectNm, cpyTmplSectNm);

    r = strcmp(cpyRtnNm, cpyTmplSectNm);
    if (r == 0) {
      retFtype = setTmplSectRtnFtype(m);
      break;
    }
    if (r < 0) {
      h = m - 1;
    } else if (r > 0) {
      l = m + 1;
    }
  }

  return retFtype;
}

