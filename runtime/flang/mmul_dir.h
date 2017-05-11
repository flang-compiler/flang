! 
! Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
! 


! directives.h -- contains preprocessor directives for F90 rte files
#define DESC_INT INTEGER(4)


#define F90_matmul_cplx16_contmxm	f90_mm_cplx16_contmxm
#define F90_matmul_cplx16_contmxv	f90_mm_cplx16_contmxv
#define F90_matmul_cplx16_contvxm	f90_mm_cplx16_contvxm
#define F90_matmul_cplx16_str1		f90_mm_cplx16_str1
#define F90_matmul_cplx16_str1_mxv	f90_mm_cplx16_str1_mxv
#define F90_matmul_cplx16_str1_mxv_t	f90_mm_cplx16_str1_mxv_t
#define F90_matmul_cplx16_str1_vxm	f90_mm_cplx16_str1_vxm
#define F90_matmul_cplx32_contmxm	f90_mm_cplx32_contmxm
#define F90_matmul_cplx32_contmxv	f90_mm_cplx32_contmxv
#define F90_matmul_cplx32_contvxm	f90_mm_cplx32_contvxm
#define F90_matmul_cplx32_str1		f90_mm_cplx32_str1
#define F90_matmul_cplx32_str1_mxv	f90_mm_cplx32_str1_mxv
#define F90_matmul_cplx8_contmxm	f90_mm_cplx8_contmxm
#define F90_matmul_cplx8_contmxv	f90_mm_cplx8_contmxv
#define F90_matmul_cplx8_contvxm	f90_mm_cplx8_contvxm
#define F90_matmul_cplx8_str1		f90_mm_cplx8_str1
#define F90_matmul_cplx8_str1_mxv	f90_mm_cplx8_str1_mxv
#define F90_matmul_cplx8_str1_mxv_t	f90_mm_cplx8_str1_mxv_t
#define F90_matmul_cplx8_str1_t		f90_mm_cplx8_str1_t
#define F90_matmul_cplx8_str1_vxm	f90_mm_cplx8_str1_vxm
#define F90_matmul_int1_contmxm		f90_mm_int1_contmxm
#define F90_matmul_int1_contmxv		f90_mm_int1_contmxv
#define F90_matmul_int1_contvxm		f90_mm_int1_contvxm
#define F90_matmul_int1_str1		f90_mm_int1_str1
#define F90_matmul_int1_str1_mxv	f90_mm_int1_str1_mxv
#define F90_matmul_int1_str1_vxm	f90_mm_int1_str1_vxm
#define F90_matmul_int2_contmxm		f90_mm_int2_contmxm
#define F90_matmul_int2_contmxv		f90_mm_int2_contmxv
#define F90_matmul_int2_contvxm		f90_mm_int2_contvxm
#define F90_matmul_int2_str1		f90_mm_int2_str1
#define F90_matmul_int2_str1_mxv	f90_mm_int2_str1_mxv
#define F90_matmul_int2_str1_vxm	f90_mm_int2_str1_vxm
#define F90_matmul_int4_contmxm		f90_mm_int4_contmxm
#define F90_matmul_int4_contmxv		f90_mm_int4_contmxv
#define F90_matmul_int4_contvxm		f90_mm_int4_contvxm
#define F90_matmul_int4_str1		f90_mm_int4_str1
#define F90_matmul_int4_str1_mxv	f90_mm_int4_str1_mxv
#define F90_matmul_int4_str1_vxm	f90_mm_int4_str1_vxm
#define F90_matmul_int8_contmxm		f90_mm_int8_contmxm
#define F90_matmul_int8_contmxv		f90_mm_int8_contmxv
#define F90_matmul_int8_contvxm		f90_mm_int8_contvxm
#define F90_matmul_int8_str1		f90_mm_int8_str1
#define F90_matmul_int8_str1_mxv	f90_mm_int8_str1_mxv
#define F90_matmul_int8_str1_vxm	f90_mm_int8_str1_vxm
#define F90_matmul_log1_contmxm		f90_mm_log1_contmxm
#define F90_matmul_log1_contmxv		f90_mm_log1_contmxv
#define F90_matmul_log1_contvxm		f90_mm_log1_contvxm
#define F90_matmul_log2_contmxm		f90_mm_log2_contmxm
#define F90_matmul_log2_contmxv		f90_mm_log2_contmxv
#define F90_matmul_log2_contvxm		f90_mm_log2_contvxm
#define F90_matmul_log4_contmxm		f90_mm_log4_contmxm
#define F90_matmul_log4_contmxv		f90_mm_log4_contmxv
#define F90_matmul_log4_contvxm		f90_mm_log4_contvxm
#define F90_matmul_log8_contmxm		f90_mm_log8_contmxm
#define F90_matmul_log8_contmxv		f90_mm_log8_contmxv
#define F90_matmul_log8_contvxm		f90_mm_log8_contvxm
#define F90_matmul_real16_contmxm	f90_mm_real16_contmxm
#define F90_matmul_real16_contmxv	f90_mm_real16_contmxv
#define F90_matmul_real16_contvxm	f90_mm_real16_contvxm
#define F90_matmul_real4_contmxm	f90_mm_real4_contmxm
#define F90_matmul_real4_contmxv	f90_mm_real4_contmxv
#define F90_matmul_real4_contvxm	f90_mm_real4_contvxm
#define F90_matmul_real4_str1		f90_mm_real4_str1
#define F90_matmul_real4_str1_mxv	f90_mm_real4_str1_mxv
#define F90_matmul_real4_str1_mxv_t	f90_mm_real4_str1_mxv_t
#define F90_matmul_real4_str1_t		f90_mm_real4_str1_t
#define F90_matmul_real4_str1_vxm	f90_mm_real4_str1_vxm
#define F90_matmul_real8_contmxm	f90_mm_real8_contmxm
#define F90_matmul_real8_contmxv	f90_mm_real8_contmxv
#define F90_matmul_real8_contvxm	f90_mm_real8_contvxm
#define F90_matmul_real8_str1		f90_mm_real8_str1
#define _F90_matmul_real8_str1a		_f90_mm_real8_str1a
#define _F90_matmul_real8_str1b		_f90_mm_real8_str1b
#define _F90_matmul_real8_str1c		_f90_mm_real8_str1c
#define F90_matmul_real8_str1_mxv	f90_mm_real8_str1_mxv
#define F90_matmul_real8_str1_mxv_t	f90_mm_real8_str1_mxv_t
#define F90_matmul_real8_str1_vxm	f90_mm_real8_str1_vxm

