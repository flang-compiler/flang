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
 */

/** \file
 * \brief Matrix multiplication routines
 */

void f90_mm_cplx16_str1_mxv_(__CPLX16_T *, __CPLX16_T *, __CPLX16_T *,
                               __INT_T *, __INT_T *, __INT_T *, __INT_T *);
void f90_mm_cplx16_str1_vxm_(__CPLX16_T *, __CPLX16_T *, __CPLX16_T *,
                               __INT_T *, __INT_T *, __INT_T *, __INT_T *);
void f90_mm_cplx16_str1_(__CPLX16_T *, __CPLX16_T *, __CPLX16_T *, __INT_T *,
                           __INT_T *, __INT_T *, __INT_T *, __INT_T *,
                           __INT_T *, __INT_T *);
void f90_mm_cplx16_str1_mxv_t_(__CPLX16_T *, __CPLX16_T *, __CPLX16_T *,
                                 __INT_T *, __INT_T *, __INT_T *, __INT_T *);

void f90_mm_cplx8_str1_mxv_(__CPLX8_T *, __CPLX8_T *, __CPLX8_T *, __INT_T *,
                              __INT_T *, __INT_T *, __INT_T *);
void f90_mm_cplx8_str1_vxm_(__CPLX8_T *, __CPLX8_T *, __CPLX8_T *, __INT_T *,
                              __INT_T *, __INT_T *, __INT_T *);
void f90_mm_cplx8_str1_(__CPLX8_T *, __CPLX8_T *, __CPLX8_T *, __INT_T *,
                          __INT_T *, __INT_T *, __INT_T *, __INT_T *, __INT_T *,
                          __INT_T *);
void f90_mm_cplx8_str1_mxv_t_(__CPLX8_T *, __CPLX8_T *, __CPLX8_T *,
                                __INT_T *, __INT_T *, __INT_T *, __INT_T *);

void f90_mm_int1_str1_mxv_(__INT1_T *, __INT1_T *, __INT1_T *, __INT_T *,
                             __INT_T *, __INT_T *, __INT_T *);
void f90_mm_int1_str1_vxm_(__INT1_T *, __INT1_T *, __INT1_T *, __INT_T *,
                             __INT_T *, __INT_T *, __INT_T *);
void f90_mm_int1_str1_(__INT1_T *, __INT1_T *, __INT1_T *, __INT_T *,
                         __INT_T *, __INT_T *, __INT_T *, __INT_T *, __INT_T *,
                         __INT_T *);

void f90_mm_int2_str1_mxv_(__INT2_T *, __INT2_T *, __INT2_T *, __INT_T *,
                             __INT_T *, __INT_T *, __INT_T *);
void f90_mm_int2_str1_vxm_(__INT2_T *, __INT2_T *, __INT2_T *, __INT_T *,
                             __INT_T *, __INT_T *, __INT_T *);
void f90_mm_int2_str1_(__INT2_T *, __INT2_T *, __INT2_T *, __INT_T *,
                         __INT_T *, __INT_T *, __INT_T *, __INT_T *, __INT_T *,
                         __INT_T *);

void f90_mm_int4_str1_mxv_(__INT4_T *, __INT4_T *, __INT4_T *, __INT_T *,
                             __INT_T *, __INT_T *, __INT_T *);
void f90_mm_int4_str1_vxm_(__INT4_T *, __INT4_T *, __INT4_T *, __INT_T *,
                             __INT_T *, __INT_T *, __INT_T *);
void f90_mm_int4_str1_(__INT4_T *, __INT4_T *, __INT4_T *, __INT_T *,
                         __INT_T *, __INT_T *, __INT_T *, __INT_T *, __INT_T *,
                         __INT_T *);

void f90_mm_int8_str1_mxv_(__INT8_T *, __INT8_T *, __INT8_T *, __INT_T *,
                             __INT_T *, __INT_T *, __INT_T *);
void f90_mm_int8_str1_vxm_(__INT8_T *, __INT8_T *, __INT8_T *, __INT_T *,
                             __INT_T *, __INT_T *, __INT_T *);
void f90_mm_int8_str1_(__INT8_T *, __INT8_T *, __INT8_T *, __INT_T *,
                         __INT_T *, __INT_T *, __INT_T *, __INT_T *, __INT_T *,
                         __INT_T *);

void f90_mm_real4_str1_mxv_(__REAL4_T *, __REAL4_T *, __REAL4_T *, __INT_T *,
                              __INT_T *, __INT_T *, __INT_T *);
void f90_mm_real4_str1_vxm_(__REAL4_T *, __REAL4_T *, __REAL4_T *, __INT_T *,
                              __INT_T *, __INT_T *, __INT_T *);
void f90_mm_real4_str1_(__REAL4_T *, __REAL4_T *, __REAL4_T *, __INT_T *,
                          __INT_T *, __INT_T *, __INT_T *, __INT_T *, __INT_T *,
                          __INT_T *);
void f90_mm_real4_str1_mxv_t_(__REAL4_T *, __REAL4_T *, __REAL4_T *,
                                __INT_T *, __INT_T *, __INT_T *, __INT_T *);

void f90_mm_real8_str1_mxv_(__REAL8_T *, __REAL8_T *, __REAL8_T *, __INT_T *,
                              __INT_T *, __INT_T *, __INT_T *);
void f90_mm_real8_str1_vxm_(__REAL8_T *, __REAL8_T *, __REAL8_T *, __INT_T *,
                              __INT_T *, __INT_T *, __INT_T *);
void f90_mm_real8_str1_(__REAL8_T *, __REAL8_T *, __REAL8_T *, __INT_T *,
                          __INT_T *, __INT_T *, __INT_T *, __INT_T *, __INT_T *,
                          __INT_T *);
void f90_mm_real8_str1_mxv_t_(__REAL8_T *, __REAL8_T *, __REAL8_T *,
                                __INT_T *, __INT_T *, __INT_T *, __INT_T *);
