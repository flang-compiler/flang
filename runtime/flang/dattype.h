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
 * \file
 * \brief dattype.h - definitions of symbols for data types.
 */

#define TY_WORD 1
#define TY_DWORD 2
#define TY_HOLL 3
#define TY_BINT 4
#define TY_SINT 5
#define TY_INT 6
#define TY_REAL 7
#define TY_DBLE 8
#define TY_QUAD 9
#define TY_CMPLX 10
#define TY_DCMPLX 11
#define TY_BLOG 12
#define TY_SLOG 13
#define TY_LOG 14
#define TY_CHAR 15
#define TY_NCHAR 16
#define TY_INT8 17
#define TY_LOG8 18

#define Is_complex(parm) ((parm) == TY_CMPLX || (parm) == TY_DCMPLX)
#define Is_real(parm) ((parm) == TY_REAL || (parm) == TY_DBLE)

#define REAL_ALLOWED(param) ((Is_complex(param)) || Is_real(param))
