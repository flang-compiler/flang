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
 * \brief Definitions for FORTRAN edit descriptor symbolics
 *
 * If contents are changed, changes must also be applied to the run-time
 * document file (fio.n) and this file must copied to the io source rte
 * directory.
 */

typedef enum {
  __BYTE = 0,   /* (byte) */
  __WORD = 1,   /* typeless */
  __DWORD = 2,  /* double typeless */
  __HOLL = 3,   /* hollerith */
  __BINT = 4,   /* byte integer */
  __SINT = 5,   /* short integer */
  __INT = 6,    /* integer*4 */
  __REAL = 7,   /* real */
  __DBLE = 8,   /* real*8 */
  __QUAD = 9,   /* real*16*/
  __CPLX = 10,  /* (real complex) */
  __DCPLX = 11, /* (real*8 complex) */
  __BLOG = 12,  /* byte logical */
  __SLOG = 13,  /* short logical */
  __LOG = 14,   /* logical */
  __CHAR = 15,  /* signed char */
  __NCHAR = 16, /* ncharacter - kanji */
  __INT8 = 17,  /* integer*8 */
  __LOG8 = 18   /* logical*8 */
} _pgfio_type;
