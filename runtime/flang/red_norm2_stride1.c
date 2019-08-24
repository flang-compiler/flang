/*
 * Copyright (c) 2019, NVIDIA CORPORATION.  All rights reserved.
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

/* red_norm2_stride1.c -- intrinsic reduction function */

#include "norm2.h"
#include <math.h>

void NORM2_REAL4 (__POINT_T *src_pointer, __INT_T *size, __REAL4_T *result) {
  //  Passing in integer*8 address of starting point in the array
  __REAL4_T *src = (__REAL4_T *)(*src_pointer);
  __REAL8_T sum = 0, val;
  __INT_T i;

  for (i = 0; i < *size; ++i) {
    val = (__REAL8_T) src[i];
    sum += val*val;
  }
  val = sqrt(sum);
  *result = (__REAL4_T) val;
}

void NORM2_REAL8 (__POINT_T *src_pointer, __INT_T *size, __REAL8_T *result) {
  // Passing in integer*8 address of starting point in the array
  __REAL8_T *src = (__REAL8_T *)(*src_pointer);
  __REAL8_T sum = 0;
  __INT_T i;

  for (i = 0; i < *size; ++i) {
    sum += src[i]*src[i];
  }
  *result = sqrt(sum);
}
