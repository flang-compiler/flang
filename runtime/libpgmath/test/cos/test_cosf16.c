
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
 */

// RUN: %libpgm-compile -Wno-aggressive-loop-optimizations -DMAX_VREG_SIZE=512 && %libpgm-run

#define FUNC cos
#define FRP f
#define PREC s
#define VL 16
#define TOL 0.00001f

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "pgmath_test.h"

int main()
{
  VRS_T expd_res = { 0.99745f, 0.99778f, 0.99805f, 0.99827f, 0.99846f, 0.99862f, 0.99875f, 0.99887f, 0.99897f, 0.99905f, 0.99913f, 0.99920f, 0.99926f, 0.99931f, 0.99936f, 0.99941f };

#include "single1.h"
}

// UNSUPPORTED: avx
// UNSUPPORTED: avx2
// XFAIL: ppc64le
