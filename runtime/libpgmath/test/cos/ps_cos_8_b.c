
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

// RUN: %libpgmath-compile -DMAX_VREG_SIZE=256 && %libpgmath-run

#define FUNC cos
#define FRP p
#define PREC s
#define VL 8
#define TOL 0.00001f

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>
#if !defined(TARGET_WIN_X8664)
#include <unistd.h>
#endif

#include "pgmath_test.h"

int main(int argc, char *argv[])
{
	VRS_T expd_res = {0.98614f, 0.98981f, 0.99220f, 0.99383f, 0.99500f, 0.99587f, 0.99653f, 0.99704f };
  VIS_T vmask __attribute__((aligned(64))) = {-1, -1, -1, -1, -1, -1, -1, -1};
#if !defined(TARGET_WIN_X8664)
	parseargs(argc, argv);
#endif

#include "single1.h"

}

// UNSUPPORTED: sse4
// UNSUPPORTED: em64t
// UNSUPPORTED: ppc64le
