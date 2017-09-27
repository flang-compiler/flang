/*
 * Copyright (c) 2000, NVIDIA CORPORATION.  All rights reserved.
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
 * Part of the f2008 complex tanh intrinsic test
 */

#include <stdio.h>
#include <complex.h>

extern float complex ctanhf(float complex);
extern double complex ctanh(double complex);

void
get_expected_cf(complex float src1[], complex float expct[], int n)
{
    int i;

    for(i= 0; i <n; i++ ) {
        expct[i] = ctanh(src1[i]);
        /*printf("%d) ynf(%e) = %e\n", i, src1[i], expct[i]);*/
    }
}

void
get_expected_cd(complex double src1[], complex double expct[], int n)
{
    int i;

    for(i= 0; i <n; i++ ) {
        expct[i] = ctanh(src1[i]);
        /*printf("%d) yn(%e) = %e\n", i, src1[i], expct[i]);*/
    }
}
