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
 * Part of the f2008 complex asin intrinsic test
 */

#include <stdio.h>
#include <complex.h>

extern float complex casinf(float complex);
extern double complex casin(double complex);

#undef USELIBMF
#if !defined(__WIN32) && !defined(__WIN64) && defined(__PGIC__) && defined(__PGIC_MINOR__)
#if (__PGIC__ > 15)
#define USELIBMF
#elif (__PGIC__ == 15) && (__PGIC_MINOR_ > 4)
#define USELIBMF
#endif
#endif

void
get_expected_cf(complex float src1[], complex float expct[], int n)
{
    int i;

    for(i= 0; i <n; i++ ) {
#ifdef USELIBMF
        expct[i] = casinf(src1[i]);
#else
        expct[i] = casin(src1[i]);
#endif
        /*printf("%d) ynf(%e) = %e\n", i, src1[i], expct[i]);*/
    }
}

void
get_expected_cd(complex double src1[], complex double expct[], int n)
{
    int i;

    for(i= 0; i <n; i++ ) {
        expct[i] = casin(src1[i]);
        /*printf("%d) yn(%e) = %e\n", i, src1[i], expct[i]);*/
    }
}
