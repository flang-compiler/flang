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
 * Part of the f2008 erfc intrinsic test
 */

#include <stdio.h>
#include <math.h>

void
get_expected_f(float src[], float expct[], int n)
{
    int i;

    for(i= 0; i <n; i++ ) {
#ifdef  WIN64
        expct[i] = erfc(src[i]);
#else
        expct[i] = erfcf(src[i]);
#endif
        /*printf("%d) erfcf(%e) = %e\n",i, src[i], expct[i]);*/
    }
}

void
get_expected_d(double src[], double expct[], int n)
{
    int i;

    for(i= 0; i <n; i++ ) {
        expct[i] = erfc(src[i]);
        /*printf("%d) erfc(%e) = %e\n",i, src[i], expct[i]);*/
    }
}
