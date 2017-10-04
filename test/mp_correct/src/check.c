/*
 * Copyright (c) 2015, NVIDIA CORPORATION.  All rights reserved.
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

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

void
check_(int * res, int * exp, int * np)
{
    int i;
    int n = *np;
    int tests_passed = 0;
    int tests_failed = 0;

    for (i = 0; i < n; i++) {
        if (exp[i] == res[i]) {
	    tests_passed ++;
        } else {
            tests_failed ++;
            printf(
	    "---- test number %d FAILED. res %d(%08x)  exp %d(%08x)\n",
	     i+1, res[i], res[i], exp[i], exp[i] );
        }
    }
    if (tests_failed == 0)
	printf(
	"---- %3d tests completed. %d tests PASSED. %d tests failed.\n",
                      n, tests_passed, tests_failed);
    else
	printf("---- %3d tests completed. %d tests passed. %d tests FAILED.\n",
                      n, tests_passed, tests_failed);
}

void
check(int * res, int * exp, int * np)
{
    check_(res, exp, np);
}

int
chkalt_(int * ir, int * ie, int * np)
{
    int i;
    int n = *np;
    for (i = 0; i < n; i++) {
	if (ir[i] != ie[i])
	    return 0;
    }
    return 1;
}

int
chkalt(int * ir, int * ie, int * np)
{
    return chkalt_(ir, ie, np);
}

#if defined(WINNT) || defined(WIN32)
void
__stdcall CHECK(res, exp, np)
    int *res, *exp, *np;
{
    check_(res, exp, np);
}

int __stdcall CHKALT (int *ir, int *ie, int *np) {
    return chkalt_(ir, ie, np);
}
#endif
#ifdef __cplusplus
}
#endif
