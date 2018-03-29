/*
 * Copyright (c) 2017-2018, NVIDIA CORPORATION.  All rights reserved.
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

/*
 * These functions are used in returning two vector arguments from a
 * single function.
 *
 * In particular, the sincos() function returns two results, SIN(x), and
 * COS(x).  The compiler expects the return values to be in (x86-64) x/ymm0
 * and x/ymm1.  There is no way with C to return those two values in those
 * two registers without some trickery.
 *
 * Given a function of the form:
 *
 * VectorType
 * FunctionNameReturning2Vectors(vector_type x)
 * {
 *   VectorType sine;
 *   VectorType cossine;
 *   return (sine,cosine); <------------ Will not work.
 * }
 *
 * But, because the our compiler ABI uses the same vector registers
 * to return the two registers as the x86-64 calling sequence ABI,
 * we can call a dummy function with those two "return" values as arguments
 * and then do nothing in the dummy function.
 *
 * This will work as long as the caller does nothing after calling the
 * dummy function.
 *
 * Now function FunctionNameReturning2Vectors becomes:
 *
 * extern VectorType return2VectorType(VectorType, VectorType);
 *
 * VectorType
 * FunctionNameReturning2Vectors(vector_type x)
 * {
 *   VectorType sine;
 *   VectorType cossine;
 *   return (return2VectorType(sine,cosine));
 * }
 *
 */

void
__mth_return2vectors(void)
{
    return;
}

#ifndef TARGET_OSX_X8664
/*
 * OSX does not support weak aliases - so just use the generic for all
 * vector types.
 */

#define ALIAS(altname) \
    void    __mth_return2##altname(void) \
        __attribute__ ((weak, alias ("__mth_return2vectors")));

ALIAS(vrs4_t)
ALIAS(vrd2_t)
ALIAS(vrs8_t)
ALIAS(vrd4_t)
ALIAS(vrs16_t)
ALIAS(vrd8_t)
#endif
