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

#if DEBUG

/** \brief VERIFY_LEVEL specifies the depth to which to verify ILI.

    Deeper levels require more time.  A good strategy is to choose
    a depth for which the verification time can be amortized against
    compiler work that have happened since the last verification. */
typedef enum VERIFY_LEVEL {
  VERIFY_BLOCK,       /**< Verify down to blocks, but no deeper. */
  VERIFY_ILT,         /**< Verify down to ILTs. */
  VERIFY_ILI_SHALLOW, /**< Verify down to first ILI encountered. */
  VERIFY_ILI_DEEP     /**< Verify down to ILI leaves. */
} VERIFY_LEVEL;

/* Verify that ILI nodes is structurally correct down to given level. */
void verify_ili(int ilix, VERIFY_LEVEL level);

/* Verify that an ILT is structurally correct down
   to given level. */
void verify_ilt(int iltx, VERIFY_LEVEL level);

/* Verify that a block is structurally correct down to given level. */
void verify_block(int bihx, VERIFY_LEVEL level);

/* Verify that function is structurally correct down to given level. */
void verify_function_ili(VERIFY_LEVEL level);

#else

#define verify_ili(ilix, level) ((void)(ilix), (void)(level))
#define verify_ilt(iltx, level) ((void)(iltx), (void)(level))
#define verify_block(bihx, level) ((void)(bihx), (void)(level))
#define verify_function_ili(level) ((void)(level))

#endif
