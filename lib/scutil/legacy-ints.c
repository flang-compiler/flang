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
 * \brief Convert 64-bit ints to/from legacy big-endian 2-word form.
 */

#include "legacy-ints.h"

void
bgitoi64(int64_t v, INT64 res)
{
  res[0] = v >> 32;
  res[1] = v;
}

int64_t
i64tobgi(INT64 v)
{
  int64_t x = ((int64_t) v[0] << 32) | (uint32_t) v[1];
  return x;
}
