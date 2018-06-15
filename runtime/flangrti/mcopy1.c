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

#include "memops.h"

#if !defined(__GNU_LIBRARY__) && \
  !defined(__GNUC__) && !defined(__clang__)
void
__c_mcopy1(char *dest, char *src, long cnt)
{
  long i;

  for (i = 0; i < cnt; i++) {
    dest[i] = src[i];
  }
  return;
}
#endif

