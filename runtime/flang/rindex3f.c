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

/* clang-format off */

/*	rindex3f.c - Implements LIB3F rindex subprogram.  */

#include "ent3f.h"

int ENT3F(RINDEX, rindex)(DCHAR(a1), DCHAR(a2) DCLEN(a1) DCLEN(a2))
{
  char *a1 = CADR(a1); /* pointer to string being searched */
  char *a2 = CADR(a2); /* pointer to string being searched for */
  int a1_l = CLEN(a1); /* length of a1 */
  int a2_l = CLEN(a2); /* length of a2 */
  int i1, i2, match;

  for (i1 = a1_l - a2_l; i1 >= 0; i1--) {
    match = 1;
    for (i2 = 0; i2 < a2_l; i2++) {
      if (a1[i1 + i2] != a2[i2]) {
        match = 0;
        break;
      }
    }
    if (match)
      return (i1 + 1);
  }
  return (0);
}
