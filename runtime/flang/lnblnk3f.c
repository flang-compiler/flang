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

/*	lnblnk3f.c - Implements LIB3F lnblnk subprogram.  */

#include "ent3f.h"

int ENT3F(LNBLNK, lnblnk)(DCHAR(a1) DCLEN(a1))
{
  int i;
  char *a1 = CADR(a1);
  int len = CLEN(a1);

  for (i = len - 1; i >= 0; i--)
    if (a1[i] != ' ')
      return i + 1;
  return 0;
}
