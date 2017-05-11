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

#include <stdlib.h>

int
__fort_atol(char *p)
{
  int n;
  char *q;

  if (p == (char *)0) {
    return (0);
  }
  n = strtol(p, &q, 0);
  switch (*q) {
  case 'k':
  case 'K':
    n <<= 10;
    break;
  case 'm':
  case 'M':
    n <<= 20;
    break;
  case 'g':
  case 'G':
    n <<= 30;
    break;
  }
  return (n);
}

long
__fort_strtol(char *str, char **ptr, int base)
{
  long val;
  char *end;

  if (str) {
    val = strtol(str, &end, base);
    if (end != str) {
      switch (*end) {
      case 'g':
      case 'G':
        val <<= 10;
      case 'm':
      case 'M':
        val <<= 10;
      case 'k':
      case 'K':
        val <<= 10;
        ++end;
      }
    }
  } else {
    val = 0;
    end = str;
  }
  if (ptr)
    *ptr = end;
  return val;
}
