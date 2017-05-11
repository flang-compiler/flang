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

/* this isn't an actual 3f routine.  But it is useful */

/* setvbuf3f(lu,type,size)

   lu is the logical unit
   type is 0 - full buffering, 1 - line buffering, 2 - no buffering
   size is the size of the new buffer

   it returns 0 on success, non-zero on error
*/

#include <stdio.h>
#include "ent3f.h"

extern FILE *__getfile3f();

int ENT3F(SETVBUF3F, setvbuf3f)(int *lu, int *type, int *size)
{
  FILE *f;
  int t;

  f = __getfile3f(*lu);
  if (f == NULL) {
    return (1);
  }
  if (*type == 0) {
    t = _IOFBF;
  } else if (*type == 1) {
    t = _IOLBF;
  } else if (*type == 2) {
    t = _IONBF;
  } else {
    return (1);
  }
  if (setvbuf(f, NULL, t, *size) != 0) {
    return (1);
  }
  return (0);
}

int ENT3F(SETVBUF, setvbuf)(int *lu, int *type, int *size,
                            DCHAR(buf) DCLEN(buf))
{
  FILE *f;
  int t;

  f = __getfile3f(*lu);
  if (f == NULL) {
    return (1);
  }
  if (*type == 0) {
    t = _IOFBF;
  } else if (*type == 1) {
    t = _IOLBF;
  } else if (*type == 2) {
    t = _IONBF;
  } else {
    return (1);
  }
  if (setvbuf(f, CADR(buf), t, *size) != 0) {
    return (1);
  }
  return (0);
}
