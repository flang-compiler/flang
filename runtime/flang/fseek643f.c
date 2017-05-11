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

/** \file
 * \brief Implements LIB3F 64-bit fseek subroutine.  */

/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"

extern FILE *__getfile3f();

int ENT3F(FSEEK64, fseek64)(lu, offset, from) int *lu;
long long *offset;
int *from;
{
  FILE *f;

  /* DON'T issue any error messages */

  f = __getfile3f(*lu);
  if (f) {
    int fr;

    switch (*from) {
    case 0:
      fr = SEEK_SET;
      break;
    case 1:
      fr = SEEK_CUR;
      break;
    case 2:
      fr = SEEK_END;
      break;
    default:
      /* ERROR */
      fprintf(__io_stderr(), "Illegal fseek value %d\n", *from);
      return 0;
    }
    if (__io_fseek64(f, *offset, fr) == 0)
      return 0;
    return __io_errno();
  }

  return 0;
}
