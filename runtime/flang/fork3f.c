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

/*	fork3f.c - Implements LIB3F fork subprogram.  */

#ifndef _WIN32

/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"

int ENT3F(FORK, fork)()
{
  void *f, *q;
  int pid;

  for (f = GET_FIO_FCBS; f != NULL; f = q) {
    q = FIO_FCB_NEXT(f);
    if (fflush(FIO_FCB_FP(f)) != 0)
      return -__io_errno();
  }

  pid = fork();
  if (pid < 0)
    return -__io_errno();
  else
    return pid;
}

#endif /* !WINNT */
