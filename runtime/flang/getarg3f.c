/*
 * Copyright (c) 1993-2018, NVIDIA CORPORATION.  All rights reserved.
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

/*  getarg and iargc utility functions for Fortran programmers  */

#include "ent3f.h"

extern int __io_get_argc();
extern char **__io_get_argv();

/*
 * get nth command line arg and put it into character variable 'arg':
 */
void ENT3F(GETARG, getarg)(int *n, DCHAR(arg) DCLEN(arg))
{
  char *arg = CADR(arg);
  int len = CLEN(arg);
  char *p, *q;
  int i = *n;
  char **v;

  if (i < 0 || i >= __io_get_argc()) {
    i = 0;
    q = arg;
  } else {
    v = __io_get_argv();
    p = v[i];
    /*  copy characters into arg:  */
    for (q = arg, i = 0; *p != 0 && i < len; q++, p++, i++)
      *q = *p;
  }
  /*  blank fill if necessary:  */
  for (; i < len; q++, i++)
    *q = ' ';
}

int ENT3F(IARGC, iargc)(void)
/*  return number of command line args following program name:  */
{
  return __io_get_argc() - 1;
}

/* include .init section */

void
ftn_arg_init()
{
}

