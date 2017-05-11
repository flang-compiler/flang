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

/*	ioinit3f.c - Implements LIB3F ioinit subprogram.  */

/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"

extern char *__fstr2cstr();
extern void __cstr_free();

#define IS_TRUE(x) ((x)&0x1)

struct {
  short ieof;
  short ictl;
  short ibzr;
} ioiflg_ = {0};

void ENT3F(IOINIT, ioinit)(int *cctl, int *bzro, int *apnd, DCHAR(prefix),
                           int *vrbose DCLEN(prefix))
{
  /* stub for now */
}
