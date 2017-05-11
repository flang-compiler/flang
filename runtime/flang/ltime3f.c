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

/*	ltime3f.c - Implements LIB3F ltime subprogram.  */

#include "ent3f.h"

typedef struct {
  int m[9]; /* 9 elements in tm structure */
} _TM;

/*
 * extern struct tm *localtime(const time_t *);
 *  the argument is either a pointer to 32-bit or 64-bit int depending on
 *  sizeof(time_t)
 */
extern _TM *localtime(void *);

void ENT3F(LTIME, ltime)(void *stime, _TM *tarray)
{
  _TM *p;
  p = localtime(stime);
  *tarray = *p;
}
