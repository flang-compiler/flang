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

/*	mvbits3f.c - Implements LIB3F mvbits subprogram.  */

#include "ent3f.h"
#include "enames.h"
#include "ftnbitsup.h"

void ENT3F(MVBITS, mvbits)(src, pos, len, dest,
                           posd) int *src; /* source field */
int *pos;                                  /* start position in source field */
int *len;                                  /* number of bits to move */
int *dest;                                 /* destination field */
int *posd;                                 /* start position in dest field */
{
  Ftn_jmvbits(*src, *pos, *len, dest, *posd);
}
