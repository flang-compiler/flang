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

/* this is the dummy version of xfer_heap.c */

#include "fioMacros.h"

extern char *sbrk(int);

/* sbrk */

char *
__fort_sbrk(int len)
{
#ifndef _WIN32
  return (sbrk(len));
#endif
}

/* verify block is in global heap */

void
__fort_verghp(char *adr, int len, char *msg)
{
}

/* init global heap comm */

void
__fort_hinit(void)
{
}

/* send */

int
__fort_hsend(int cpu, struct ents *e)
{
  return (0);
}

/* recv */

int
__fort_hrecv(int cpu, struct ents *e)
{
  return (0);
}
