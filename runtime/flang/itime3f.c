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

/*	itime3f.c - Implements LIB3F itime subroutine.  */

#include "ent3f.h"

#include <time.h>

void ENT3F(ITIME, itime)(int iarray[3])
{
  time_t ltime;
  struct tm *ltimvar;

  ltime = time(0);
  ltimvar = localtime(&ltime);
  iarray[0] = ltimvar->tm_hour;
  iarray[1] = ltimvar->tm_min;
  iarray[2] = ltimvar->tm_sec;
  return;
}
