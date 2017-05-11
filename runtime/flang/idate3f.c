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

/*	idate3f.c - Implements LIB3F idate subroutine.  */

#include "ent3f.h"

#include <time.h>

void
    ENT3F(IDATE, idate)(int *date_array)
{
  time_t ltime;
  struct tm *ltimvar;
  int yr;

  ltime = time(0);
  ltimvar = localtime(&ltime);
  date_array[0] = ltimvar->tm_mon + 1;
  date_array[1] = ltimvar->tm_mday;
  yr = ltimvar->tm_year;
  if (yr > 99)
    yr = yr % 100;
  date_array[2] = yr;
}
