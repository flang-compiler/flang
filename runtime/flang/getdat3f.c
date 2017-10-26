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

/*	getdat3f.c - Implements getdat subroutine.  */

#if defined(WIN64) || defined(WIN32)

#include <windows.h>
#include "ent3f.h"

void ENT3F(GETDAT, getdat)(unsigned short *iyr, unsigned short *imon,
                           unsigned short *iday)
{
  SYSTEMTIME st;
  GetLocalTime(&st); /* gets current time */
  *iyr = st.wYear;
  *imon = st.wMonth;
  *iday = st.wDay;
}

void ENT3F(GETDAT4, getdat4)(int *iyr, int *imon, int *iday)
{
  SYSTEMTIME st;
  GetLocalTime(&st); /* gets current time */
  *iyr = st.wYear;
  *imon = st.wMonth;
  *iday = st.wDay;
}

void ENT3F(GETDAT8, getdat8)(long long *iyr, long long *imon, long long *iday)
{
  SYSTEMTIME st;
  GetLocalTime(&st); /* gets current time */
  *iyr = st.wYear;
  *imon = st.wMonth;
  *iday = st.wDay;
}
#endif
