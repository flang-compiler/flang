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

/*	gettim3f.c - Implements gettim subroutine.  */

#if defined(WIN64) || defined(WIN32)

#include <windows.h>
#include "ent3f.h"

void ENT3F(GETTIM, gettim)(unsigned short *hr, unsigned short *min,
                           unsigned short *sec, unsigned short *hsec)
{
  SYSTEMTIME st;
  GetLocalTime(&st); /* gets current time */
  *hr = st.wHour;
  *min = st.wMinute;
  *sec = st.wSecond;
  *hsec = st.wMilliseconds / 10;
}

void ENT3F(GETTIM4, gettim4)(int *hr, int *min, int *sec, int *hsec)
{
  SYSTEMTIME st;
  GetLocalTime(&st); /* gets current time */
  *hr = st.wHour;
  *min = st.wMinute;
  *sec = st.wSecond;
  *hsec = st.wMilliseconds / 10;
}

void ENT3F(GETTIM8, gettim8)(long long *hr, long long *min, long long *sec,
                             long long *hsec)
{
  SYSTEMTIME st;
  GetLocalTime(&st); /* gets current time */
  *hr = st.wHour;
  *min = st.wMinute;
  *sec = st.wSecond;
  *hsec = st.wMilliseconds / 10;
}
#endif
