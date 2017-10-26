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

/*	sleep3f.c - Implements DFPORT SLEEPQQ subprogram.  */

#ifndef _WIN32
#include <unistd.h>
#endif
#include "ent3f.h"

#if defined(WIN64) || defined(WIN32)

#include <windows.h>

void ENT3F(SLEEPQQ, sleepqq)(t) unsigned int *t;
{
  Sleep(*t); /* MS Sleep() is in terms of milliseconds */
}
#else
void ENT3F(SLEEPQQ, sleepqq)(t) unsigned int *t;
{
  sleep((*t) / 1000);
}

#endif
