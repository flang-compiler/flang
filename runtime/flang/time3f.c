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

/*	time3f.c - Implements LIB3F time function.  */

#include <time.h>
#include "ent3f.h"

#if defined(TARGET_INTERIX_X8664) || defined(TARGET_WIN_X8664)
#define INT long long
#else
#define INT long
#endif

INT ENT3F(TIME, time)() { return time(0); }

long long ENT3F(TIME8, time8)() { return time(0); }
