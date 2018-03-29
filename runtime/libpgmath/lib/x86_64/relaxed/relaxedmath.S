
/*
 * Copyright (c) 2017-2018, NVIDIA CORPORATION.  All rights reserved.
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
 */

#include "directives.h"

#define _SX0 0
#define _SX1 4
#define _SX2 8
#define _SX3 12

#define _SY0 16
#define _SY1 20
#define _SY2 24
#define _SY3 28

#define _SR0 32
#define _SR1 36
#define _SR2 40
#define _SR3 44

#ifndef LNUM
#define LNUM 001
#endif
#define NNN LNUM

#define LNUM 001
#include "relaxedtable.h"

#undef FMA4_TARGET
#define VEX_TARGET 1
#undef LNUM
#define LNUM 004
#include "relaxedmath_vex.h"
#include "relaxedmath_vex_mask.h"


#undef VEX_TARGET 
#undef LNUM
#define FMA4_TARGET 1
#define LNUM 005
#include "relaxedmath_vex.h"
#include "relaxedmath_vex_mask.h"

