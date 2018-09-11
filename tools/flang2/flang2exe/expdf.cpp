/*
 * Copyright (c) 1993-2017, NVIDIA CORPORATION.  All rights reserved.
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

#include "gbldefs.h"
#include "global.h"
#include "symtab.h"
#include "ilm.h"
/* need ilmtp.h since expand.h tests #ifdef IM_... */
#include "ilmtp.h"
#include "ili.h"
#define EXPANDER_DECLARE_INTERNAL
#include "expand.h"
#include "regutil.h"

ILIB ilib;

ILTB iltb;

BIHB bihb;

NMEB nmeb;

EXP expb = {0};

RCANDB rcandb;

RATB ratb;
