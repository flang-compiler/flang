/*
 * Copyright (c) 2013-2018, NVIDIA CORPORATION.  All rights reserved.
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

/* ll_dbgutl.c utility routines for LLVM to enable debug info generation for
   Accelerator languages */

#include "gbldefs.h"
#include "global.h"
#include "llutil.h"
#include "lldebug.h"
#include "symtab.h"

static int expr_id;

