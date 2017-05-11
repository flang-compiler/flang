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

/** \file
    \brief Data definitions for communication data structures.
 */

#include "gbldefs.h"
#include "global.h"
#include "symtab.h"
#include "soc.h"
#include "semant.h"
#include "ast.h"
#include "gramtk.h"
#include "comm.h"
#include "symutl.h"
#include "extern.h"

TRANSFORM trans = {{NULL, 0, 0},
                   {NULL, 0, 0},
                   {NULL, 0, 0},
                   0,
                   0,
                   0,
                   NULL,
                   NULL,
                   0,
                   0,
                   0,
                   0,
                   0};
struct arg_gbl arg_gbl = {0, 0, FALSE, FALSE};
struct forall_gbl forall_gbl = {0, 0, 0, 0, 0, 0};
struct pre_loop pre_loop = {0, 0, 0, 0};
struct comminfo comminfo = {0, 0, 0, 0, 0, 0, 0, 0, 0, {0, 0, 0, 0, 0, 0}, 0};
struct tbl tbl = {NULL, 0, 0};
struct tbl pertbl = {NULL, 0, 0};
struct tbl gstbl = {NULL, 0, 0};
struct tbl brtbl = {NULL, 0, 0};
