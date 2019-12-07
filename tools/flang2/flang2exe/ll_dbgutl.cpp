/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
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

