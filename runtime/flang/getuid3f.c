/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

/* clang-format off */

/*	getuid3f.c - Implements LIB3F getuid subprogram.  */

#ifndef WINNT

#include "ent3f.h"
#include <unistd.h>

int ENT3F(GETUID, getuid)() { return getuid(); }

#endif /* !WINNT */
