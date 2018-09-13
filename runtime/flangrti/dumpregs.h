/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */


/**
 *  \file
 *  Declare routines that access the machine registers
 */
#if defined(TARGET_OSX) || defined(TARGET_WIN)
#define gregset_t void
void dumpregs(void *);
void *getRegs(void *);
#else
void dumpregs(gregset_t *regs);
gregset_t *getRegs(ucontext_t *u);
#endif
