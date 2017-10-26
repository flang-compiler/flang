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

#if !defined(TARGET_WIN)
#include <sys/ucontext.h>
#endif
#include "stdioInterf.h"

/* define register indexes */

#define R8 0
#define R9 1
#define R10 2
#define R11 3
#define R12 4
#define R13 5
#define R14 6
#define R15 7
#define RDI 8
#define RSI 9
#define RBP 10
#define RBX 11
#define RDX 12
#define RAX 13
#define RCX 14
#define RSP 15
#define RIP 16

#if defined(TARGET_OSX) || defined(TARGET_WIN)
/* no gregs and/or ucontext defined in for OSX or Windows */
void * 
getRegs(void *u)
{
 return (void *)0;
}
void
dumpregs(void *regs)
{
}

#else

void
dumpregs(gregset_t *regs)
{
#if defined(LINUX8664)
  fprintf(__io_stderr(), "   rax %016lx, rbx %016lx, rcx %016lx\n",
          (*regs)[RAX], (*regs)[RBX], (*regs)[RCX]);
  fprintf(__io_stderr(), "   rdx %016lx, rsp %016lx, rbp %016lx\n",
          (*regs)[RDX], (*regs)[RSP], (*regs)[RBP]);
  fprintf(__io_stderr(), "   rsi %016lx, rdi %016lx, r8  %016lx\n",
          (*regs)[RSI], (*regs)[RDI], (*regs)[R8]);
  fprintf(__io_stderr(), "   r9  %016lx, r10 %016lx, r11 %016lx\n",
          (*regs)[R9], (*regs)[R10], (*regs)[R11]);
  fprintf(__io_stderr(), "   r12 %016lx, r13 %016lx, r14 %016lx\n",
          (*regs)[R12], (*regs)[R13], (*regs)[R14]);
  fprintf(__io_stderr(), "   r15 %016lx\n", (unsigned long)(*regs)[R15]);
#endif
}

gregset_t *
getRegs(ucontext_t *u)
{
  return &u->uc_mcontext.gregs;
}

#endif

