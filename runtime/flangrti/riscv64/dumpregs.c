/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

#include <sys/ucontext.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <ctype.h>
#include "stdioInterf.h"

typedef struct {
    int     rn;     // Register index in to "regs" pointer
    char    *s;     // Symbolic name of register
} gprs_t;


/*
 * The way the structure below is organized, the registers are all
 * sequential with no gaps - the structure is probably overkill - but
 * allows for some flexibility.
 */

gprs_t gprs[] = {
    { 0, "x0" }, { 1, "x1" }, { 2, "x2"}, { 3, "x3" }, { 4, "x4" },
    { 5, "x5" }, { 6, "x6" }, { 7, "x7" }, { 8, "x8" }, { 9, "x9" },
    {10, "x10"}, {11, "x11"}, {12, "x12"}, {13, "x13"}, {14, "x14"},
    {15, "x15"}, {16, "x16"}, {17, "x17"}, {18, "x18"}, {19, "x19"},
    {20, "x20"}, {21, "x21"}, {22, "x22"}, {23, "x23"}, {24, "xr24"},
    {25, "x25"}, {26, "x26"}, {27, "x27"}, {28, "x28"}, {29, "x29"},
    {30, "x30"}, {31, "x31"},
};

void
dumpregs(uint64_t *regs)
{
  int i;
  int j;
  char *pc = NULL;

  if (regs == NULL)
    return;             // Not sure if this is possible

/*
 * Output has the following format:
 *  <REG>    <HEXADECIMAL>         <DECIMAL>               <ASCII>
 *  Example:
 *  r0       0x00003fffaf4a309c       70367390085276       .0J..?..
 *  sp       0x00003ffff437d1a0       70368546509216       ..7..?..
 *  toc      0x0000000010019300            268538624       ........
 *  r3       0x0000000010000e64            268439140       d.......
 *  ...
 */

  for (i = 0; i < sizeof gprs / sizeof *gprs; ++i) {
    fprintf(__io_stderr(), " %-8s 0x%016" PRIx64 " %20" PRId64 "\t",
      gprs[i].s, regs[gprs[i].rn], regs[gprs[i].rn]);
    pc = (char *)&(regs[gprs[i].rn]);
    for (j = 0; j < 8; ++j) {
      fputc(isprint(pc[j]) ? pc[j] : '.', __io_stderr());
    }
    fputs("\n", __io_stderr());
  }
}

uint64_t *
getRegs(ucontext_t *u)
{
  mcontext_t *mc = &u->uc_mcontext;
  return (uint64_t *)&(mc->__gregs);
}
