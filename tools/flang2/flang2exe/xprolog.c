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

/**
   \file
   \brief win64 data definitions to store prolog information
*/

#include <scutil.h>
#include "gbldefs.h"
#include "global.h"
#include "machreg.h"
#include "xprolog.h"
#include "symtab.h"
#include "ili.h"

#if defined(TARGET_WIN_X8664)
#define ASMFIL gbl.asmfil
#define DIR_BYTE ".byte"
#define DIR_2BYTE ".2byte"
#define ALLOC_MED_SIZE 512000 - 8

extern void xp_print_header(char *, int);
extern char *xp_get_reg_name(int);
extern char xprolog_beg_lab[XLAB_SIZE] = "";
extern char *hex(int, int); /* assem.c */

char *xdata_opcode[] = {"PUSH_NONVOL",     "ALLOC_LARGE",   "ALLOC_SMALL",
                        "SET_FPREG",       "SAVE_NONVOL",   "SAVE_NONVOL_FAR",
                        "SAVE_XMM",        "SAVE_XMM_FAR",  "SAVE_XMM128",
                        "SAVE_XMM128_FAR", "PUSH_MACHFRAME"};

#define XDATA_REGSIZE 17
extern int xdata_reg[XDATA_REGSIZE] = {-1, /* bad */
                                       0,  /* rax */
                                       1,  /* rcx */
                                       2,  /* rdx */
                                       8,  /* r8 */
                                       9,  /* r9 */
                                       10, /* r10 */
                                       11, /* r11 */
                                       3,  /* rbx */
                                       5,  /* rbp */
                                       7,  /* rdi */
                                       6,  /* rsi */
                                       12, /* r12 */
                                       13, /* r13 */
                                       14, /* r14 */
                                       15, /* r15 */
                                       4 /* rsp */};

static int unwind_count; /* unwind count for this function */

/**
   \brief number of UNWIND_CODE nodes this instruction inhabits
*/
int
xpr_node_count(short instr, int idx)
{

  switch (instr) {

  case UWOP_PUSH_NONVOL:
  case UWOP_ALLOC_SMALL:
  case UWOP_SET_FPREG:
  case UWOP_PUSH_MACHFRAME:
    return 1;

  case UWOP_SAVE_XMM:
  case UWOP_SAVE_XMM128:
  case UWOP_SAVE_NONVOL:
    return 2;
  case UWOP_ALLOC_LARGE:
    if (xprolog_regs[idx].offset <= ALLOC_MED_SIZE)
      return 2;
    return 3;
  case UWOP_SAVE_XMM_FAR:
  case UWOP_SAVE_XMM128_FAR:
  case UWOP_SAVE_NONVOL_FAR:
    return 3;
  }
#if DEBUG
  interr("xp_node_count: unsupported encoding\n", 0, 4);
#endif
  return 0;
}

/**
   \brief return a string describing the operation
 */
extern char *
xp_get_op_string(int i)
{
  static char buff[40];

  switch (xprolog_regs[i].instr) {

  case UWOP_PUSH_NONVOL:
  case UWOP_SAVE_NONVOL:
  case UWOP_SAVE_NONVOL_FAR:
  case UWOP_SAVE_XMM:
  case UWOP_SAVE_XMM_FAR:
  case UWOP_SAVE_XMM128:
  case UWOP_SAVE_XMM128_FAR:
    snprintf(buff, 40, "(%s)%s", xp_get_reg_name(i),
             xdata_opcode[xprolog_regs[i].instr]);
    break;
  case UWOP_SET_FPREG:
    snprintf(buff, 40, " %s[rsp+%d] %s",

             xp_get_reg_name(i), xprolog_regs[i].offset,
             xdata_opcode[xprolog_regs[i].instr]);
    break;
  case UWOP_ALLOC_SMALL:
    snprintf(buff, 40, "(%d/8 -8)%s", xprolog_regs[i].offset,
             xdata_opcode[xprolog_regs[i].instr]);
    break;
  case UWOP_ALLOC_LARGE:
    if (xprolog_regs[i].offset <= ALLOC_MED_SIZE)
      snprintf(buff, 40, "(short form)%s", xdata_opcode[xprolog_regs[i].instr]);
    else
      snprintf(buff, 40, "(long form)%s", xdata_opcode[xprolog_regs[i].instr]);
    break;
  case UWOP_PUSH_MACHFRAME:
    snprintf(buff, 40, "UNSUPPORTED UWOP_PUSH_MACHFRAME\n");
    break;
  }
  return (buff);
}

/**
   \brief return the full byte op (reg)opcode for the unwind code
   array
*/
extern short
xp_get_op_encoding(int i)
{

  int op = 0;
  switch (xprolog_regs[i].instr) {

  case UWOP_PUSH_NONVOL:
  case UWOP_SAVE_NONVOL:
  case UWOP_SAVE_NONVOL_FAR:
    op = xdata_reg[xprolog_regs[i].regnum];
    break;

  case UWOP_SET_FPREG:
    op = xprolog_regs[i].offset / 16;
    break;

  case UWOP_SAVE_XMM:
  case UWOP_SAVE_XMM_FAR:
  case UWOP_SAVE_XMM128:
  case UWOP_SAVE_XMM128_FAR:
    op = xprolog_regs[i].regnum - 1;
    break;
  case UWOP_ALLOC_LARGE:
    if (xprolog_regs[i].offset <= ALLOC_MED_SIZE)
      op = 0;
    else
      op = 1;
    break;
  case UWOP_ALLOC_SMALL:
    op = xprolog_regs[i].offset - 8;
    op /= 8;
    break;
#if DEBUG
  case UWOP_PUSH_MACHFRAME:
  default:
    interr("xp_get_op_encoding: unsupported encoding\n", 0, 4);
#endif
  }
  op = op << 4;                /* left shift the register number */
  op |= xprolog_regs[i].instr; /* or in the instruction */
  return op;
}

/**
   \brief return the gp or xmm register name for this entry in the
   xprolog array
*/
extern char *
xp_get_reg_name(int i)
{
  switch (xprolog_regs[i].instr) {

  case UWOP_PUSH_NONVOL:
  case UWOP_SET_FPREG:
  case UWOP_SAVE_NONVOL:
  case UWOP_SAVE_NONVOL_FAR:
    return (gp_reg[xprolog_regs[i].regnum]);

  case UWOP_SAVE_XMM:
  case UWOP_SAVE_XMM_FAR:
  case UWOP_SAVE_XMM128:
  case UWOP_SAVE_XMM128_FAR:
    return (xm_reg[xprolog_regs[i].regnum]);
  case UWOP_ALLOC_SMALL:
  case UWOP_ALLOC_LARGE:
    return ("NONE");
  }

#if DEBUG
  interr("xp_get_reg_name: unsupported encoding\n", 0, 4);
#endif
  return 0;
}

extern void
xp_print_unwind_code(int i)
{
  int scaled_offset;
  char prolog_offset_buff[50];

  snprintf(prolog_offset_buff, 50, "%s-%s", xprolog_regs[i].lab,
           xprolog_beg_lab);
  fprintf(ASMFIL, "\t%s\t%s\t     # entry %d: offset\n", DIR_BYTE,
          prolog_offset_buff, i + 1);

  fprintf(ASMFIL, "\t%s\t0x%x\t     # %s\n", DIR_BYTE, xp_get_op_encoding(i),
          xp_get_op_string(i));

  if (xpr_node_count(xprolog_regs[i].instr, i) > 1) {
    switch (xprolog_regs[i].instr) {
    case UWOP_SAVE_XMM_FAR:
    case UWOP_SAVE_XMM128_FAR:
    case UWOP_SAVE_NONVOL_FAR:
    case UWOP_ALLOC_LARGE:
      /* need this test */
      if (xprolog_regs[i].offset > ALLOC_MED_SIZE) {
        /* 3 node version */
        union un {
          short short_off[4];
          long int_off;
        } tmp;
        char *c1, *c2;
        BCOPY(&tmp, &xprolog_regs[i].offset, char, sizeof(long));
        c1 = (char *)(&tmp);
        c2 = c1 + 1;
        fprintf(ASMFIL,
                "\t%s\t%s,%s         # little endian lower half of 0x%x\n",
                DIR_BYTE, hex(*c1 & 0xff, 2), hex(*c2 & 0xff, 2), tmp.int_off);

        c1 = ++c2;
        c2 = c1 + 1;
        fprintf(ASMFIL,
                "\t%s\t%s,%s         # little endian upper half of 0x%x\n",
                DIR_BYTE, hex(*c1 & 0xff, 2), hex(*c2 & 0xff, 2), tmp.int_off);
        break;
      }
    /* fall through to offset/8: 2 node version */

    case UWOP_SAVE_XMM:
    case UWOP_SAVE_NONVOL:
      /* print the scaled by 8 offset */
      scaled_offset = 0;
      if (xprolog_regs[i].offset != 0)
        scaled_offset = abs(xprolog_regs[i].offset) / 8;

      fprintf(ASMFIL, "\t%s\t0x%x         # to: offset(%d)/8\n", DIR_2BYTE,
              scaled_offset, abs(xprolog_regs[i].offset));
      break;

    case UWOP_SAVE_XMM128:
      /* print the scaled by 16 offset */
      scaled_offset = 0;
      if (xprolog_regs[i].offset != 0)
        scaled_offset = abs(xprolog_regs[i].offset) / 16;

      fprintf(ASMFIL, "\t%s\t0x%x         # to: offset(%d)/16\n", DIR_2BYTE,
              scaled_offset, abs(xprolog_regs[i].offset));
      break;
    }
  }
}

/**
   \brief print the unwind codes for this function sptr
*/
extern void
xp_print_unwind_codes(int sptr, char *func_name)
{
  int i;

  xp_print_header(func_name, sptr);
  fprintf(ASMFIL, "\t    \t             # start unwind code array\n");
  if (unwind_count > 0) {
    /* in reverse */
    for (i = xpr_reg_idx - 1; i >= 0; i--) {
      xp_print_unwind_code(i);
    }
    /* even out the count */
    if (unwind_count & 1)
      fprintf(ASMFIL, "\t%s\t0x00, 0x00   # Even Fill\n", DIR_BYTE);
  }
}

extern void
xp_print_header(char *funcname, int sptr)
{
  int i;

  fprintf(ASMFIL, "\t%s\t0x01         # version 1, flag:0\n", DIR_BYTE);
  fprintf(ASMFIL, "\t%s\t%s - %s  # size of prolog\n", DIR_BYTE,
          xprolog_regs[xpr_reg_idx - 1].lab, funcname);

  /* count the number of unwind codes */
  unwind_count = 0;
  for (i = 0; i < xpr_reg_idx; i++) {
    unwind_count += xpr_node_count(xprolog_regs[i].instr, i);
  }
  if (!unwind_count)
    unwind_count = 1; /* use default */

  fprintf(ASMFIL, "\t%s\t0x%x          # count of (2 byte) unwind nodes\n",
          DIR_BYTE, unwind_count);

  if ((win64_xdata_frame_adjust > 0) && (win64_xdata_frame_adjust < 240)) {
    fprintf(ASMFIL, "\t%s\t0x%x5         # (%d/16) sets rbp:5\n", DIR_BYTE,
            win64_xdata_frame_adjust / 16, win64_xdata_frame_adjust);
    win64_xdata_frame_adjust = 0;
  } else {
    fprintf(ASMFIL, "\t%s\t0x00         # no frame register\n", DIR_BYTE);
  }
}

/**
   \brief Store win64 XDATA prolog information
*/
extern void
add_xdata_reg(int rg, long p, int instr, char *lab, int labnum)
{

  xprolog_regs[xpr_reg_idx].regnum = rg;
  xprolog_regs[xpr_reg_idx].offset = p;
  xprolog_regs[xpr_reg_idx].instr = instr;
  if (lab)
    snprintf(xprolog_regs[xpr_reg_idx].lab, XLAB_SIZE, "%s%d", lab, labnum);
  switch (instr) {
  case UWOP_ALLOC_SMALL:
    if (p > 128) {
      xprolog_regs[xpr_reg_idx].instr = UWOP_ALLOC_LARGE;
    }
    break;
  case UWOP_SAVE_NONVOL:
    if (p > ALLOC_MED_SIZE) {
      xprolog_regs[xpr_reg_idx].instr = UWOP_SAVE_NONVOL_FAR;
    }
    break;
  case UWOP_SAVE_XMM:
    /* Should not be used: gone as of 9/27/05 */
    if (p > ALLOC_MED_SIZE) {
      xprolog_regs[xpr_reg_idx].instr = UWOP_SAVE_XMM_FAR;
    }
    break;
  case UWOP_SAVE_XMM128:
    /* scaled by 16, instead of the standard 8 */
    if ((p / 2) > ALLOC_MED_SIZE) {
      xprolog_regs[xpr_reg_idx].instr = UWOP_SAVE_XMM128_FAR;
    }
    break;
  }

  xpr_reg_idx++;
}
#else
int foo_tmp; /* non empty translation unit */
#endif
