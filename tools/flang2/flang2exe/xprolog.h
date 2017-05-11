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
   \brief win64 data definitions to store xdata/pdata prolog
   information  to enable stack unwind and setjmp/longjmp calls
*/

#define UWOP_PUSH_NONVOL 0
#define UWOP_ALLOC_LARGE 1
#define UWOP_ALLOC_SMALL 2
#define UWOP_SET_FPREG 3
#define UWOP_SAVE_NONVOL 4
#define UWOP_SAVE_NONVOL_FAR 5
/* UWOP_SAVE_XMM and UWOP_SAVE_XMM_FAR are being depreciated 10/5
   AMD software conventions
 */
#define UWOP_SAVE_XMM 6
#define UWOP_SAVE_XMM_FAR 7
#define UWOP_SAVE_XMM128 8
#define UWOP_SAVE_XMM128_FAR 9
#define UWOP_PUSH_MACHFRAME 10

extern char *xdata_opcode[];
extern void add_xdata_and_label(int, long, int);
extern void add_xdata_no_label(int, long, int);
extern void add_xdata_reg(int, long, int, char *, int);
extern ISZ_T win64_xdata_frame_adjust;

/* translate our data registers to the XDATA encoding */
#define XDATA_REGSIZE 17
#define XLAB_SIZE 12
extern char xprolog_beg_lab[XLAB_SIZE];
extern int xdata_reg[XDATA_REGSIZE];

struct xprolog {
  int regnum;
  long offset;
  short instr;
  char lab[XLAB_SIZE];
};

extern struct xprolog xprolog_regs[];
extern int xpr_reg_idx;
extern ISZ_T win64_xdata_frame_adjust;
