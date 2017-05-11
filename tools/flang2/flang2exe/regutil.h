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
 * \brief Machine independent register utilities
 *
 * C/FTN macros, typedefs, and data declarations used to access the register
 * candidate table, the register assigned table, and the register temporaries.
 * This information is machine independent. The machine dependent stuff is in
 * machreg.h.
 */

#include <stdint.h>

#define RTEMPS 11

#define LST_AREA 6
#define AR_AREA 7

/*   Linked list item */
typedef struct LST_TAG {
  int item;
  struct LST_TAG *next;
} LST;

#define ADDNODE(l, entry)                                          \
  {                                                                \
    LST *node;                                                     \
    extern char *getitem();                                        \
    node = (LST *)getitem(LST_AREA, sizeof(struct LST_TAG));       \
    assert(node != 0, "no space allocated for linked-list", 0, 3); \
    node->item = entry;                                            \
    node->next = l;                                                \
    l = node;                                                      \
  }

/***** node for ar assignment of common block addresses *****/
typedef struct arasgntag {
  INT max;
  INT min;
  short weight;
  short ar;
  int sym; /* 0 ==> statics, 1 ==> constants, ow sym. */
  INT offset;
  struct arasgntag *next;
} ARASGN;

/***** Register Constant Flags *****/
/* For restricting the assignment of registers to constants per RTYPE.
 * Used in machreg.c to init reg[...].const_flag; checked where
 * assignments occur (i.e., globalreg.c).
 */

#define RCF_NONE 0      /* constants not assigned */
#define RCF_ALL 0x1     /* no restriction on value */
#define RCF_NOT0 0x2    /* not if value is 0 */
#define RCF_NOT32K 0x4  /* not if int value in [-32768, 32767] */
#define RCF_NOTL16 0x8  /* not if high 16 bits of mask value == 0 */
#define RCF_NOTH16 0x10 /* not if low 16 bits of mask value == 0 */
#define RCF_NOTA8B 0x20 /* not if abs(v) <= 255 (8 bits) */
#define RCF_NOTA9B 0x40 /* not if abs(v) <= 511 (9 bits) */

/*****  Register Assigned Table  *****/

typedef struct {
  int reg;    /* ili index of register define or temp
               * load.
               */
  int addr;   /* ILI addr of value assigned */
  char atype; /* type of candidate:
               *   RATA_NME, RATA_CONST, RATA_ILI,
               *   RATA_TEMP, RATA_IND, RATA_UPLV
               */
  char rtype; /* register type:
               *   RATA_IR, RATA_SP, RATA_DP, RATA_AR
               */
  union {
    uint16_t all;
    struct {
      uint16_t confl : 1; /* ILI usage conflicts with the existing*/
                          /*     msize - opt 2 only */
      uint16_t store : 1; /* a store of the variable occurred */
      uint16_t eint : 1;  /* extended int (ST100) */
      uint16_t vint : 1;  /* vectorial int (ST140) */
    } bits;
  } flags;
  INT msize; /* memory size of register; also, the dtype
              * record if vector
              */
  int val;   /* value assigned to register:
              *     names entry, constant st item,
              *     ili entry
              * If this is the first entry, this is the
              * number of elements (assignments).
              */
} RAT;

/*****  Storage Information for RAT  *****/

typedef struct {
  RAT *stg_base;
  int stg_avail;
  int stg_size;
  LOGICAL mexits;   /* true => multiple exits in current loop */
  LOGICAL use_agra; /* true => alternate global reg alloc */
} RATB;

/*****  Macros for RAT *****/

/* NOTE: if these rtype values change,
 *         inits of il_rtype_df,il_mv_rtype, il_free_rtype
 *         in regutil.c and reg in machreg.c will have to change
 */
/* rtypes */
#define RATA_IR 0
#define RATA_SP 1
#define RATA_DP 2
#define RATA_AR 3
#define RATA_KR 4
#define RATA_VECT 5
#define RATA_QP 6
#define RATA_CSP 7
#define RATA_CDP 8
#define RATA_CQP 9
#define RATA_X87 10
#define RATA_CX87 11

#define RATA_RTYPES_ACTIVE RATA_X87
/* these RTYPES are not actively processed.
 * They are mapped onto the RATA_SP or RATA_DP rtypes.
 * XM are used for sse xmm registers. STK are the old x87 stack-based regs.
 */
#define RATA_SPXM 12
#define RATA_DPXM 13
#define RATA_RTYPES_TOTAL RATA_DPXM + 1
/****
Remove these and their refs in regutil.c & ....x86/src/machreg.c
#define RATA_SPSTK      8
#define RATA_DPSTK      9
#define RATA_RTYPES_TOTAL RATA_DPSTK + 1
****/

#define RATA_NME 0
#define RATA_CONST 1
#define RATA_ILI 2
#define RATA_TEMP 3
#define RATA_IND 4
#define RATA_ARR 5
#define RATA_RPL 6
#define RATA_UPLV 7

#define RAT_REG(i) ratb.stg_base[i].reg
#define RAT_ADDR(i) ratb.stg_base[i].addr
#define RAT_RTYPE(i) ratb.stg_base[i].rtype
#define RAT_ATYPE(i) ratb.stg_base[i].atype
#define RAT_FLAGS(i) ratb.stg_base[i].flags.all
#define RAT_CONFL(i) ratb.stg_base[i].flags.bits.confl
#define RAT_STORE(i) ratb.stg_base[i].flags.bits.store
#define RAT_EINT(i) ratb.stg_base[i].flags.bits.eint
#define RAT_VINT(i) ratb.stg_base[i].flags.bits.vint
#define RAT_MSIZE(i) ratb.stg_base[i].msize
#define RAT_VAL(i) ratb.stg_base[i].val
#define RAT_ISIR(i) (RAT_RTYPE(i) == RATA_IR)
#define RAT_ISAR(i) (RAT_RTYPE(i) == RATA_AR)
#define RAT_ISSP(i) (RAT_RTYPE(i) == RATA_SP)
#define RAT_ISDP(i) (RAT_RTYPE(i) == RATA_DP)
#define RAT_ISCSP(i) (RAT_RTYPE(i) == RATA_CSP)
#define RAT_ISCDP(i) (RAT_RTYPE(i) == RATA_CDP)
#define RAT_ISCQP(i) (RAT_RTYPE(i) == RATA_CQP)
#define RAT_ISKR(i) (RAT_RTYPE(i) == RATA_KR)
#define RAT_ISNME(i) (RAT_ATYPE(i) == RATA_NME)
#define RAT_ISCONST(i) (RAT_ATYPE(i) == RATA_CONST)
#define RAT_ISILI(i) (RAT_ATYPE(i) == RATA_ILI)
#define RAT_ISTEMP(i) (RAT_ATYPE(i) == RATA_TEMP)
#define RAT_ISUPLV(i) (RAT_ATYPE(i) == RATA_UPLV)
#define MAXRAT 67108864

#define GET_RAT(i)                                          \
  {                                                         \
    i = ratb.stg_avail++;                                   \
    if (ratb.stg_avail > MAXRAT)                            \
      error(7, 4, 0, CNULL, CNULL);                         \
    NEED(ratb.stg_avail, ratb.stg_base, RAT, ratb.stg_size, \
         ratb.stg_size + 100);                              \
    if (ratb.stg_base == NULL)                              \
      error(7, 4, 0, CNULL, CNULL);                         \
  }

/*****  Register Candidate Table  *****/

typedef struct {
  char atype; /* type of candidate:
               *   RATA_NME, RATA_CONST, RATA_ILI,
               *   RATA_TEMP, RATA_IND, RATA_ARR,
               *   RATA_UPL
               */
  char rtype; /* register type:			*/
  /*   RATA_IR, RATA_SP, RATA_DP, RATA_AR	*/

  union {
    unsigned int all;
    struct {
      uint16_t confl : 1;  /* ILI usage conflicts with the existing
                            *   msize
                            */
      uint16_t store : 1;  /* variable was stored */
      uint16_t cse : 1;    /* candidate is an induction cse
                            * use - for opt 2 only
                            */
      uint16_t ok : 1;     /* ok to assign register to const cand */
      uint16_t noreg : 1;  /* do not assign register to non-const cand */
      uint16_t ignore : 1; /* ignore this candidate */
      uint16_t eint : 1;   /* extended int (ST100) */
      uint16_t vint : 1;   /* vectorial int (ST140) */
      uint16_t inv : 1;    /* this candidate is for an invariant */
      uint16_t tinv : 1;   /* this candidate is for a transitive invariant */
    } bits;
  } flags;
  INT msize; /* memory size of register; also, the dtype
              * record if vector
              */
  int val;   /* index of candidate (depends on atype)*/
  int temp;  /* register temp if RATA_TEMP */
  int next;  /* next candidate */
  int count; /* number of uses */
  int oload; /* other load ili for the same variable */
  int ocand; /* other load candidate that produces the same value */
  int rat;   /* RAT for this candidate */
} RCAND;

/*****  Storage Information for RCAND  *****/

typedef struct {
  RCAND *stg_base;
  int stg_avail;
  int stg_size;
  int count;      /* count of a candidate   */
  int weight;     /* value used to increment the count of a
                   * candidate  */
  int static_cnt; /* count of statics in a function; init'd
                   * by reg_init.  */
  int const_cnt;  /* count of constants in a function */
  int kr;         /* any KR loads, stores, constants */
} RCANDB;

/*****  Macros for RCAND  *****/

#define RCAND_RTYPE(i) rcandb.stg_base[i].rtype
#define RCAND_ATYPE(i) rcandb.stg_base[i].atype
#define RCAND_FLAGS(i) rcandb.stg_base[i].flags.all
#define RCAND_CONFL(i) rcandb.stg_base[i].flags.bits.confl
#define RCAND_STORE(i) rcandb.stg_base[i].flags.bits.store
#define RCAND_CSE(i) rcandb.stg_base[i].flags.bits.cse
#define RCAND_OK(i) rcandb.stg_base[i].flags.bits.ok
#define RCAND_NOREG(i) rcandb.stg_base[i].flags.bits.noreg
#define RCAND_IGNORE(i) rcandb.stg_base[i].flags.bits.ignore
#define RCAND_EINT(i) rcandb.stg_base[i].flags.bits.eint
#define RCAND_VINT(i) rcandb.stg_base[i].flags.bits.vint
#define RCAND_INV(i) rcandb.stg_base[i].flags.bits.inv
#define RCAND_TINV(i) rcandb.stg_base[i].flags.bits.tinv
#define RCAND_MSIZE(i) rcandb.stg_base[i].msize
#define RCAND_VAL(i) rcandb.stg_base[i].val
#define RCAND_TEMP(i) rcandb.stg_base[i].temp
#define RCAND_NEXT(i) rcandb.stg_base[i].next
#define RCAND_COUNT(i) rcandb.stg_base[i].count
#define RCAND_OLOAD(i) rcandb.stg_base[i].oload
#define RCAND_OCAND(i) rcandb.stg_base[i].ocand
#define RCAND_RAT(i) rcandb.stg_base[i].rat
#define RCAND_ISNME(i) (RCAND_ATYPE(i) == RATA_NME)
#define RCAND_ISCONST(i) (RCAND_ATYPE(i) == RATA_CONST)
#define RCAND_ISILI(i) (RCAND_ATYPE(i) == RATA_ILI)
#define RCAND_ISTEMP(i) (RCAND_ATYPE(i) == RATA_TEMP)
#define RCAND_ISUPLV(i) (RCAND_ATYPE(i) == RATA_UPLV)

#define MAXRCAND 65254
#define GET_RCAND(i)                                                \
  {                                                                 \
    i = rcandb.stg_avail++;                                         \
    if (rcandb.stg_avail > MAXRCAND)                                \
      error(7, 4, 0, CNULL, CNULL);                                 \
    NEED(rcandb.stg_avail, rcandb.stg_base, RCAND, rcandb.stg_size, \
         rcandb.stg_size + 100);                                    \
    if (rcandb.stg_base == NULL)                                    \
      error(7, 4, 0, CNULL, CNULL);                                 \
    RCAND_FLAGS(i) = 0;                                             \
    RCAND_OCAND(i) = 0;                                             \
    RCAND_RAT(i) = 0;                                               \
  }

/*****  Symbol table stuff relevant to the assignment of registers  *****/
/*****  These are used throughout the expander and optimizer        *****/

/* FTN's storage classes */
#define IS_LCL(s) (SCG(s) == SC_LOCAL || SCG(s) == SC_PRIVATE)
#define IS_EXTERN(s) (SC_ISCMBLK(SCG(s)) || SCG(s) == SC_EXTERN)
#define IS_STATIC(s) (SCG(s) == SC_STATIC)
#define IS_CMNBLK(s) (SC_ISCMBLK(SCG(s)))
#define IS_DUM(s) (SCG(s) == SC_DUMMY)
#define IS_LCL_OR_DUM(s) (IS_LCL(s) || IS_DUM(s))
#define IS_REGARG(s) (REGARGG(s) && REDUCG(s))

#define IS_PRIVATE(s) (SCG(s) == SC_PRIVATE)

/* macros used to access register defining/moving ili */
#define RTYPE_DF(rtype) (il_rtype_df[rtype])
#define MV_RTYPE(rtype) (il_mv_rtype[rtype])

/*****  External Data Declarations  *****/

extern RATB ratb;
extern RCANDB rcandb;

extern int il_rtype_df[RATA_RTYPES_TOTAL];
extern int il_mv_rtype[RATA_RTYPES_TOTAL];

/*****  Function Declarations (defined in regutil.c)  *****/

extern void reg_init(int);
extern void addrcand(int);
#if DEBUG
extern void dmprcand(void);
extern void dmprat(int);
#endif
extern int getrcand(int);
extern void endrcand(void);
extern void storedums(int, int);
extern void mkrtemp_init(void);
extern SPTR mkrtemp(int);
extern SPTR mkrtemp_sc(int, SC_KIND);
extern SPTR mkrtemp_cpx(DTYPE);
extern SPTR mkrtemp_cpx_sc(DTYPE, SC_KIND);
extern SPTR mkrtemp_arg1_sc(DTYPE, SC_KIND);
extern void mkrtemp_end(void);
extern void mkrtemp_copy(int *);
extern void mkrtemp_update(int *);
extern void mkrtemp_reinit(int *);
extern int assn_rtemp(int);
extern void assn_input_rtemp(int, int);
extern int assn_rtemp_sc(int, SC_KIND);
extern int assn_sclrtemp(int, SC_KIND);
extern void dmp_rat(int);
