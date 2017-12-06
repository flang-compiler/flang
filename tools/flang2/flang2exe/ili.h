/*
 * Copyright (c) 1994-2017, NVIDIA CORPORATION.  All rights reserved.
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

#ifndef ILI_H_
#define ILI_H_

#ifndef ILITP_UTIL  /* don't include if building ilitp utility prog*/
#include "iliatt.h" /* defines ILI_OP */
#endif

#include "atomic_common.h"

/** \file
 * \brief ILI header file  -  x86-64 version.
 */
#ifndef MAX_OPNDS
#define MAX_OPNDS 5 /* Max number of operands for an ili */
#endif

/***** ILI Declarations *****/

typedef struct {
  unsigned short opc; /**< Logically an ILI_OP */
  /* practically all hosts will insert 2 bytes of padding here. */
  int hshlnk;
  int count;
  int visit;
  int alt;
  int opnd[MAX_OPNDS];
  int vlist; /* linked list of ILI visited by a traversal */
  int tndx;
  int tndx2;
  int lili; /* CG: set to the ILI's linear ILI.  This field must be 0
             * on entry to function 'schedule()'. */
} ILI;

typedef struct {
  STG_MEMBERS(ILI);
} ILIB;

#define ILI_REPL(i) ilib.stg_base[i].count
#define ILI_OPC(i) ilib.stg_base[i].opc
#define ILI_HSHLNK(i) ilib.stg_base[i].hshlnk
#define ILI_VISIT(i) ilib.stg_base[i].visit
#define ILI_ALT(i) ilib.stg_base[i].alt
#define ILI_COUNT(i) ilib.stg_base[i].count
#define ILI_RAT(i) ilib.stg_base[i].count
#define ILI_OPND(i, opn) ilib.stg_base[i].opnd[(opn)-1]
#define ILI_VLIST(i) ilib.stg_base[i].vlist
#define ILI_TNDX(i) ilib.stg_base[i].tndx
#define ILI_TNDX2(i) ilib.stg_base[i].tndx2
#define ILI_LILI(i) ilib.stg_base[i].lili

/***** ILI Attributes Declarations *****/

typedef struct {
  char *name;          /* ili name */
  char *opcod;         /* machine instruction mnemonic (CG only) */
  short oprs;          /* number of operands */
  unsigned short attr; /* AT attributes. e.g. cse, dom,  -
                        * Field size (right to left):
                        *   4 -- IL_TYPE
                        *   1 -- IL_COMM
                        *   5 -- IL_RES
                        *   2 -- IL_DOM/CSEG
                        *   1 -- IL_SSENME
                        *   1 -- IL_VECT
                        * ------------------
                        *  14 -- total
                        */
  /* x86-64 code generator info:
   */
  unsigned notCG : 1;
  unsigned CGonly : 1;
  unsigned notAILI : 1;
  unsigned terminal : 1;
  unsigned move : 1;
  unsigned memdest : 1;
  unsigned ccarith : 1;
  unsigned cclogical : 1;
  unsigned ccmod : 1;
  unsigned shiftop : 1;
  unsigned memarg : 1;
  unsigned ssedp : 1;
  unsigned ssest : 1;
  unsigned conditional_branch : 1;
  unsigned sse_avx : 1;      /* ILI can generate SSE or AVX instructions */
  unsigned avx_only : 1;     /* ILI can only generate AVX instructions */
  unsigned avx_special : 1;  /* AVX version is a special case */
  unsigned avx3_special : 1; /* AVX3 version is a special case */
  unsigned asm_special : 1;
  unsigned asm_nop : 1;
  unsigned accel : 1;

  unsigned short replaceby;
  char size; /* can be 'b', 'w', 'l', 'q' or 'y', or 0 if unspecified */

  char oprflag[MAX_OPNDS]; /* ILIO_ type of each opnd.  See IL_OPRFLAG */
} ILIINFO;

typedef enum ILIO_KIND {
  ILIO_NULL = 0,
  ILIO_SYM = 1,
  ILIO_STC = 4,
  ILIO_OFF = 5,
  ILIO_NME = 6,
  ILIO_IR = 7,
  ILIO_SP = 8,
  ILIO_DP = 9,
  ILIO_CS = 10,
  ILIO_CD = 11,
  ILIO_AR = 12,
  ILIO_KR = 13,
  ILIO_XMM = 14, /* xmm register number */
  ILIO_X87 = 15,
  ILIO_DOUBLEDOUBLE = 16,
  ILIO_FLOAT128 = 17,
  ILIO_LNK = 18,
  ILIO_IRLNK = 19,
  ILIO_SPLNK = 20,
  ILIO_DPLNK = 21,
  ILIO_ARLNK = 22,
  ILIO_KRLNK = 23,
  ILIO_QPLNK = 24,
  ILIO_CSLNK = 25,
  ILIO_CDLNK = 26,
  ILIO_CQLNK = 27,
  ILIO_128LNK = 28,
  ILIO_256LNK = 29,
  ILIO_512LNK = 30,
  ILIO_X87LNK = 31,
  ILIO_DOUBLEDOUBLELNK = 32,
  ILIO_FLOAT128LNK = 33
} ILIO_KIND;

#define ILIO_MAX 33
#define ILIO_ISLINK(n) ((n) >= ILIO_IRLNK)

/* Reflexive defines */
#define ILIO_NULL ILIO_NULL
#define ILIO_SYM ILIO_SYM
#define ILIO_STC ILIO_STC
#define ILIO_OFF ILIO_OFF
#define ILIO_NME ILIO_NME
#define ILIO_IR ILIO_IR
#define ILIO_SP ILIO_SP
#define ILIO_DP ILIO_DP
#define ILIO_CS ILIO_CS
#define ILIO_CD ILIO_CD
#define ILIO_AR ILIO_AR
#define ILIO_KR ILIO_KR
#define ILIO_XMM ILIO_XMM
#define ILIO_X87 ILIO_X87
#define ILIO_DOUBLEDOUBLE ILIO_DOUBLEDOUBLE
#define ILIO_FLOAT128 ILIO_FLOAT128
#define ILIO_LNK ILIO_LNK
#define ILIO_IRLNK ILIO_IRLNK
#define ILIO_SPLNK ILIO_SPLNK
#define ILIO_DPLNK ILIO_DPLNK
#define ILIO_ARLNK ILIO_ARLNK
#define ILIO_KRLNK ILIO_KRLNK
#define ILIO_QPLNK ILIO_QPLNK
#define ILIO_CSLNK ILIO_CSLNK
#define ILIO_CDLNK ILIO_CDLNK
#define ILIO_CQLNK ILIO_CQLNK
#define ILIO_128LNK ILIO_128LNK
#define ILIO_256LNK ILIO_256LNK
#define ILIO_512LNK ILIO_512LNK
#define ILIO_X87LNK ILIO_X87LNK
#define ILIO_DOUBLEDOUBLELNK ILIO_DOUBLEDOUBLELNK
#define ILIO_FLOAT128LNK ILIO_FLOAT128LNK

/* ILIINFO.attr field definitions. */
#define ILIA_NULL 0

#define ILIA_COMM 1 /* comm field */

/* result type field */
typedef enum ILIA_RESULT {
  ILIA_TRM = 0,
  ILIA_LNK = 1,
  ILIA_IR = 2,
  ILIA_SP = 3,
  ILIA_DP = 4,
  ILIA_AR = 5,
  ILIA_KR = 6,
  ILIA_CC = 7,
  ILIA_FCC = 8,
  ILIA_QP = 9,
  ILIA_CS = 10,
  ILIA_CD = 11,
  ILIA_CQ = 12,
  ILIA_128 = 13,
  ILIA_256 = 14,
  ILIA_512 = 15,
  ILIA_X87 = 16,
  ILIA_DOUBLEDOUBLE = 17,
  ILIA_FLOAT128 = 18
} ILIA_RESULT;

#define ILIA_MAX 18

/* Reflexive defines */
#define ILIA_TRM ILIA_TRM
#define ILIA_LNK ILIA_LNK
#define ILIA_IR ILIA_IR
#define ILIA_SP ILIA_SP
#define ILIA_DP ILIA_DP
#define ILIA_AR ILIA_AR
#define ILIA_KR ILIA_KR
#define ILIA_CC ILIA_CC
#define ILIA_FCC ILIA_FCC
#define ILIA_QP ILIA_QP
#define ILIA_CS ILIA_CS
#define ILIA_CD ILIA_CD
#define ILIA_CQ ILIA_CQ
#define ILIA_128 ILIA_128
#define ILIA_256 ILIA_256
#define ILIA_512 ILIA_512
#define ILIA_X87 ILIA_X87
#define ILIA_DOUBLEDOUBLE ILIA_DOUBLEDOUBLE
#define ILIA_FLOAT128 ILIA_FLOAT128

#define ILIA_DOM 1 /* dom/cse field */
#define ILIA_CSE 2

/* Macros use the IL_RES(opc) as a value */
#define ILIA_ISIR(t) ((t) == ILIA_IR)
#define ILIA_ISSP(t) ((t) == ILIA_SP)
#define ILIA_ISDP(t) ((t) == ILIA_DP)
#define ILIA_ISAR(t) ((t) == ILIA_AR)
#define ILIA_ISKR(t) ((t) == ILIA_KR)
#define ILIA_ISCS(t) ((t) == ILIA_CS)
#define ILIA_ISCD(t) ((t) == ILIA_CD)

/* *** operand type:    ILIO_... e.g. ILIO_DPLNK */
#define IL_OPRFLAG(opcode, opn) (ilis[opcode].oprflag[opn - 1])

#define IL_OPRS(opc) (ilis[opc].oprs)
#define IL_NAME(opc) (ilis[opc].name)
#define IL_MNEMONIC(opc) (ilis[opc].opcod)
#define IL_ISLINK(i, opn) (IL_OPRFLAG(i, opn) >= ILIO_LNK)

/* *** operation type:  ILTY_... e.g. ILTY_ARTH  */
#define IL_TYPE(idx) (ilis[(idx)].attr & 0xf)

#define IL_COMM(i) ((ilis[i].attr >> 4) & 0x1)    /* Yields ILIA_COMM or 0    */
#define IL_RES(i) \
  ((ILIA_RESULT)((ilis[i].attr >> 5) & 0x1f))     /* Yields ILIA_TRM..ILIA_AR */
#define IL_LNK(i) ((ilis[i].attr >> 5) & 0x1f)    /* Yields ILIA_LNK or 0     */
#define IL_DOM(i) ((ilis[i].attr >> 10) & 0x3)    /* Yields ILIA_DOM or 0     */
#define IL_CSEG(i) ((ilis[i].attr >> 10) & 0x3)   /* Yields ILIA_CSE or 0     */
#define IL_IATYPE(i) ((ilis[i].attr >> 10) & 0x3) /* ILIA_DOM, ILIA_CSE or 0*/
#define IL_SSENME(i) ((ilis[i].attr >> 12) & 0x1) /* Yields 1 or 0 */
#define IL_VECT(i) ((ilis[i].attr >> 13) & 0x1)   /* Yields 1 or 0 */
/* Can this operation have a memory fence? */
#define IL_HAS_FENCE(i) (((ilis[i].attr >> 14) & 3) != 0)
/* Is this operation an IL_ATOMICRMWx? */
#define IL_IS_ATOMICRMW(i) (((ilis[i].attr >> 14) & 0x3) == 2)
/* Is this operation an IL_CMPXCHGx? */
#define IL_IS_CMPXCHG(i) (((ilis[i].attr >> 14) & 0x3) == 3)
/* Does this operation perform an atomic update?
   IL_OPND(2) is the address of the operand to be updated. */
#define IL_IS_ATOMIC_UPDATE(i) (((ilis[i].attr >> 14) & 0x3) >= 2)

typedef enum ILTY_KIND {
  ILTY_NULL = 0,
  ILTY_ARTH = 1,
  ILTY_BRANCH = 2,
  ILTY_CONS = 3,
  ILTY_DEFINE = 4,
  ILTY_LOAD = 5,
  ILTY_MOVE = 6,
  ILTY_OTHER = 7,
  ILTY_PROC = 8,
  ILTY_STORE = 9,
  ILTY_PLOAD = 10,
  ILTY_PSTORE = 11
} ILTY_KIND;

/* Reflexive defines for values inspected by #ifdef. */
#define ILTY_PLOAD ILTY_PLOAD
#define ILTY_PSTORE ILTY_PSTORE

/* Standard offsets for various register set references. */
#define IR_OFFSET 0
#define SP_OFFSET 1
#define DP_OFFSET 2
#define AR_OFFSET 3

/***** Values of conditions in relationals *****/

typedef enum CC_RELATION {
  CC_EQ = 1,
  CC_NE = 2,
  CC_LT = 3,
  CC_GE = 4,
  CC_LE = 5,
  CC_GT = 6,
  CC_NOTEQ = 7,
  CC_NOTNE = 8,
  CC_NOTLT = 9,
  CC_NOTGE = 10,
  CC_NOTLE = 11,
  CC_NOTGT = 12,
  /* CC values are sometimes negated to denote IEEE floating-point relations.
     The -12 here is a "strut" to ensure that the enum's underlying integral
     type is signed. */
  CC_IEEE_NOTGT = -12
} CC_RELATION;

/* Let subsequent headers know that CC_RELATION is available. */
#define CC_RELATION_IS_DEFINED 1

#define NEW_FMA /* ...to generate FMA3 or FMA4 instructions */

/* The following flags are used in the 'stc' operand of an FMATYPE
 * ILI to describe an FMA instruction.  The FMA operation is:
 *	dest = <sign> (factor1 * factor2)  <addop>  term
 */
#define FMA_MINUS_PROD 1          /* if set <sign> is -, otherwise it's + */
#define FMA_MINUS_TERM 2          /* if set <addop> is -, otherwise it's + */
#define FMA_DEST_IS_FACTOR1 4     /* used for FMA3 */
#define FMA_DEST_IS_TERM 8        /* used for FMA3 & packed reduction FMAs */
#define FMA_MEMOP_IS_FACTOR2 0x10 /* used for [DS]FMA and P[DS]FMA ILIs */
#define FMA_MEMOP_IS_TERM 0x20    /*   "     "     "     "     "     "  */
#define FMA_GEN_FMA3_132 0x40
#define FMA_GEN_FMA3_213 0x80
#define FMA_GEN_FMA3_231 0x100
#define FMA_GEN_FMA4 0x200

/********************************************************************
 * JHM (7 April 2014): The following #define is necessary for
 * compiling comp.shared/llvm/src/llvect.c.  Delete it when possible.
 *******************************************************************/
#define FMA_DEST_IS_SRC1 FMA_DEST_IS_FACTOR1

/* The following flags are used in the 'stc' operand of VEXTRACT and
 * VINSERT ILIs to specify which 'vextract...' or 'vinsert...'
 * instruction to use.
 */
#define SUF_f128 0x10    /* only used in AVX instructions */
#define SUF_f32x4 0x20   /*   "   "   "  AVX3   "    "    */
#define SUF_f32x8 0x40   /*   "   "   "    "    "    "    */
#define SUF_f64x2 0x80   /*   "   "   "    "    "    "    */
#define SUF_f64x4 0x100  /*   "   "   "    "    "    "    */
#define SUF_i128 0x200   /*   "   "   "  AVX2   "    "    */
#define SUF_i32x4 0x400  /*   "   "   "  AVX3   "    "    */
#define SUF_i32x8 0x800  /*   "   "   "    "    "    "    */
#define SUF_i64x2 0x1000 /*   "   "   "    "    "    "    */
#define SUF_i64x4 0x2000 /*   "   "   "    "    "    "    */

/*
 * Memory reference size/type codes.
 *
 * Legacy assumptions observed:
 *   -  (code & 3) < 2 if and only if the type size is 1 or 2 bytes.
 *   -  (code & 3) == log2(type size) if the type size is 1, 2, 4, or 8 bytes.
 */
typedef enum MSZ {
  MSZ_SBYTE = 0x00,  /* signed byte */
  MSZ_SHWORD = 0x01, /* signed 16-bit short */
  MSZ_UBYTE = 0x04,  /* unsigned byte */
  MSZ_UHWORD = 0x05, /* unsigned 16-bit short */

  /* Codes for types larger than two bytes. These are all distinct values
   * suitable for use as case labels in switches.  The holes in this sequence
   * of code values avoid violating the first legacy assumption described above.
   */
  MSZ_SWORD = 0x02,  /* signed 32-bit int */
  MSZ_SLWORD = 0x03, /* signed 64-bit long */
  MSZ_UWORD = 0x06,  /* unsigned 32-bit int */
  MSZ_ULWORD = 0x07, /* unsigned 64-bit long */
  MSZ_FWORD = 0x0a,  /* 32-bit single precision float */
  MSZ_FLWORD = 0x0b, /* 64-bit double precision float */
  MSZ_I8 = 0x0f,     /* distinct 64-bit integer type */
  MSZ_PTR = 0x13,    /* distinct 64-bit pointer type */
  MSZ_F10 = 0x16,    /* X87 FPU 80-bit extended precision */
  MSZ_F16 = 0x17,    /* 128-bit quad precision float */
  MSZ_F32 = 0x1a,    /* 256-bit float */
  MSZ_F8x2 = 0x1b,   /* 128-bit double-double float */

  MSZ_UNDEF = 0xff, /* undefined MSZ code */
} MSZ;

#define MSZ_TO_BYTES                                                  \
  {                                                                   \
    1 /* SBYTE */, 2 /* SHWORD */, 4 /* SWORD */, 8 /* SLWORD */,     \
        1 /* UBYTE */, 2 /* UHWORD */, 4 /* UWORD */, 8 /* ULWORD */, \
        0 /* 0x08  */, 0 /* 0x09   */, 4 /* FWORD */, 8 /* FLWORD */, \
        0 /* 0x0c  */, 0 /* 0x0d   */, 0 /* 0x0e  */, 8 /* I8     */, \
        0 /* 0x10  */, 0 /* 0x11   */, 0 /* 0x12  */, 8 /* PTR    */, \
        0 /* 0x14  */, 0 /* 0x15   */, 16 /* F10  */, 16 /* F16   */, \
        0 /* 0x18  */, 0 /* 0x19   */, 32 /* F32  */, 16 /* F8x2  */, \
        0 /* 0x1c  */, 0 /* 0x1d   */, 0 /* 0x1e  */, 0 /* 0x1f   */  \
  }

/* Reflexive defines for values that are inspected by preprocessor directives */
#define MSZ_F10 MSZ_F10
#define MSZ_I8 MSZ_I8
#define MSZ_SLWORD MSZ_SLWORD
#define MSZ_ULWORD MSZ_ULWORD
#define MSZ_UWORD MSZ_UWORD

/* Synonyms (beware conflicting case values) */
#define MSZ_WORD MSZ_SWORD
#define MSZ_BYTE MSZ_UBYTE
#define MSZ_F4 MSZ_FWORD
#define MSZ_F8 MSZ_FLWORD
#define MSZ_DBLE MSZ_FLWORD
#define MSZ_DFLWORD MSZ_FLWORD
#define MSZ_DSLWORD MSZ_SLWORD

typedef struct {
  unsigned int latency; /* ST | LD | R/R | R/M */
  unsigned int attrs;   /* ST | LD | R/M | R/R */
} SCHINFO;

#define P_FADD 0x01
#define P_FMUL 0x02
#define P_FST 0x04
#define DEC_DIR 0x10
#define DEC_DBL 0x20
#define DEC_VEC 0x40

#define ST_SHIFT 24
#define LD_SHIFT 16
#define RM_SHIFT 8
#define RR_SHIFT 0

#define SCH_ATTR(i) (schinfo[(i)].attrs)
#define SCH_LAT(i) (schinfo[(i)].latency)

/*****  ILI External Data Declarations *****/

extern ILIB ilib;
extern ILIINFO ilis[];

#ifndef ILITP_UTIL
extern LOGICAL share_proc_ili; /* defd in iliutil.c */
extern LOGICAL share_qjsr_ili; /* defd in iliutil.c */

/*  declare external functions iliutil.c, unless building ilitp utility prog */
void ili_init(void);
int ili_traverse(int (*)(), int);
void ili_visit(int, int);
void ili_unvisit(void);
void prilitree(int i); /* iliutil.c */
void garbage_collect(void (*mark_function)());

int jsrsearch(int);
int qjsrsearch(int);

int addili(ILI *);
int get_ili_ns(ILI *);
int ad1ili(ILI_OP, int);
int ad2ili(ILI_OP, int, int);
int ad3ili(ILI_OP, int, int, int);
int ad4ili(ILI_OP, int, int, int, int);
int ad5ili(ILI_OP, int, int, int, int, int);
int ad_cse(int);
int has_cse(int ilix);
int ad_icon(INT);
int ad_kcon(INT, INT);
int ad_kconi(ISZ_T);
int ad_aconi(ISZ_T);
int ad_acon(int, ISZ_T);
int ad_aconk(INT, INT);
int ad_load(int);
int ad_free(int);
ILI_OP ldopc_from_stopc(ILI_OP);
ISZ_T get_isz_conili(int);

int ili_opnd(int, int);
int mk_address(int);
int compute_address(int);

int sel_icnst(ISZ_T, int);
int sel_iconv(int, int);
int sel_decr(int, int);
int sel_aconv(int);

int is_argili_opcode(ILI_OP);
int is_cseili_opcode(ILI_OP);
int is_freeili_opcode(ILI_OP);
int is_mvili_opcode(ILI_OP);
int is_rgdfili_opcode(ILI_OP);
int is_daili_opcode(ILI_OP);
int is_dfrili_opcode(ILI_OP);
int is_integer_comparison_opcode(ILI_OP);  /* includes conditional jumps */
int is_floating_comparison_opcode(ILI_OP); /* ditto */
int is_unsigned_opcode(ILI_OP);            /* ditto */

DTYPE ili_get_vect_dtype(int);

int ili_subscript(int);
int ili_isdeleted(int);
int ili_throw_label(int);
int uikmove(int);
int ikmove(int);
int kimove(int);
void initcallargs(int count);
void addcallarg(int ili, int nme, int dtype);
int gencallargs(void);
char *gnr_math(char *, int, int, char *, int);
char *fast_math(char *, int, int, char *);
char *relaxed_math(char *, int, int, char *);
int mkfunc_avx(char *, int);
void rm_smove(void);

void dump_ili(FILE *, int);

#define XBIT_NEW_MATH_NAMES XBIT(164, 0x800000)
/* the following macro is for experimenting with the new method for certain
 * complex operations/intrinsics -- when complete, just drop _CMPLX from the 
 * use(s).
 */
#define XBIT_NEW_MATH_NAMES_CMPLX (XBIT_NEW_MATH_NAMES && XBIT(26,1))

#define XBIT_NEW_RELAXEDMATH XBIT(15, 0x400)

#define XBIT_VECTORABI_FOR_SCALAR XBIT(26,2)

/* Complements a relation; also known as negation or inversion.
 *  complement_int_cc(CC_LT) -> CC_GE
 *  complement_ieee_cc(CC_LT) -> CC_NOTLT
 */
CC_RELATION complement_int_cc(CC_RELATION cc);
CC_RELATION complement_ieee_cc(CC_RELATION cc);

/* Commutes a relation to correspond to an exchange of its operands.
 *  commute_cc(CC_LT) -> CC_GT
 */
CC_RELATION commute_cc(CC_RELATION cc);

/* Reduces a comparison of two operands whose result is compared with zero
 * into a single comparison of the two operands.
 *  combine_*_ccs(CC_x, CC_NE or CC_GT) -> CC_x
 *  combine_*_ccs(CC_x, CC_EQ or CC_LE) -> complement_*_cc(CC_x)
 *  combine_*_ccs(CC_x, CC_LT or CC_GE) -> 0
 */
CC_RELATION combine_int_ccs(CC_RELATION binary_cc, CC_RELATION zero_cc);
CC_RELATION combine_ieee_ccs(CC_RELATION binary_cc, CC_RELATION zero_cc);

/* Predicate: if two operands were equal, would that satisfy a condition? */
bool cc_includes_equality(CC_RELATION cc);

/* Predicate: are two condition codes complements of each other? */
bool ccs_are_complementary(CC_RELATION cc1, CC_RELATION cc2);

int ll_ad_outlined_func(ILI_OP, ILI_OP, char *, int, int, int, int);

#ifdef DEBUG
void dmpili(void);
void dmpilitree(int i);
void _ddilitree(int i, int flag);
#endif

MSZ mem_size(TY_KIND ty);
int rewr_ili_nme(int tree, int oldili, int newili, int oldnme, int newnme,
                 int douse, int dodef);
extern int rewr_ili(int, int, int);
extern void rewr_cln_ili(void);

/* iliutil.h */
int find_ili(int tree, int it);
int genretvalue(int ilix, ILI_OP resultopc);

/*****  ILT, BIH, NME  declarations  *****/
#include "ilt.h"
#include "bih.h"
#include "nme.h"
LOGICAL qjsr_in(int ilix);
int alt_qjsr(int ilix);
LOGICAL find_ili(int tree, int it);
LOGICAL is_llvm_local_private(int sptr);
int mk_charlen_parref_sptr(int);

/***** Atomic Operation Encodings *****/
int atomic_encode(MSZ msz, SYNC_SCOPE scope, ATOMIC_ORIGIN origin);
int atomic_encode_rmw(MSZ msz, SYNC_SCOPE scope, ATOMIC_ORIGIN origin,
                      ATOMIC_RMW_OP op);
MEMORY_ORDER memory_order(int ilix);
ATOMIC_INFO atomic_info(int ilix);
extern LOGICAL is_omp_atomic_ld(int);
extern LOGICAL is_omp_atomic_st(int);

ATOMIC_INFO atomic_decode(int encoding);
int atomic_info_index(ILI_OP opc);

/* compare-exchange requires 8 inputs.  To avoid having to allow 8-operand ILI
   operations, it's broken into 2 ILIs, an IL_CMPXCHGx on top of an
   IL_CMPXCHG_DST.  Clients should avoid creating or referencing the 
   IL_CMPXCHG_DST instruction directly, and instead use the interfaces below.*/

bool cmpxchg_is_weak(int ilix);
int cmpxchg_loc(int ilix);
int ad_cmpxchg(ILI_OP opc, int ilix_val, int ilix_loc, int nme,
               int stc_atomic_info, int ilix_comparand, int ilix_is_weak,
               int ilix_sucess, int ilix_failure);

CMPXCHG_MEMORY_ORDER cmpxchg_memory_order(int ilix);

/* Extract MSZ from an int that is a MSZ operand or an encoded ATOMIC_INFO.
   This functionality is handy for extracting the MSZ from an instruction
   that might be a plain load/store or atomic/load/store. */ 
#define ILI_MSZ_FROM_STC(x) ((MSZ)(x)&0xFF)

/* Get MSZ of an IL_LD or IL_ATOMICLDx instruction */
#define ILI_MSZ_OF_LD(ilix) (ILI_MSZ_FROM_STC(ILI_OPND((ilix), 3)))

/* Get MSZ of an IL_ST, IL_STSP, IL_STDP, or IL_ATOMICSTx instruction */
#define ILI_MSZ_OF_ST(ilix) (ILI_MSZ_FROM_STC(ILI_OPND((ilix), 4)))

int imul_const_ili(ISZ_T valconst, int valilix);
int imul_ili_ili(int leftx, int rightx);
int iadd_const_ili(ISZ_T valconst, int valilix);
int iadd_ili_ili(int leftx, int rightx);
int isub_ili_ili(int leftx, int rightx);
int idiv_ili_const(int valilix, ISZ_T valconst);
int idiv_ili_ili(int leftx, int rightx);
int imax_ili_ili(int leftx, int rightx);
int imin_ili_ili(int leftx, int rightx);

#endif /* !defined(ILITP_UTIL) */
#endif /* ILI_H_ */
