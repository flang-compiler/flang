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
 * \brief Definitions and declarations global to Hammer Code Generator
 */

/*======================================
 * Part 1: Macros used by Code Generator
 *====================================*/

/*----------------------------
 * Set target dependent values
 *--------------------------*/

/* Leave CG32BIT, IS_X86_32, ... undefined. */

#if defined(TARGET_WIN)
#define WINDOWS_ABI TRUE
#else
#define WINDOWS_ABI FALSE
#endif

#define GENERATE_AVX    TEST_FEATURE(FEATURE_AVX)
#define GENERATE_AVX2   TEST_FEATURE(FEATURE_AVX2)
#define GENERATE_AVX3   TEST_FEATURE(FEATURE_AVX512F)
#define GENERATE_AVX32  TEST_FEATURE(FEATURE_AVX512VL)
#define HAS_FMA3        TEST_FEATURE(FEATURE_FMA3)
#define HAS_FMA4        TEST_FEATURE(FEATURE_FMA4)
#define HAS_FMA         (HAS_FMA3 || HAS_FMA4)

/*-----------------------------------------------
 * Define an additional assert macro, 'asrt()'
 *----------------------------------------------*/

#if DEBUG
#define asrt(c) \
  if (c)        \
    ;           \
  else          \
  fprintf(stderr, "asrt failed. line %d, file %s\n", __LINE__, __FILE__)
#else
#define asrt(c)
#endif

#define IL(li) (cg.lili_base + (li)) /* linear ILI access */

#define FIRST_AILI(bih) cg.bih_info[bih].first_aili

#define IL_OPMASK_LD    IL_LD    /* temporary */
#define IL_OPMASK_ST    IL_ST    /* temporary */

/*---------------------------------------------------
 * Support for 64-bit integers (via scutil functions)
 *-------------------------------------------------*/

#define SYM64_TO_INT64(sptr, val64) \
  {                                 \
    val64[0] = CONVAL1G(sptr);      \
    val64[1] = CONVAL2G(sptr);      \
  }

#define SCALAR32_TO_INT64(v, val64) \
  {                                 \
    val64[1] = (v);                 \
    if ((v) >= 0)                   \
      val64[0] = 0;                 \
    else                            \
      val64[0] = ~0;                \
  }

#define INT64_IS_ZERO(val64) (val64[1] == 0 && val64[0] == 0)

#define IS_EXTERN(sptr) (SCG(sptr) == SC_CMBLK || SCG(sptr) == SC_EXTERN)

#define IS_STATIC(sptr) (SCG(sptr) == SC_STATIC || STYPEG(sptr) == ST_CONST)

/* The following macro was introduced to fix Flyspray 23661.
 * N.B.: If we define non-commutative versions of the ILI opcodes
 * IL_[DF]{MAX,MIN} then references to this macro can be replaced by
 * just "IL_COMM(opc)".
 */
#define CAN_COMMUTE_OPNDS(opc)  (IL_COMM(opc)      && \
                                 (opc) != IL_DMAX  && \
                                 (opc) != IL_DMIN  && \
                                 (opc) != IL_FMAX  && \
                                 (opc) != IL_FMIN)

/*---------------------------------------------------------------
 * Value for aili size field, and 3rd argument of 'asm_operand()'
 *-------------------------------------------------------------*/

#define NOSIZE 0

/*------------------------------------------------------
 * Define the 3 getitem areas used by the Code Generator
 *----------------------------------------------------*/

/* 'CG_LONGTERM_AREA' is freed just once, at the end of the code
 * generation for each user function.
 */
#define CG_LONGTERM_AREA 10

/* 'CG_MEDTERM_AREA' is freed:
 * -- immediately before the cgoptim2 phase;
 * -- at the end of cgoptim2 phase, before cgassem.
 */
#define CG_MEDTERM_AREA 6

/* 'CG_SHORTTERM_AREA' is freed:
 * -- at the end of the linearize/cgoptim1/genaili phase for each block;
 * -- at certain points during OPT2 register allocation.
 */
#define CG_SHORTTERM_AREA 11

/*--------------------------------------------------------------------
 * Defines for the unsigned condition codes; the signed condition
 * codes are already defined in ili.h.  These values are used to index
 * 'cond_txt[]'.
 *------------------------------------------------------------------*/

#define CC_B     7
#define CC_NB    8
#define CC_BE    9
#define CC_NBE  10
#define CC_P    11
#define CC_NP   12

/*-----------------------------------------------------------
 * Defines for x-flags related to CG and register allocation.
 *---------------------------------------------------------*/

#define XBIT_SAVE_ALL_GP_REGS   XBIT(164, 1)    /* #pragma save_all_gp_regs */
#define XBIT_SAVE_ALL_REGS      XBIT(164, 2)    /* #pragma save_all_regs */
#define XBIT_SAVE_USED_GP_REGS  XBIT(164, 4)    /* #pragma save_used_gp_regs */
#define XBIT_SAVE_USED_REGS     XBIT(164, 8)    /* #pragma save_used_regs */

#define XBIT_TREGION_CSE        XBIT(135, 0x800000)
#define XBIT_CAND_SCORE_OPT     (XBIT(135, 0x4000000) || flg.opt >= 4)
#define XBIT_LOOP_PRESSURE_MOD  (! XBIT(164, 0x40) || flg.opt >= 4)

#define XBIT_NEW_PRE                    (XBIT(164, 0x1000) && flg.opt >= 2)
#define XBIT_CAN_ADD_BLOCKS_AFTER_EXIT  (! XBIT(164, 0x20000))
#define XBIT_IKCON_LILI_PEEPHOLE_OPTS   (! XBIT(164, 0x10000000))

#define XBIT_GENERATE_SCALAR_FMA        (! XBIT(164, 0x40000000) && HAS_FMA)

#define XBIT_MCACHE_ALIGN       XBIT(119, 0x10000000)    /* -Mcache_align */

#define LIST_END  (-1)

/*------------------------------------------------------------------
 * Need a bih flag for register allocation live interval computation
 *----------------------------------------------------------------*/

#define VISITED(bih)  BIH_PL(bih)

#define INTERVAL_CONFLICT(a, b)                      \
  ((a)->first_use->number < (b)->last_use->number && \
   (a)->last_use->number > (b)->first_use->number)

#define INTERVAL_CONFLICT2(af, al, bf, bl) \
  (af->number < bl->number && al->number > bf->number)

/*-----------------------------------------------------
 * Values for the 'flags' field of the UITEM structure.
 *---------------------------------------------------*/

#define U_SRC1      1
#define U_SRC2      2
#define U_DEST      4
#define U_OPMASK    8

#define U_REGOP     0x10
#define U_BASEREG   0x20
#define U_INDEXREG  0x40

/*-------------------------------------------------------------
 * Values for the 3rd argument of function 'add_cand_to_list()'
 *-----------------------------------------------------------*/

#define MUST_NOT_CONFLICT  TRUE
#define MAY_CONFLICT       FALSE

/*------------------------------
 * Macros for various iterators.
 *----------------------------*/

/* Iterator to iterate every source operands */

#define foreach_src_oprnd(opnd, ai)                 \
  {                                                 \
    OPRND *opnd;                                    \
    int count = 0;                                  \
    for (opnd = (ai)->src1; opnd != NULL;           \
         opnd = (count == 1) ? (ai)->src2 : NULL) { \
      count++;

#define next_src_oprnd \
  }                    \
  }

/* Iterator to iterate every destination operands */

#define foreach_dest_oprnd(opnd, ai) \
  {                                  \
    OPRND *opnd;                     \
    for (opnd = (ai)->dest; opnd != NULL; opnd = NULL) {

#define next_dest_oprnd \
  }                     \
  }

/* Iterator to iterate UD links for a use AILI.
 * An unique UD link is identified by an unique definition AILI
 * and an unique reading operand. Note that a UD link may not
 * be complete depending on the scale of global data flow analyse.
 * Use routine has_complete_ud to query whether an AILI has a
 * complete UD link.
 */

#define foreach_ud_in_list(defi, opr, aili)                        \
  {                                                                \
    AILI *defi;                                                    \
    OPRND *opr;                                                    \
    struct VITEM *item;                                            \
    for (item = aili->ud_chain; item != NULL; item = item->next) { \
      defi = item->ai;                                             \
      opr = item->op;

#define next_ud_in_list \
  }                     \
  }

/* Iterator to iterate DU links for a definition AILI.
 * An unique DU link is identified by an unique use AILI
 * and an unique reading operand. Note that a DU link may not
 * be complete depenidng on the scale of global data flow
 * analyse.  Use routine has_complete_du to query whether an AILI
 * has a complete DU link.
 */

#define foreach_du_in_list(usei, opr, aili)                        \
  {                                                                \
    AILI *usei;                                                    \
    OPRND *opr;                                                    \
    struct VITEM *item;                                            \
    for (item = aili->du_chain; item != NULL; item = item->next) { \
      usei = item->ai;                                             \
      opr = item->op;

#define next_du_in_list \
  }                     \
  }

/* Iterator to iterate tagged locations in CG data flow */

#define foreach_tagged_loc(loc)                          \
  {                                                      \
    int loc;                                             \
    int ndx;                                             \
    for (ndx = INVALID_TAG + 1; ndx <= max_tag; ndx++) { \
      loc = tag_map[ndx];

#define next_tagged_loc \
  }                     \
  }

/* Iterator to iterate every tagged location in the given bit vector */

#define foreach_loc_in_bv(bv, loc)                       \
  {                                                      \
    int loc;                                             \
    int ndx;                                             \
    for (ndx = INVALID_TAG + 1; ndx <= max_tag; ndx++) { \
      if (bv_mem(bv, ndx)) {                             \
        loc = tag_map[ndx];

#define next_loc_in_bv \
  }                    \
  }                    \
  }


/*====================================
 * Part 2: Typedefs for Code Generator
 *==================================*/

/*---------------------------------------------------------------
 * PRESSURE_TYPE: This is the data type of index values for the
 * 'pressure[]' arrays in the structs 'AILI', 'bih_info' and
 * 'loop_info', so the values must start at 0 and be consecutive.
 * The final value is the size of these arrays.
 *-------------------------------------------------------------*/
typedef enum {
  Gp_pressure = 0,    /* must start at 0 since used as an array index */
  Gp_cs_pressure,
  Xm_pressure,
  Xm_cs_pressure,
  Xm_legacy_pressure,
  Opmask_pressure,
  Opmask_cs_pressure,
  N_PRESSURE_TYPES    /* = 7, the size of the 'pressure[]' arrays */
} PRESSURE_TYPE;


typedef enum {
  REG_NONE = 0, /* N.B.: Must be 0 -- assumed by 'ok_register_cand()' */
  REG_GP,
  REG_XM,
  REG_OPMASK,
  REG_GP8,   /* Only used for the 2nd argument of 'cg_reg_name()' */
  REG_GP16,  /*    "      "      "      "      "      "      "    */
  REG_GP32,  /*    "      "      "      "      "      "      "    */
  REG_YM,    /*    "      "      "      "      "      "      "    */
  REG_ZM,    /*    "      "      "      "      "      "      "    */
  REG_XM128, /* Only used as a return value of 'ok_register_cand()'
              *   and 'ok_static_cand()'; means DT_128 variable. */
  REG_XM256, /*    "      "      "      ; means DT_256 variable. */
  REG_XM512, /*    "      "      "      ; means DT_512 variable. */
  REG_X87
} REGCLASS;

/* Special values for the 'regnum' argument of 'get_reg()'
 */
typedef enum {
  RTYPE_TEMP = -1,
  RTYPE_GLOBAL = -2,  /* Sets 'rc->global', denoting a potential
                       *   cand.  This may be created by 'find_pc()',
                       *   'do_renaming()' & 'do_split_live_range()'.
                       */
  RTYPE_XMM_TEMP = -3 /* Sets 'rc->xmm' */
} RTYPE;

struct bih_info {
  struct AILI *first_aili; /* a pointer to the first AILI in this block */
  int pre_first_lili;      /* the first LILI in this block;  used during PRE */
  int first_lili;          /* the first LILI in the EBB containing this block*/
  int last_lili;           /* last LILI in the EBB containing this block */
  int pressure[N_PRESSURE_TYPES];    /* the max reg pressures in this block */
};

/*-------------------------------
 * OPRND (AILI operand) structure
 *-----------------------------*/

typedef struct {
  int regnum;           /* actual register number, or 99 if unassigned */
  REGCLASS regclass;    /* REG_GP, REG_XM or REG_OPMASK */
  int candidate_num;    /* index of the corresponding register candididate
                         *   in the 'RCAND' array 'cg.candidates[]' */
} REGOP;

typedef struct {
  INT64 val;      /* 64 bit integer value (2 element array) */
  LOGICAL i32bit; /* TRUE if this constant is a 32 bit value */
} IMMEDOP;

typedef struct {/* immediate address constant */
  int sptr;     /* static or external symbol */
  int ksptr;
  LOGICAL lea; /* true if this AIMMEDOP is the 1st opnd of an lea */
} AIMMEDOP;

typedef struct {          /*  address mode  */
  struct OPRND *basereg;  /* base register */
  struct OPRND *indexreg; /* index register */
  int sptr;               /* sptr to an auto, static or extern variable */
  int ksptr;              /* sptr to a 64-bit integer constant offset */
  int nme;                /* names table entry for this memory ref */
  short scale;            /* scale is 1, 2, 4, or 8 */
} ADDROP;

/* A register pair, used for compare-exchange on 64-bit targets.
 */
typedef struct {
  struct OPRND *hi;
  struct OPRND *lo;
} REGPAIROP;

typedef struct OPRND {
  enum {
    OP_NONE,
    OP_REG,
    OP_IMMED,
    OP_AIMMED,
    OP_ADDR,
    OP_REGPAIR,
    OP_SPTR,
    OP_X87
  } opkind;
  union {
    REGOP reg;
    IMMEDOP immed;
    AIMMEDOP aimmed;
    ADDROP addr;
    REGPAIROP regpair;
    int sptr;    /* for OP_SPTR */
    int x87;     /* for OP_X87; %st == 1 */
  } u;
  int loc; /* memory location referenced by this operand */
} OPRND;

/*----------------------------
 * LILI (linear ILI) structure
 *--------------------------*/
typedef struct {
  int oprs[MAX_OPNDS];   /* ILI operands */
  OPRND *rslt;           /* result register */
  short opc;             /* ILI opcode */
  short usect;           /* the number of (link) uses of this LILI */
  short n_deferred_decr_uses;    /* the number of calls to 'decr_use()'
                                  *   that have been deferred */
  short n_fma_uses;
  short n_fnma_uses;
  unsigned short fdepth; /* floating point expression depth */
  unsigned short idepth; /* integer expression depth */
  union {
    short compareonly; /* set in cggenai.c for certain compare operations
                        *   which are the first operand of a SELECT */
    short fpuses;      /* cgoptim1.c - for f.p. loads and constant ILIs,
                        *   counts number of uses as operand of a f.p.
                        *   store, etc. */
    int x87_results;   /* For JSRs, for -Mchkfpstk: # of x87 results */
  } u;
  int ikcon; /* For DCON/FCON, points to equivalent KCON/ICON ILI */
  int gvn;   /* LILI's global value number, only used if
              *   XBIT_NEW_PRE is non-zero. */
  int prev;
  int next;
  int next2;
  short branch_num;  /* If XBIT_TREGION_CSE, 1 means LILI is in the 'if'
                      *   branch and 2 means it's in the 'else' branch;
                      *   otherwise 0. */
  short is_fma_expr; /* If XBIT_GENERATE_SCALAR_FMA, a non-zero value
                      *   indicates an expression that can be evaluated by
                      *   a scalar FMA (fused multiply add) instruction.
                      *   The value indicates the form of the expression,
                      *   as follows:
                      *   1: ab +/- c;  2: -ab +/- c; 3: c +/- ab. */
  int findex;
  int lineno;
  /*
   * Flags used by the CG:
   */
  unsigned is_deletable_st : 1;   /* a store LILI that will be deleted */
  unsigned is_rescheduled_st : 1; /* a store LILI that was moved forward*/
  unsigned is_stored : 1;         /* LILI's result is source of a store */
  unsigned is_fma_product : 1;    /* [FD]MUL or [FD]NEG( [FD]MUL ) is used
                                   *   as the product in an FMA instruction */
  unsigned is_fneg : 1;           /* [FD]SUB is a subtract from 0.0 */
  unsigned is_bprefetch : 1;      /* [IK]CJMPZ derived from a BPREFETCH */
  unsigned is_opnd_of_uikmv : 1;  /* the LILI is an operand of a UIKMV */
  unsigned contains_acon_tls : 1; /* this LILI expression contains an
                                   *   ACON_TLS{_PIC} LILI */
  unsigned no_acon_tls : 1;       /* this LILI expression doesn't contain
                                   *   an ACON_TLS{_PIC} LILI */
  /*
   * Flags used during PRE:
   */
  unsigned kills_expr : 1; /* Used during PRE, means that this LILI
                            *  kills the PRE candidate expression */
  unsigned pre_inserted : 1;
  unsigned pre_stored : 1;
  unsigned pre_load : 1;
  unsigned has_next_occ : 1;
  unsigned has_prev_occ : 1;
  unsigned in_par_block : 1;
  unsigned in_prefetch_block : 1;
  unsigned is_expr_gvn : 1;
  unsigned is_store_gvn : 1;
  unsigned is_addr_expr : 1;
  unsigned is_addr_subexpr : 1;
  unsigned is_mem_load : 1;
  unsigned is_rcand_load : 1;
  unsigned is_call : 1;
  unsigned is_avloc : 1;
  unsigned is_pantout : 1;
} LILI;

typedef enum {
  REG_NODATA = 0,
  REG_LV_START = 0x1,
  REG_LV_END = 0x2,
  REG_LV_EDGE_SPILL = 0x4,
  REG_LV_EDGE_TRANS = 0x10
} REG_DATA;

/* Each instruction has a corresponding FENCE_TYPE attribute which
 * describes restrictions about moving other instructions over the
 * instruction.  The values of FENCE_TYPE form a 4-point diamond
 * lattice, with FENCE_NONE describing the weakest constraints and
 * FENCE_ACQ_REL describing the strongest constraints.  There is no
 * "FENCE_SEQ_CST" corresponding to "sequentially consistent" since it
 * would have place the same constraints on single-thread instruction
 * scheduling as FENCE_ACQ_REL.
 */
typedef enum {
  /* No restrictions implied by this instruction.  If this instruction
   * is a load or store, other instruction's FENCE_TYPE may restrict
   * their motion with respect to this instruction.
   */
  FENCE_NONE = 0,

  /* No load may move above this instruction.  Used only on load
   * instructions.
   */
  FENCE_ACQUIRE = 0x1,

  /* No store may move below this instruction.  Used only on store
   * instructions.
   */
  FENCE_RELEASE = 0x2,

  /* No memory reference, regardless of its fence_type, or instruction
   * with fence_type != FENCE_NONE, may move above or below this
   * instruction.
   */
  FENCE_ACQ_REL = (FENCE_ACQUIRE | FENCE_RELEASE)
} FENCE_TYPE;

/*--------------------------------------------------
 * AILI (i.e. attributed ILI) structure.
 *
 * There are various pointers to AILI objects, e.g.:
 *   cg.{first_aili, last_aili}
 *   cg.candidates[cand].{first_use, last_use}
 *   use_list_item->ai
 *   live_range_item->{firstai, lastai}
 *   cg.bih_info[bih].first_aili
 *   ai->{prev, next}
 *------------------------------------------------*/
typedef struct AILI {
  OPRND *src1;            /* first source operand */
  OPRND *src2;            /* 2nd source operand */
  OPRND *dest;            /* destination operand */
  OPRND *opmask;          /* for AVX-512, the opmask operand */
  struct VITEM *ud_chain; /* UD chain */
  struct VITEM *du_chain; /* DU chain */
  short opc;              /* op code */
  short size;             /* 'b', 'w', 'l', 'q': byte, short, word or long */
  short rc_info;          /* see REG_DATA */
  short cand_no;          /* last cand to fill this slot */
  short rc_sptr;          /* sptr for a memop of a spill */
  CC_RELATION cc;         /* condition code ... one of CC_RELATION values */
  int number;             /* this AILI # */
  short pressure[N_PRESSURE_TYPES];
                          /* OPT2 reg assignment: reg pressures for this AILI */
  struct AILI *prev;      /* pointer to previous aili in list */
  struct AILI *next;      /* pointer to next aili in list */
  short branch_num;       /* if XBIT_TREGION_CSE, 1 means AILI is in the 'if'
                           *  branch and 2 means it's in the 'else' branch;
                           *  otherwise 0. */
  int findex;
  int lineno;
  unsigned memory : 1;    /* set when we have an IL_GASM w/ "memory" clobber */
#ifdef IL_FENCE
  unsigned fence_type : 2;   /* a FENCE_TYPE value */
  unsigned lock_prefix : 1;  /* use LOCK prefix in assembly code */
#endif
} AILI;

/* Wrapper as element of linked list */

typedef struct VITEM {
  int xm;
  int bih;
  AILI *ai;
  struct OPRND *op;
  struct VITEM *next;
} VITEM;

/*------------------------------------------------
 * RCAND (i.e. register candidate) structure.
 * 'cg.candidates[]' is an array of these structs.
 *----------------------------------------------*/
typedef struct RCAND {
  OPRND *regop;           /* pointer to the corresponding OP_REG operand */
  int nme;                /* names table entry for corresp. variable */
  int next;               /* used to form various lists of candidates */
  REGCLASS regclass;      /* REG_GP, REG_XM or REG_OPMASK */
  AILI *first_use;        /* beginning of the candidate's live interval */
  AILI *last_use;         /* end of this candidate's live interval */
  struct live_range_item *live_range;
  struct PITEM *pitem;    /* pointer to potential candidate, else NULL */
  struct UITEM *use_list; /* linked list of uses for this candidate */
  int def;                /* pointer to one of the defs for this cand,
                           *   if any */
  int var;                /* pointer to VARIABLE table entry for
                           *   live-var analysis */
  short n_jsrs;           /* number of calls the candidate is live over */
  unsigned global : 1;
  unsigned preassigned : 1;
  unsigned xmm : 1;       /* a REG_XM candidate storing a 16 byte value */
  unsigned ymm : 1;       /* an AVX 'ymm' register, i.e. a REG_XM
                           *   candidate that stores a 32 byte value.
                           *   If 'ymm' is set then so is 'xmm'. */
  unsigned zmm : 1;       /* an AVX3 'zmm' register, i.e. a REG_XM
                           *   candidate that stores a 64 byte value.
                           *   If 'zmm' is set then so is 'xmm' & 'ymm'.*/
  unsigned use_cs_reg : 1;
  unsigned use_legacy_reg : 1;   /* for AVX-512, must use one of the registers
                                  *   %[xyz]mm0-15, not %[xyz]mm16-31 */
  unsigned gasm_opnd : 1; /* used as an i/p or o/p opnd of an IL_GASM */
  unsigned coalesced : 1;
} RCAND;

/*------------------------------------------------------------------
 * UITEM (i.e. use list) structure.
 * 'cg.candidates[cand].use_list' points to a list of these structs.
 *----------------------------------------------------------------*/
typedef struct UITEM {/* element of use_list for reg cand */
  AILI *ai;           /* aili in which this candidate is used */
  struct UITEM *next;
  unsigned int flags; /* U_SRC1, etc */
} UITEM;

/*----------------
 * PITEM structure
 *--------------*/
typedef struct PITEM {/* potential register candidate */
  OPRND *old_addrop;
  OPRND *new_regop;
  int nme;
  int var; /* index into variable array */
  int usage_weight;
  double cand_score;
  struct PITEM *next;
} PITEM;

/*--------------------------
 * LIVE_RANGE_ITEM structure
 *------------------------*/
typedef struct live_range_item {
  AILI *firstai;
  AILI *lastai;
  int n_uses; /* only used for register candidates, not loops */
  struct live_range_item *next;
} LIVE_RANGE_ITEM;

/*-----------------------------------------------------------------
 * CGDATA structure, containing external variables local to the CG.
 *---------------------------------------------------------------*/
typedef struct {
  LILI *lili_base;   /* base pointer for linear ili */
  int lili_size;     /* size of linear ili area     */
  int first_lili;    /* the first linear ili in the current ebb */
  int last_lili;     /* next available slot in linear ili table */
  int last_terminal; /* the terminal LILI for which 'gen_aili()'
                      *   is currently generating AILIs. */
  int entry_sym;     /* symbol table pointer for current function */
  int curr_bih;      /* current ili block */
  LOGICAL use_framep;        /* stack frame pointer required */
  LOGICAL use_leave;         /* must use the leave instruction or equivalent
                              * to restore the frame pointer.
                              */
  LOGICAL pic;               /* generate position-independent code */
  LOGICAL medium_code_model; /* UNIX-ABI - small code model by default */
  LOGICAL leaf_func;         /* this function contains no JSRs */
  LOGICAL has_dt128_locals;
  LOGICAL has_big_auto_array;
  LOGICAL mscall_seen;
  LOGICAL alloc_global_regs; /* TRUE if assigning regs to user vars */
  LOGICAL linearisation_phase;
  LOGICAL flow_initialized;
  LOGICAL tls;               /* TRUE if this func contains a TLS variable */
  LOGICAL needs_vzerouppers; /* TRUE if VZEROUPPER required around calls */

  int stack_alignment;          /* 8 or 16 - alignment of the stack pointer */
  struct bih_info *bih_info;    /* array of structs that store first_aili
                                 *   and register pressure info for each bih. */
  int bih_info_size;            /* size of the array cg.bih_info[] */
  LIVE_RANGE_ITEM **loop_range; /* table of loop ranges */
  AILI *first_aili;             /* pointer to beginning of linked AILI */
  AILI *last_aili;              /* end of linked AILI / most recently added */
  ISZ_T max_arg_space;
  RCAND *candidates;    /* array of register candidate structs */
  int cand_size;        /* size of candidates array */
  int last_cand;        /* last entry in the candidates array */
  int reg_alloc_method; /* 1 for OPT1 reg assignment, else 2 */
  short return_regs[2]; /* the bin number(s) of the function's result
                         *   register(s): > 0 for GP, < 0 for XM
                         *   (= - bin number), and 0 for none */
  short callee_save_regs[MAX_N_REGS];
  int last_callee_save;
  int stackp_dummy_offset; /* save this allocations frame/stack info */
  int stackp_local_offset;
  int push_area_size;
  PITEM *potential_candidates; /* reg allocation: list of potential cands */
  int global_candidates;       /* list of global register candidates */
  OPRND *immed0;               /* aili operand representing constant 0 */
  OPRND *immed1;               /* aili operand representing constant 1 */
  char *sym_noreg;             /* basepointer of noreg flag for symbols */
  int sym_noreg_size;
  int branch_num;
  short max_num_gp_gasm_regs; /* maximum number of GP regs used as operands
                               *   of an IL_GASM.  Used by OPT1 reg alloc. */
  short max_num_xm_gasm_regs; /* ditto for XM registers */
  int n_jsrs_and_mem_gasms;   /* total number of JSR and GASM AILIs in the
                               *   whole function.  Used by 'set_n_jsrs(). */
  int x87_defs;               /* no. of x87 floating-point values defined */
  int n_xmm_regs;             /* total no. of xmm regs (8, 16 or 32) */
  int n_regs;                 /* total no. of regs (16, 32 or 48) */
} CGDATA;

extern CGDATA cg;    /* defined in cgmain.c */

/* Timing statistics */

typedef enum {
  CG_TIMING_START = 0,
  ALIAS_BUILD,
  COLLECT_DEF_USE,
  RD_LOCAL,
  RD_GLOBAL,
  RD_UD,
  CG_TIMING_FINISH
} CGTIMING;

/* Debug traces */

#undef TRACE0
#undef TRACE1
#undef TRACE2

#if DEBUG
#define TRACE0(t, s) \
  if (t)             \
  fprintf(gbl.dbgfil, s)
#define TRACE1(t, s, a1) \
  if (t)                 \
  fprintf(gbl.dbgfil, s, a1)
#define TRACE2(t, s, a1, a2) \
  if (t)                     \
  fprintf(gbl.dbgfil, s, a1, a2)
#define TRACE3(t, s, a1, a2, a3) \
  if (t)                         \
  fprintf(gbl.dbgfil, s, a1, a2, a3)

#define CLOCK_START
#define CLOCK_FINISH
#define CLOCK_DURATION

#else
#define TRACE0(t, s)
#define TRACE1(t, s, a1)
#define TRACE2(t, s, a1, a2)
#define TRACE3(t, s, a1, a2, a3)
#define CLOCK_START
#define CLOCK_FINISH
#define CLOCK_DURATION
#endif

/*===========================================
 * Part 4: External functions local to the CG
 *=========================================*/

/*--------
 * error.c
 *------*/
void asrt_failed(const char *filename, int line);

/*---------
 * cgmain.c
 *-------*/
void schedule(void);
REGCLASS ok_register_cand(int nme);
int cg_gettemp(int dtype, int sc);
void cg_sym_noreg(int);
void reset_expr_id(void);

/*-----------
 * cglinear.c
 *---------*/
int linearize_ebb(int bih);
int ld_st_size_bytes(ILI_OP ld_st_opc, int stc);
bool same_base_but_disjoint(int addr1_li, int sz1, int addr2_li, int sz2);
int add_lili(ILI_OP opc, int op1, int op2);
int insert_lili(int next_li, ILI_OP opc, int op1, int op2);
int dup_lili(int next_li, int li);
void decr_usect(int li);
MEMORY_ORDER memory_order_from_lili_operand(int li);

/*-----------
 * cgoptim1.c
 *---------*/
void cgoptim1(void);
void find_scalar_fma_lilis(void);

#if DEBUG
void dump_lilis(int stage, const char *heading);
void dump_bih_flags(int bih);
void dump_bih_flow_info(int bih);
#endif

/*----------
 * cggenai.c
 *--------*/
void genaili_init(void);
OPRND *gen_aili(int li);
OPRND *addrop(OPRND *basereg, int sptr, int ksptr, OPRND *indexreg, int scale);
OPRND *get_reg(int li, int regnum, REGCLASS r);

/*-----------
 * cgoptim2.c
 *---------*/
void cgoptim2(void);
LOGICAL is_in_loop(AILI *ai, int lp);
LOGICAL potential_static_cand(int nme);
void find_potential_cands(void);
void peephole1(LOGICAL);
void cg_init_flow(void);
void cg_end_flow(void);
void add_aili(AILI *ai, ILI_OP opc, OPRND *src1, OPRND *src2, OPRND *dest,
              int size);
void renumber_aili(AILI *ai);
LOGICAL unspillable_use(UITEM *u);
void check_addrop_uses(RCAND *rc, AILI *reload_ai);
void replace_use(AILI *, OPRND *, unsigned int flags);
void replace_uses(UITEM *use_list, OPRND *addrop);
void remove_use(RCAND *, AILI *, unsigned int use_flag);
void add_cand_to_list(int *head, int cand, LOGICAL must_not_conflict);
void set_bih_weight(int bih, int *p_weight);
void estimate_blkcnts(void);
void add_rng_to_live_range(AILI *firstai, AILI *lastai, RCAND *rc,
                                  LIVE_RANGE_ITEM **loop_range);
LOGICAL is_blend_or_fmacc_opcode(int opc);
void cg_x87(void);

#if DEBUG
void dump_aili(char *);
void dump_candidates(char *);
char *cand_string(int cand);
char *cg_nme_string(int nme);
#endif

/*-----------
 * cgopt1rg.c
 *---------*/
void opt1_reg_alloc(void);
void coalesce(void);
void gen_save_restores(int cand, AILI *ai);

/*-----------
 * cgopt2rg.c
 *---------*/
void opt2_reg_alloc(void);
LOGICAL opt2_chk_coalesce(RCAND *rc);

/*----------
 * cgsched.c
 *--------*/
void schedule_aili(void);
bool opc_uses_cc(ILI_OP opc, LOGICAL aili_cc_must_have_CC_value);

/*----------
 * cgassem.c
 *--------*/
void cgassem(void);
char *cg_reg_name(int regnum, REGCLASS regclass);
