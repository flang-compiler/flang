/*
 * Copyright (c) 1993-2018, NVIDIA CORPORATION.  All rights reserved.
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
 * \brief various definitions for the expand module
 */

#include <stdint.h>

/*  DEBUG-controlled -q stuff  */

#define EXPDBG(x, y) (DEBUG && DBGBIT(x, y))

/* storage allocation macros  */

#define EXP_ALLOC(stgb, dt, sz) \
  {                             \
    NEW(stgb.stg_base, dt, sz); \
    stgb.stg_size = sz;         \
  }

#define EXP_MORE(stb, dt, nsz)                                              \
  {                                                                         \
    stb.stg_base =                                                          \
        (dt *)sccrelal((char *)stb.stg_base, ((BIGUINT64)((nsz) * sizeof(dt)))); \
    stb.stg_size = nsz;                                                     \
  }

#define EXP_NEED(stb, dt, nsz)      \
  if (stb.stg_avail > stb.stg_size) \
    EXP_MORE(stb, dt, nsz);

#define EXP_FREE(stb) FREE(stb.stg_base)

int is_passbyval_dummy(int);

#ifndef FE90

/*****  expander's view of the ILMs  *****/

typedef struct {
  ILM_T opc;
  ILM_T opnd[1];
} ILM;

#define ILM_OPC(i) ((i)->opc)
#define ILM_OPND(i, opn) ((i)->opnd[opn - 1])

/*
 * ILM Auxillary Area Declarations - Used to accumulate information about
 * ILMs while being expanded. There is an item for each ILM.  Each item in
 * this area is indexed by the ILM index; since the ILM index is just an
 * offset from the beginning of the ILM area, there will be items in the aux
 * area that are not used.
 */
typedef struct {
  int w1;
  int w2;
  int w3;
  int w4;
  int w5;
  int w6;
  int w7;
  int w8;
} ILM_AUX;

#define ILM_TEMP(i) (expb.temps[i])

#define ILI_OF(i) (expb.ilmb.stg_base[i].w1)
#define NME_OF(i) (expb.ilmb.stg_base[i].w2)
#define SCALE_OF(i) (expb.ilmb.stg_base[i].w4)

#define ILM_RESULT(i) (expb.ilmb.stg_base[i].w1)
#define ILM_NME(i) (expb.ilmb.stg_base[i].w2)
#define ILM_BLOCK(i) (expb.ilmb.stg_base[i].w3)
#define ILM_SCALE(i) (expb.ilmb.stg_base[i].w4)

#define ILM_RRESULT(i) ILM_RESULT(i)
#define ILM_IRESULT(i) (expb.ilmb.stg_base[i].w7)

/* RESTYPE is used to indicate result type */
#define ILM_RESTYPE(i) (expb.ilmb.stg_base[i].w6)
#define ILM_ISCMPLX 1
#define ILM_ISDCMPLX 2
#define ILM_ISCHAR 3
#define ILM_ISI8 4
#define ILM_ISX87CMPLX 5
#define ILM_ISDOUBLEDOUBLECMPLX 6
#define ILM_ISFLOAT128CMPLX 7

/* character stuff */
#define ILM_MXLEN(i) (expb.ilmb.stg_base[i].w5)
#define ILM_CLEN(i) (expb.ilmb.stg_base[i].w7)

/* this is used to tell whether an operand was
 * directly expanded for this parent ILM, or some other */
#define ILM_EXPANDED_FOR(i) (expb.ilmb.stg_base[i].w8)

#define DOREG1 (flg.opt == 1 && !XBIT(8, 0x8))
#define ADDRCAND(a, b) \
  if (DOREG1)          \
  exp_rcand(a, b)

/* FTN string stuff */

#define STR_AREA 6

typedef struct _str {/* string descriptor */
  char aisvar;       /* string address is variable if TRUE */
  char liscon;       /* string length is constant */
  char dtype;        /* TY_CHAR or TY_NCHAR */
  int aval;          /* address symptr or ili */
  int lval;          /* string length or ili */
  int cnt;           /* # items this list */
  int tempnum;       /* temp # for this var */
  struct _str *next; /* next strdesc */
} STRDESC;

/* data common to expander module  */

typedef struct {
  int temps[9]; /* ili index temp area during expand */
  struct {
    ILM_AUX *stg_base;
    int stg_size;
  } ilmb;
  union {
    uint16_t wd;
    struct {
      uint16_t waitlbl : 1;    /* waiting for a LABEL ILM	 */
      uint16_t noblock : 1;    /* no block has been created	 */
      uint16_t excstat : 1;    /* excstat was changed		 */
      uint16_t dbgline : 1;    /* blocks are to be debugged	 */
      uint16_t callfg : 1;     /* function calls an external	 */
      uint16_t sdscunsafe : 1; /* call might mod descriptor */
      uint16_t noheader : 1;   /* no entry header written (ftn) */
    } bits;
  } flags;
  int nilms;   /* number of (short) words in the ILM block */
  int curlin;  /* line number of the current ILI block	 */
  int curbih;  /* index of BIH of the current ILT block	 */
  int curilt;  /* index of the current (last) ILT		 */
  int saveili; /* ILI (a JMP) not yet added to the block	 */
  int retlbl;  /* ST index to the current return label	 */
  int retcnt;  /* decimal number for the current rtn label */
  int swtcnt;  /* decimal number for the last switch array */
  int arglist; /* ST index of the current argument list	 */
  struct {
    short next;  /* decimal # for the next arglist	 */
    short start; /* start # for arglists in a function	 */
    short max;   /* max "next" # for arglists in a func.	 */
  } arglcnt;
  int uicmp;         /* symbol table index of uicmp function	 */
  int gentmps;       /* general temps */
  LOGICAL qjsr_flag; /* qjsr present in the function/subprogram */
  LOGICAL intr_flag; /* intrinsic present in the function/subprogram*/
  LOGICAL isguarded; /* increment when encounter DOBEGNZ */
  INT ilm_words;     /* # of ilm words in the current ili block */
  INT ilm_thresh;    /* if ilm_words > ilm_thresh, break block */
  SC_KIND sc;        /* storage class used for expander-created
                      * temporaries (SC_LOCAL, SC_PRIVATE).
                      */
  int lcpu2;         /* temporary for the current function's
                      * value of mp_lcpu2().
                      */
  int lcpu3;         /* temporary for the current function's
                      * value of mp_lcpu3().
                      */
  int ncpus2;        /* temporary for the  current function's
                      * value of mp_ncpus2().
                      */
  int chartmps;      /* character temps */
  int chardtmps;     /* char descriptor temps */
  STRDESC *str_base; /* string descriptor list */
  int str_size;
  int str_avail;
  int logcjmp;  /* compare & branch ili for logical values:
                 * default is IL_LCJMPZ (odd/even test); -x 125 8
                 * implies IL_ICJMPZ (zero/non-zero test).
                 * initialized by exp_init().
                 */
  int aret_tmp; /* temporary for the alternate return value */
  int clobber_ir; /* gcc-asm clobber list (iregs) info */
  int clobber_pr; /* gcc-asm clobber list (pregs) info */
  int mxcsr_tmp;  /* temporary for the value of the mxcsr */
  int implicitdataregions;
  DTYPE charlen_dtype;
} EXP;

extern EXP expb;

#define CHARLEN_64BIT (XBIT(68,1) || XBIT(68,0x20))

/* llassem.c */
int mk_charlen_address(int sptr);
int make_uplevel_arg_struct(void);
void load_uplevel_addresses(int display_temp);
void arg_is_refd(int);

/* outliner.c */
extern int ll_get_shared_arg(int);
extern int ll_get_hostprog_arg(int , int );

/* semutil0.c */
int getrval(int ilmptr);

int optional_missing(int nme);
int optional_present(int nme);
int rdilms();
int compl_br(int, int);
int exp_addbih(int);
int reduce_ilt(int, int);

/* exputil.c */
/* This header file is included by some translation units
   that do not include the header that defined SWEL.
   So the presence of SWELG is used to indirectly detect
   whether SWEL is present. */
#ifdef SWELG
int mk_swlist(INT n, SWEL *swhdr, int doinit);
#endif
int mk_argasym(int);
int mk_impsym(int);
int get_byval_local(int);
SPTR mkfunc_sflags(const char *nmptr, const char *flags);

void cr_block(void);
void wr_block(void);
void flsh_block(void);
void loc_of(int);
void mkarglist(int, int);
int check_ilm(int, int);
void chk_terminal_func(int, int);
int find_argasym(int sptr);
int add_reg_arg_ili(int, int, int, DTYPE);

#if DEBUG
void dmpnme(void);
#endif
void exp_rcand(int ilix, int nmex);
void reg_assign1(void);

/* Use DT_NONE to detect whether DTYPE is declared. */
#ifdef DT_NONE
int create_array_ref(int nmex, int sptr, DTYPE dtype, int nsubs, int *subs,
                     int ilix, int sdscilix, int inline_flag, int *pnme);
#endif /* DT_NONE */

#ifdef EXPANDER_DECLARE_INTERNAL
/* Routines internal to the expander that should not be declared
   as part of the public interface. */

void chk_block(int);
void chk_block_suppress_throw(int);
void eval_ilm(int);
void exp_ac(ILM_OP, ILM *, int);
void exp_bran(ILM_OP, ILM *, int);
void exp_call(ILM_OP, ILM *, int);
void exp_qjsr(char *, int, ILM *, int);
void exp_estmt(int);
void exp_label(int);
void expand_smove(int, int, DTYPE);
void exp_remove_gsmove(void);
void exp_load(ILM_OP, ILM *, int);
void exp_store(ILM_OP, ILM *, int);
void exp_misc(ILM_OP, ILM *, int);
void exp_ref(ILM_OP, ILM *, int);
int exp_mac(ILM_OP, ILM *, int);
void exp_array(ILM_OP, ILM *, int);
void exp_fstring(ILM_OP opc, ILM *ilmp, int curilm);
int exp_alloca(ILM *);
void exp_alloc_prologue(void);
void exp_alloc_epilogue(void);
void exp_restore_mxcsr(void);
void exp_pure(int, int, ILM *, int);
int exp_get_sdsc_len(int, int, int);
SPTR frte_func(SPTR (*)(const char *), const char *);
void put_funccount(void);
void replace_by_zero(ILM_OP opc, ILM *ilmp, int curilm);
void replace_by_one(ILM_OP opc, ILM *ilmp, int curilm);
int expand_narrowing(int ilix, DTYPE dtype);
#define expand_throw_point(ilix, dtype, ili_st) \
  (DEBUG_ASSERT(0, "throw points supported only for C++"), (ilix))

#endif /* EXPANDER_DECLARE_INTERNAL */

/* expand.c */
int expand(void);
void ref_threadprivate(int, int *, int *);
int llGetThreadprivateAddr(int);
void ref_threadprivate_var(int, int *, int *, int);
int getThreadPrivateTp(int);
void ds_init(void);
bool bindC_function_return_struct_in_registers(int func_sym);
int charlen(int sym);

/* expsmp.c */
#ifdef EXPANDER_DECLARE_INTERNAL
void exp_smp_init(void);
void exp_smp_fini(void);
void exp_smp(ILM_OP, ILM *, int);
#endif /* EXPANDER_DECLARE_INTERNAL */
int exp_lcpu2(int);
int exp_lcpu3(int);
int exp_ncpus2(int);
int add_mp_p(int);
int add_mp_v(int);
int add_mp_penter(int);
int add_mp_pexit(void);
int add_mp_ncpus(void);
int add_mp_lcpu(void);
void no_pad_func(char *);
void exp_mp_func_prologue(void);

/* expatomics.c */
#ifdef EXPANDER_DECLARE_INTERNAL
int get_capture_read_ili(void);
int get_capture_update_ili(void);
void set_capture_read_ili(int);
void set_capture_update_ili(int);

/* cgmain.c */
bool ll_check_struct_return(DTYPE);

ILI_OP get_atomic_update_opcode(int ili);
void set_is_in_atomic(int);
int get_is_in_atomic(void);
void set_is_in_atomic_read(int);
int get_is_in_atomic_read(void);
void set_is_in_atomic_write(int);
int get_is_in_atomic_write(void);
void set_is_in_atomic_capture(int);
int get_is_in_atomic_capture(void);

LOGICAL exp_end_atomic(int, int);
#ifdef PD_IS_ATOMIC
bool exp_atomic_intrinsic(PD_KIND pd, ILM *ilmp, int curilm);
#endif
int exp_mp_atomic_read(ILM *);
void exp_mp_atomic_write(ILM *);
void exp_mp_atomic_update(ILM *);
void exp_mp_atomic_capture(ILM *);
void ldst_msz(DTYPE, ILI_OP *, ILI_OP *, MSZ *);
#endif /* EXPANDER_DECLARE_INTERNAL */

int gethost_dumlen(int arg, ISZ_T address);
int getdumlen(void);
int llProcessNextTmpfile(void);
void ll_set_new_threadprivate(int);

void AssignAddresses(void);

void chk_block(int new_ili);
void exp_add_copy(int lhssptr, int rhssptr);

void set_assn(int);
#endif /* ifndef FE90 */
