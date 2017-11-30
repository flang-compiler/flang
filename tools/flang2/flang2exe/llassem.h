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

#ifndef LLASSEM_H_
#define LLASSEM_H_

#include "llutil.h"

typedef struct argdtlist DTLIST;

struct argdtlist {
  LL_Type *lltype;
  int byval;

  /* XXX: sptr needs to go away, since fortran sptrs are only relevant for
   * the function being compiled.  This is for homing
   * (process_formal_arguments) support.  Which should only be called when
   * this sptr data is valid.
   */
  int sptr;
  DTLIST *tail;
  DTLIST *next;
};

typedef struct uplevelpair {
  int oldsptr; /* sptr from ilm file */
  int newsptr; /* newsptr - from symbolxref[oldsptr] */
  int newmem;  /* sptr member of struct for newsptr */
} UPLEVEL_PAIR;

#define STACK_CAN_BE_32_BYTE_ALIGNED (aux.curr_entry->flags & 0x200)
#define ENFORCE_32_BYTE_STACK_ALIGNMENT (aux.curr_entry->flags |= 0x400)

#define IS_STABS (XBIT(120, 0x20))
#define ASMFIL gbl.asmfil

/** general length suitable for creating names from a symbol name during
    assembly, e.g., 1 for null, 3 for extra '_' , * 4 for @### with mscall */
#define MXIDLN (3 * MAXIDLEN + 10)

/**
 * structure to represent items being dinit'd -- used to generate
 * a sorted list of dinit items for a given common block or local
 * variable.
 */
typedef struct DSRT_TAG {
  int sptr;     /* sym being init'd (could be structure) */
  ISZ_T offset; /* byte offset of item init'd */

  int sectionindex; /* Fortran - section index */
  long filepos;     /* Fortran dinit file position for item's dinit(s) */
  int func_count;   /* Fortran save/restore func_count */

  int dtype;    /* used for C */
  int len;      /* used for C - character */
  ISZ_T conval; /* used for C */

  struct DSRT_TAG *next; /* next in list (sorted in ascending offset) */
  struct DSRT_TAG *ladd; /* item last added - not used in C */
} DSRT;

extern int master_sptr;
char *get_string_constant(int);
char *get_local_overlap_var(void);
int get_ag_argdtlist_length(int gblsym);
LOGICAL get_byval_from_argdtlist(const char *argdtlist);
int get_sptr_from_argdtlist(char *argdtlist);
int get_sptr_uplevel_address(int sptr);
LOGICAL is_llvmag_entry(int gblsym);
void set_llvmag_entry(int gblsym);
void set_ag_argdtlist_is_valid(int gblsym);
void llvm_funcptr_store(int sptr, char *ag_name);
int add_ag_typename(int gblsym, char *typename);
int ll_shallow_copy_uplevel(int hostsptr, int olsptr);
int runtime_alignment(int syma);
void assem_put_linux_trace(int);

char *put_next_member(char *ptr);
ISZ_T put_skip(ISZ_T old, ISZ_T new);
void emit_init(int tdtype, ISZ_T tconval, ISZ_T *addr, ISZ_T *repeat_cnt,
               ISZ_T loc_base, ISZ_T *i8cnt, int *ptrcnt, char **cptr);

void create_static_name(char *name, int usestatic, int num);
void create_static_base(int num);
void hostsym_is_refd(int sptr);

/*
 * macros to get and put DSRT pointers in symbol table entry - this
 * uses the XREF field
 */
#define DSRTG(s) ((DSRT *)get_getitem_p(XREFLKG(s)))
#define DSRTP(s, v) XREFLKP(s, put_getitem_p(v))

#define GET_DSRT (DSRT *) getitem(2, sizeof(DSRT))

/* structures and routines to process assembler globals for the entire file */

#define AG_HASHSZ 19
#define AG_SIZE(s) agb.s_base[s].size
#define AG_ALIGN(s) agb.s_base[s].align
#define AG_DSIZE(s) agb.s_base[s].dsize
#define AG_SYMLK(s) agb.s_base[s].symlk
#define AG_HASHLK(s) agb.s_base[s].hashlk
#define AG_NMPTR(s) agb.s_base[s].nmptr
#define AG_TYPENMPTR(s) agb.s_base[s].type_nmptr
#define AG_OLDNMPTR(s) agb.s_base[s].old_nmptr
#define AG_TYPEDESC(s) agb.s_base[s].typedesc /* Boolean */
#define AG_STYPE(s) agb.s_base[s].stype
#define AG_RET_LLTYPE(s) agb.s_base[s].ret_lltype
#define AG_LLTYPE(s) agb.s_base[s].lltype
#define AG_DTYPE(s) agb.s_base[s].dtype
#define AG_DTYPESC(s) agb.s_base[s].dtypesc
#define AG_SC(s) agb.s_base[s].sc /* Storage class */
#define AG_ALLOC(s) agb.s_base[s].alloc
#define AG_REF(s) agb.s_base[s].ref
#define AG_DEFD(s) agb.s_base[s].defd
#define AG_DEVICE(s) agb.s_base[s].device
#define AG_ISMOD(s) agb.s_base[s].ismod
#define AG_ISTLS(s) agb.s_base[s].istls
#define AG_NEEDMOD(s) agb.s_base[s].needmod
#define AG_ISCTOR(s) agb.s_base[s].ctor
#define AG_ISIFACE(s) agb.s_base[s].iface
#define AG_FINAL(s) agb.s_base[s].final
#define AG_UPLEVEL_AVL(s) agb.s_base[s].uplevel_avl
#define AG_UPLEVEL_SZ(s) agb.s_base[s].uplevel_sz
#define AG_UPLEVELPTR(s) agb.s_base[s].uplist
#define AG_UPLEVEL_OLD(s, i) agb.s_base[s].uplist[i].oldsptr
#define AG_UPLEVEL_NEW(s, i) agb.s_base[s].uplist[i].newsptr
#define AG_UPLEVEL_MEM(s, i) agb.s_base[s].uplist[i].newmem
#define AG_DLL(s) agb.s_base[s].dll
#define AG_NAME(s) agb.n_base + agb.s_base[s].nmptr
#define AG_TYPENAME(s) agb.n_base + agb.s_base[s].type_nmptr
#define AG_OLDNAME(s) agb.n_base + agb.s_base[s].old_nmptr
#define AG_ARGDTLIST(s) agb.s_base[s].argdtlist
#define AG_ARGDTLIST_LENGTH(s) agb.s_base[s].n_argdtlist
#define AG_ARGDTLIST_IS_VALID(s) agb.s_base[s].argdtlist_is_set

#define FPTR_HASHLK(s) fptr_local.s_base[s].hashlk
#define FPTR_IFACENMPTR(s) fptr_local.s_base[s].ifacenmptr
#define FPTR_IFACENM(s) fptr_local.n_base + fptr_local.s_base[s].ifacenmptr
#define FPTR_NMPTR(s) fptr_local.s_base[s].nmptr
#define FPTR_NAME(s) fptr_local.n_base + fptr_local.s_base[s].nmptr
#define FPTR_SYMLK(s) fptr_local.s_base[s].symlk

#define DEFINE_STRUCT

LL_Value *gen_ptr_offset_val(int, LL_Type *, char *);

/**
   \brief llassem global symbol table entries
 */
typedef struct {
  ISZ_T size;      /**< max size of common block in file
                      if entry/proc, 1 => defd, 0 => proc */
  ISZ_T dsize;     /**< size of common block when init'd */
  INT nmptr;
  INT type_nmptr;  /**< Used for external function */
  INT farg_nmptr;  /**< make all function that is not defined in same file
                      vararg with first argument specified if any */
  INT old_nmptr;   /**< Used for interface to keep original function name */
  INT align;       /**< alignment for BIND(C) variables */
  int symlk;       /**< used to link ST_CMBLK and ST_PROC */
  int hashlk;      /**< hash collision field */
  int dtype;       /**< used for keep track dtype which is created for static/
                      bss area (only for AGL ag-local) */
  int dtypesc;     /**< dtype scope */
  int n_argdtlist; /**< Number of items in argdtlist */
  LOGICAL argdtlist_is_set; /**< Argdtlist has built, perhaps with 0 args */
  char stype;               /**< ST_ of global */
  char sc;                  /**< SC_ of global */
  char alloc;               /**< ALLOCATABLE flag */
  char dll;                 /**< DLL_NONE, DLL_EXPORT, DLL_IMPORT */
  LL_Type *lltype;          /**< LLVM representation of the ag symbol */
  LL_Type *ret_lltype;      /**< If this is a func this is the return type */
  DTLIST *argdtlist;        /**< linked listed of argument lltype */
  int uplevel_avl;
  int uplevel_sz;
  UPLEVEL_PAIR *uplist;  /**< uplevel list for internal procecure */
  unsigned ref : 1;      /**< ST_PROC is referenced */
  unsigned defd : 1;     /**< module ST_CMBLK is defined in file */
  unsigned device : 1;   /**< CUDA device routine */
  unsigned ismod : 1;
  unsigned needmod : 1;
  unsigned ctor : 1;     /**< set if this routine has attribute constructor */
  unsigned typedesc : 1; /**< set if this variable is a type descriptor */
  unsigned iface : 1;    /**< set if this is part of interface */
  unsigned final : 1;    /**< set if this is final table */
  unsigned istls : 1;    /**< set if this is TLS */
} AG;

/**
   \brief storage allocation structure for assem's symtab
 */
typedef struct AGB_t {
  AG *s_base;   /**< pointer to table of common block nodes */
  int s_size;   /**< size of CM table */
  int s_avl;    /**< currently available entry */
  char *n_base; /**< pointer to names space */
  int n_size;
  int n_avl;
  int hashtb[AG_HASHSZ];
} AGB_t;

DEFINE_STRUCT AGB_t agb;

void set_ag_return_lltype(int gblsym, LL_Type *llt);
LL_Type *get_ag_return_lltype(int gblsym);
void set_ag_lltype(int gblsym, LL_Type *llt);

/** similar to AG struct but smaller */
typedef struct {
  INT nmptr;
  INT ifacenmptr;
  int hashlk;
  int symlk;
} FPTRSYM;

/** storage for function pointer */
DEFINE_STRUCT struct {
  FPTRSYM *s_base;
  int s_size;
  int s_avl;
  char *n_base; /* pointer to names space */
  int n_size;
  int n_avl;
  int hashtb[AG_HASHSZ];
} fptr_local;

DEFINE_STRUCT DSRT *lcl_inits;     /* head list of DSRT's for local variables */
DEFINE_STRUCT DSRT *section_inits; /* head list of DSRT's for initialized
                                      variables in named sections */
DEFINE_STRUCT DSRT *extern_inits;  /* head list of DSRT's for BIND(C) module
                                      variables */
DEFINE_STRUCT char static_name[MXIDLN]; /* name of STATIC area for a function */
DEFINE_STRUCT int first_data;

struct sec_t {
  const char *name;
  int align;
};

/* ag entries */
DEFINE_STRUCT int ag_cmblks; /* list of common blocks in file */
DEFINE_STRUCT int ag_procs;  /* list of procs in file */
DEFINE_STRUCT int ag_other;  /* list of other externals in file */
DEFINE_STRUCT int ag_global; /* list of symbols that need to be declared
                                global */
DEFINE_STRUCT int ag_typedef; /* list of derived type that need to be
                                 declared  */
DEFINE_STRUCT int ag_static; /* keep name and type of static */
DEFINE_STRUCT int ag_intrin; /* intrinsic list generated by the bridge and
                                has no sptr */
DEFINE_STRUCT int ag_local; /* dummy arguments which is a subroutine -
                               need its signature and type */
DEFINE_STRUCT int ag_funcptr; /* list of function pointer - should be a member
                                 of user defined type. Currently keep both
                                 LOCAL(any?) and STATIC in same list */

int find_ag(const char *ag_name);
int get_typedef_ag(char *ag_name, char *typename);
int get_ftn_typedesc_dtype(int sptr);

void put_i32(int);
void put_addr(int, ISZ_T, int);
void put_string_n(char *, ISZ_T, int);
void put_short(int);
void put_int4(INT);

/* AG table accessors and mutators */
int get_llvm_funcptr_ag(int sptr,
                        char *ag_name); /* TODO: rename for consistency */

int get_private_size(void);
int local_funcptr_sptr_to_gblsym(int sptr);

union {
  unsigned short i8;
  unsigned char byte[2];
} i8bit;

union {
  unsigned short i16;
  unsigned char byte[2];
} i16bit;

union {
  unsigned int i32;
  float r4;
  unsigned char byte[4];
} i32bit;

union {
  unsigned long i64; /* need to make sure this is 64 bit */
  double r8;
  unsigned char byte[8];
} i64bit;

#if defined(TARGET_LLVM_X8664) || defined(TARGET_LLVM_POWER) || defined(TARGET_LLVM_ARM64)
#define DIR_LONG_SIZE 64
#else
#define DIR_LONG_SIZE 32
#endif

#define MAXARGLEN 256

void put_fstr(int sptr, int add_null);
int runtime_32_byte_alignment(int acon_sptr);
int is_cache_aligned(int syma);
int get_dummy_ag(int sptr);
char *getextfuncname(int sptr);
char *get_ag_name(int);
void ll_override_type_string(LL_Type *llt, const char *str);

int alignment(DTYPE);
char *gen_llvm_vconstant(const char *, int, int, int);
int get_int_dtype_from_size(int);
int add_member_for_llvm(int, int, DTYPE, ISZ_T);
int mk_struct_for_llvm_init(const char *name, int size);
LL_Type *update_llvm_typedef(DTYPE dtype, int sptr, int rank);
int llvm_get_unique_sym(void);

void assemble(void);
void assem_data(void);
void assemble_end(void);
void assem_init(void);
void assem_dinit(void);
void assemble_init(int, char *[], char *);
void put_section(int sect);
void assem_emit_align(int n);
void align_func(void);
void put_global(char *name);
void put_func_name(int sptr);
char *getsname(int sptr);
void put_type(int sptr);
void init_huge_tlb(void);
void init_flushz(void);
void init_daz(void);
void init_ktrap(void);
char *getdname(int sptr);
ISZ_T get_socptr_offset(int);
#if defined(PG0CL)
#define llassem_end_func(ignore1,arg2) assem_end_func(arg2)
#else
#define llassem_end_func(arg1, arg2) lldbg_function_end(arg1, arg2)
#endif
void arg_is_refd(int);
unsigned align_of_var(SPTR);

void assem_end(void);
int get_ag(int sptr);
int get_hollerith_size(int sptr);
void _fixup_llvm_uplevel_symbol(void);
void _add_llvm_uplevel_symbol(int sptr);
void add_aguplevel_oldsptr(void);
char *get_main_progname(void);
LL_Type *get_lltype_from_argdtlist(char *argdtlist);
void addag_llvm_argdtlist(int gblsym, int arg_num, int arg_sptr, LL_Type *);
int get_master_sptr(void);
int generic_dummy_dtype(void);
LL_Type *make_generic_dummy_lltype(void);
void set_llvm_iface_oldname(int, char *);
LL_ABI_Info *process_ll_abi_func_ftn(int, LOGICAL);
LL_Type *get_local_overlap_vartype(void);
LL_ObjToDbgList **llassem_get_objtodbg_list(SPTR sptr);
extern void ll_process_routine_parameters(int sptr);

#endif
