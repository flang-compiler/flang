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

/* global.h - FTN global variables and flags. */

/* An index into the symbol table. */
typedef enum SPTR {
  NME_NULL = -1,
  SPTR_NULL = 0,
  SPTR_MAX = 67108864 /* Maximum allowed value */
} SPTR;

#ifdef __cplusplus
// Enable symbol table traversals to work.
static inline void operator++(SPTR &s)
{
  s = SPTR(s + 1);
}
#endif

#ifdef UTILSYMTAB
typedef int RUTYPE;
#endif

typedef struct {
  int maxsev;      /* maximum error severity for this compile */
  int lineno;      /* current source line number */
  int findex;      /* current file index */
  char *src_file;  /* name of main input source file */
  char *curr_file; /* name of current input source file */
  char *module;    /* object module name */
  FILE *srcfil;    /* file pointer for source input file */
  FILE *cppfil;    /* file pointer for preprocessor output */
  FILE *dbgfil;    /* file pointer for debug file */
  FILE *ilmfil;    /* file pointer for (temporary) ILM file */
  FILE *objfil;    /* file pointer for output object file */
  FILE *asmfil;    /* file pointer for output assembly file */
  FILE *stbfil;    /* file pointer for symbols and datatype for llvm compiler */
  LOGICAL eof_flag;
  SPTR currsub;  /* symtab ptr to current subprogram */
  SPTR caller;   /* symtab ptr to current caller (for bottom-up inlining) */
  int cgr_index; /* call graph index to current subprogram */
  LOGICAL arets; /* set to true if any entry contains an
                    alternate return.  */
  RUTYPE rutype;    /* RU_PROG, RU_SUBR, RU_FUNC, or RU_BDATA */
  int funcline;  /* line number of header statement */
  int cmblks;    /* pointer to list of common blocks */
  int externs;   /* pointer to list of external functions */
  int consts;    /* pointer to list of referenced constants */
  int entries;   /* list of entry symbols */
  int statics;   /* list of "static" variables */
  int bssvars;   /* list of uninitialized "static" variables */
  int locals;    /* pointer to list of local variables   */
  int basevars;  /* pointer to list of base symbols used for global offsets */
  int asgnlbls;  /* pointer to list of labels appearing in assign stmts.*/
  int vfrets;    /* nonzero if variable format (<>) items present */
  ISZ_T caddr;   /* current available address in code space */
  ISZ_T locaddr; /* current available address for local variables,
                  * (positive offset from $local)  */
  ISZ_T saddr;   /* current available address for static variables,
                  * (positive offsets from $static.  */
  ISZ_T
  bss_addr;    /* current available address for static uninitialized variables,
                * (positive offsets from .BSS)  */
  ISZ_T paddr; /* current available address for private variables */
  int prvt_sym_sz;  /* symbol representing size of private area */
  int stk_sym_sz;   /* symbol representing size of stack area */
  int autobj;       /* list of automatic data objects; the st field
                     * AUTOBJ is used to link together the objects; NOSYM
                     * terminates the list.
                     */
  INT silibcnt;     /* number of scheduled ILI blocks */
  char *loc_arasgn; /* pointer to list of ARASGN's for local cmnblk */
  char datetime[21];
  int entbih;           /* entry bih of a function, set by expander/optimizer
                         * to communicate with other modules.
                         */
  int func_count;       /* function counter, current # of function being
                         * compiled, incremented by assem_init */
  char *file_name;      /* full pathname of input file; -file may override */
  int ftn_true;         /* value of .TRUE.; -1 (default) or 1 (-x 125 8) */
  LOGICAL has_program;  /* true if a fortran 'program' has been seen */
  LOGICAL in_include;   /* set to true if source is from an include file */
  LOGICAL nowarn;       /* if TRUE, don't issue warning & informational errors*/
  int internal;         /* internal subprogram state:
                         * 0 - current subprogram does not contain internal
                         *     subprograms.
                         * 1 - current subprogram contains internal subprograms
                         *     (current subprogram is the 'host' subprogram).
                         * >1 - current subprogram is an internal subprogram.
                         */
  SPTR outersub;        /* symtab ptr to containing subprogram */
  int threadprivate;    /* pointer to list of symbols created for each thread-
                         * private common block.  Each symbol will represent
                         * a vector of pointers used to locate a thread's
                         * copy of the common block.
                         */
  LOGICAL nofperror;    /* if TRUE, error.c:fperror() does not report errors */
  int fperror_status;   /* error status of a floating point operation
                         * performed by scutil.
                         */
  FILE *ipafil;         /* propagated ipa information */
  FILE *ipofil;         /* newly generated ipa information */
  FILE *dependfil;      /* make dependency information */
  int multiversion;     /* if we're compiling multiple versions of a subprogram
                         */
  int numversions;      /* if we're compiling multiple versions of a subprogram
                         */
  int numcontained;     /* after compiling a host subprogram, how many
                         * contained subprograms are there left to compile */
  int multi_func_count; /* used when compiling multiple versions */
  int pgfi_avail;
  int ec_avail; /* Profile edge count info is available */
  char *fn;     /* name of file being compiled passed from the FE */
  int cuda_constructor;
  int cudaemu; /* emulating CUDA device code */
#ifdef PGF90
  int typedescs; /* list of type descriptors */
#endif
  LOGICAL denorm; /* enforce denorm for the current subprogram */
  int outlined;   /* is outlined function .*/
  int usekmpc;    /* use KMPC runtime. turned on for -ta=multicore for llvm. */
} GBL;

#undef MAXCPUS
#define MAXCPUS 256

extern GBL gbl;
#define GBL_CURRFUNC gbl.currsub
#define TPNVERSION 25

typedef struct {
  LOGICAL asmcode;
  LOGICAL list;
  LOGICAL object;
  LOGICAL xref;
  LOGICAL code;
  LOGICAL include;
  int debug;
  int opt;
  LOGICAL depchk;
  LOGICAL depwarn;
  LOGICAL dclchk;
  LOGICAL locchk;
  LOGICAL onetrip;
  LOGICAL save;
  int inform;
  UINT xoff;
  UINT xon;
  LOGICAL ucase;
  char **idir;
  char **linker_directives;
  char *llvm_target_triple;
  LOGICAL dlines;
  int extend_source;
  LOGICAL i4;
  LOGICAL line;
  LOGICAL symbol;
  int profile;
  LOGICAL standard;
  int dbg[96];
  LOGICAL dalign; /* TRUE if doubles are double word aligned */
  int astype;     /* target dependent value to support multiple asm's */
  LOGICAL recursive;
  int ieee;
  int inliner;
  int autoinline;
  int vect;
  LOGICAL endian;
  LOGICAL terse;
  int dollar;   /* defines the char to which '$' is translated */
  int x[251];   /* x flags */
  LOGICAL quad; /* quad align "unconstrained objects" if sizeof >= 16 */
  int anno;
  LOGICAL qa; /* TRUE => -qa appeared on command line */
  LOGICAL es;
  LOGICAL p;
  char **def;
  char *stdinc; /* NULL => use std include; 1 ==> do not look in
                 * std dir; o.w., use value as the std dir */
  LOGICAL smp;  /* TRUE => allow smp directives */
  int errorlimit;
  LOGICAL trans_inv; /* global equiv to -Mx,7,0x10000 */
  int tpcount;
  int tpvalue[TPNVERSION]; /* target processor(s), for unified binary */
} FLG;

extern FLG flg;

#define IEEE_CMP (flg.ieee || !XBIT(15, 0x8000000))
