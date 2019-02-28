/*
 * Copyright (c) 1994-2019, NVIDIA CORPORATION.  All rights reserved.
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

#ifndef GLOBAL_H_
#define GLOBAL_H_

/** \file global.h
    \brief Fortran global variables and flags.
*/

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

typedef enum {
  RU_SUBR = 1,
  RU_FUNC,
  RU_PROC,
  RU_PROG,
  RU_BDATA,
} RU_TYPE;

typedef struct {
  int maxsev;      /* maximum error severity for this compile */
  int lineno;      /* current source line number */
  int findex;      /* current file index */
  char *src_file;  /* name of main input source file */
  char *curr_file; /* name of current input source file */
  char *module;    /* object module name */
  char *ipaname;   /* IPA database name */
  FILE *srcfil;    /* file pointer for source input file */
  FILE *cppfil;    /* file pointer for preprocessor output */
  FILE *dbgfil;    /* file pointer for debug file */
  FILE *ilmfil;    /* file pointer for (temporary) ILM file */
  FILE *objfil;    /* file pointer for output object file */
  FILE *asmfil;    /* file pointer for output assembly file */
  FILE *outfil;    /* file pointer for source output file */
  FILE *symfil;    /* file pointer for symbol output file */
  FILE *gblfil;    /* file pointer for static global info output file */
  FILE *stbfil;    /* file pointer for symbols and datatypes */
  LOGICAL eof_flag;
  SPTR currsub;     /* symtab ptr to current subprogram */
  SPTR outersub;    /* symtab ptr to host subprogram, if any, or zero */
  int outerentries; /* list of entry symbols to host subprogram, if any, or zero
                       */
  SPTR currmod;     /* symtab ptr to module symbol, if any, or zero */
  LOGICAL arets;    /* set to true if any entry contains an
                       alternate return.  */
  RU_TYPE rutype;   /* RU_PROG, RU_SUBR, RU_FUNC, or RU_BDATA */
  int funcline;     /* line number of header statement */
  int cmblks;       /* pointer to list of common blocks */
  int externs;      /* pointer to list of external functions */
  int consts;       /* pointer to list of referenced constants */
  int entries;      /* list of entry symbols */
  int statics;      /* list of "static" variables */
  int locals;       /* pointer to list of local variables   */
  int asgnlbls;     /* pointer to list of labels appearing in assign stmts.*/
  int ent_select;   /* sptr of int variable whose value (0 .. #entries-1)
                     * denotes which entry was entered. this is zero if
                     * ENTRYs aren't present.
                     */
  int stfuncs;      /* list of statement functions defined in subprogram */
  ISZ_T locaddr;    /* current available address for local variables,
                     * (positive offset from $local)  */
  ISZ_T saddr;      /* current available address for static variables,
                     * (positive offsets from $static.  */
  int autobj;       /* list of automatic data objects; the st field
                     * AUTOBJ is used to link together the objects; NOSYM
                     * terminates the list.
                     */
  int exitstd;      /* pointer to std after which exit code (epilogue)
                     * is added for the current subprogram
                     */
  char datetime[21];
  int entbih;           /* entry bih of a function, set by expander/optimizer
                         * to communicate with other modules.
                         */
  int func_count;       /* function counter, current # of function being
                         * compiled, incremented by assem_init */
  char *file_name;      /* full pathname of input file; -file may override */
  int ftn_true;         /* value of .TRUE.; -1 (default) or 1 (-x 125 8) */
  LOGICAL in_include;   /* set to true if source is from an include file */
  int tp_adjarr;        /* list of template and processor adjustable array
                         * objects; the AUTOBJ st field is the link field;
                         * NOSYM terminates the list.
                         */
  int p_adjarr;         /* pointer to list of based adjustable array-objects;
                         * the SYMLK st field is the link field; NOSYM
                         * terminates the list.
                         */
  int p_adjstr;         /* pointer to list of adjustable lenght string objects;
                         * the SYMLK st field is the link field; NOSYM
                         * terminates the list.
                         */
  LOGICAL nowarn;       /* if TRUE, don't issue warning & informational errors*/
  char *prog_file_name; /* file name containing the 'module', 'program',
                         * 'subroutine', 'function', or 'blockdata' stmt.
                         * follows include files, if necessary. */
  int internal;         /* internal subprogram state:
                         * 0 - current subprogram does not contain internal
                         *     subprograms.
                         * 1 - current subprogram contains internal subprograms
                         *     (current subprogram is the 'host' subprogram).
                         * >1 - current subprogram is an internal subprogram.
                         */
  LOGICAL nofperror;    /* if TRUE, error.c:fperror() does not report errors */
  int fperror_status;   /* error status of a floating point operation
                         * performed by scutil.
                         */
  int sym_nproc;        /* symbol number of hpf$np symbol */
  LOGICAL is_f90;       /* frontend is for Fortran 90 */
  FILE *ipafil;         /* propagated ipa information */
  FILE *ipofil;         /* newly generated ipa information */
  FILE *dependfil;      /* make dependency information */
  FILE *moddependfil;   /* make dependency information */
  char *fn;             /* name of file being compiled which was previously
                         * preprocessed (can be more general if we choose).
                         */
  LOGICAL denorm;       /* enforce denorm for the current subprogram */
  LOGICAL inomptarget;  /* set if it is OpenMP's target region*/
} GBL;

#undef MAXCPUS
#define MAXCPUS 256

/* Max number of dimensions.  F'2008 requires 15,  Intel is 31. */
#define MAXRANK 7

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
  LOGICAL output;
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
  LOGICAL dlines;
  int extend_source;
  LOGICAL i4;
  LOGICAL line;
  LOGICAL symbol;
  int profile;
  LOGICAL standard;
  int dbg[67];
  LOGICAL dalign; /* TRUE if doubles are double word aligned */
  int astype;     /* target dependent value to support multiple asm's */
  LOGICAL recursive;
  int ieee;
  int inliner;
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
  char **undef;
  char *stdinc; /* NULL => use std include; 1 ==> do not look in
                 * std dir; o.w., use value as the std dir */
  LOGICAL hpf;
  LOGICAL freeform;
  LOGICAL sequence;
  int ipa;
  LOGICAL craft_supported;
  LOGICAL doprelink; /* generate the .prelink.f file */
  LOGICAL genilm;    /* generate ilm, not fortran, output */
  LOGICAL defaulthpf;
  LOGICAL defaultsequence;
  int errorlimit;
  LOGICAL omptarget;  /* TRUE => allow omp accel directives */
  LOGICAL smp; /* TRUE => allow smp directives */
  int tpcount;
  int tpvalue[TPNVERSION]; /* target processor(s), for unified binary */
  int accmp;
  char *cmdline; /* command line used to invoke the compiler */
} FLG;

extern FLG flg;

#endif
