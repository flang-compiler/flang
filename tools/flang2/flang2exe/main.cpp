/*
 * Copyright (c) 1997-2018, NVIDIA CORPORATION.  All rights reserved.
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
   \brief Fortran backend main program and initialization routines.
 */

#include "main.h"
#include "dinit.h"
#include "dinitutl.h"
#include "global.h"
#include "version.h"
#include "machar.h"
#include "upper.h"
#include "fih.h"
#include "cgraph.h"
#include "x86.h"
#include "dbg_out.h"
#include "xref.h"
#include "exp_rte.h"
#include "rmsmove.h"
#include "mwd.h"
#include "llassem.h"
#include "cgllvm.h"
#include "outliner.h"
#if !defined(TARGET_WIN)
#include <unistd.h>
#endif
#include <time.h>
#include "ilm.h"
#include "ili.h"
#include "upper.h"
#include "semant.h"
#include "dwarf2.h"
#include "direct.h"
#include "expand.h"
#include "scope.h"
#include <stdbool.h>
#include "flang/ArgParser/arg_parser.h"
#include "dtypeutl.h"

static bool process_input(char *argv0, bool *need_cuda_constructor);

/** Product name in debug output
 */
#ifdef FLANG_VENDOR
#define DNAME FLANG_VENDOR "F90"
#else
#define DNAME "F90"
#endif

#if DEBUG & sun
#ifndef _ERRNO_H
extern int errno;
#endif
#endif

#define IS_COFF (flg.astype == 1)
#define IS_ELF (flg.astype == 0)
#define IS_STABS (XBIT(120, 0x20))
#define IS_DWARF (IS_ELF || XBIT(120, 0x80) || XBIT(120, 0x200))
#define IS_COFFDBG (IS_COFF && !XBIT(120, 0x80) && !XBIT(120, 0x200))

/* contents of this file:  */

static void reptime(void);
static void init(int, char *[]);
static void reinit(void);

static int saveoptflag;
static int savevectflag;
static int savex8flag;
static int saverecursive;
static char *objectfile;
static void process_stb_file(void);
#define STB_UPPER() (gbl.stbfil != NULL)
#define IS_PARFILE (gbl.ilmfil == par_file1 || gbl.ilmfil == par_file2)

static action_map_t *phase_dump_map;

/*
 * for reporting time
 */
static char *who[] = {"init", "import",   "expand", "", "",
                      "",     "assemble", "xref",   ""};
#define _N_WHO (sizeof(who) / sizeof(char *))
static INT xtimes[_N_WHO];
static char *cmdline = NULL;
static char *ccff_filename = NULL;
#include "ccffinfo.h"

#if DEBUG
static int dodebug = 0;
#define TR(str)               \
  if (dodebug) {              \
    fprintf(gbl.dbgfil, str); \
    fflush(gbl.dbgfil);       \
  }
#else
#define TR(str)
#endif

#if DEBUG && !defined(EXTRACTOR) && (defined(X86_64) || defined(TARGET_POWER))
#define DEBUGQQ 1
#else
#define DEBUGQQ 0
#endif

#if defined(FLANG_VENDOR) && defined(DNAME)
static char *dbg_feature = DNAME;
#else
static char *dbg_feature = "flang";
#endif

static int ipa_import_mode = 0;

#define DUMP(a)

#define NO_FLEXLM

#if DEBUG
#include "bih.h"
#include "ilt.h"
/** \brief Check line number, findex  after each various stages to make sure
 * they don't contain 0 as a line number, so that ccff_info don't get
 * linenumber as 0
 */
static void
check_lineno(char *phase)
{
  int block;
  int ilt;
  int first = 1;

  if (DBGBIT(73, 1)) {
    for (block = BIH_NEXT(0); block; block = BIH_NEXT(block)) {
      /* For debugged code, the first block line number is 0 *
         therefor we ignore it */
      if (flg.debug || XBIT(120, 0x1000) || (XBIT(123, 0x400) && first)) {
        first = 0;
        if (BIH_LINENO(block) == 0)
          continue;
      }
      if (BIH_LINENO(block) == 0) {
        printf("ERROR: BIH_LINENO() is 0 after :%s\n", phase);
      }
      if (BIH_FINDEX(block) == 0) {
        printf("ERROR: BIH_FINDEX() is 0 after :%s\n", phase);
      }
      for (ilt = BIH_ILTFIRST(block); ilt; ilt = ILT_NEXT(ilt)) {
        if (ILT_LINENO(ilt) == 0) {
          printf("ERROR: ILT_LINENO() is 0 after :%s\n", phase);
        }
        if (ILT_FINDEX(ilt) == 0) {
          printf("ERROR: ILT_FINDEX() is 0 after :%s\n", phase);
        }
      }
    }
  }
}
#endif

/** \brief Process one input file
 * \param argv0 path to initalize accelerator compilation
 * \param need_cuda_constructor indicates that need to add a CUDA construction,
 * this parameter is also written to
 * \return false to indicated end of processing, true otherwise
 */
static bool
process_input(char *argv0, bool *need_cuda_constructor)
{
  static int accsev = 0;
  bool have_data_constructor = false;
  bool is_constructor = false;

llvm_restart:
  if (gbl.maxsev > accsev)
    accsev = gbl.maxsev;

#if DEBUG & sun
  if (DBGBIT(7, 4))
    if (malloc_verify() != 1)
      interr("main: malloc_verify failsA", errno, ERR_Fatal);
#endif
    reinit();
#if DEBUG & sun
  if (DBGBIT(7, 4))
    if (malloc_verify() != 1)
      interr("main: malloc_verify failsB", errno, ERR_Fatal);
#endif
  xtimes[0] += getcpu();
  /* don't increment if it is outlined function because it
   * uses STATICS/BSS from host routine.
   */
  if (flg.smp && IS_PARFILE) {
    ll_set_outlined_currsub();
  }
  gbl.func_count++;

  if (gbl.multiversion <= 1) {
    TR(DNAME " ILM INPUT begins\n")
    if (!IS_PARFILE)
    {
      upper(0);
      if (gbl.eof_flag)
        return false;
      upper_assign_addresses();
    }
  }

  is_constructor = gbl.cuda_constructor;
  xtimes[1] += getcpu();
  DUMP("upper");

  if (gbl.cuda_constructor) {
    have_data_constructor = false;
/* Generate the CUDA constructor */
    gbl.cuda_constructor = 0;
    return true;
  }
  if (DBGBIT(5, 1))
    symdmp(gbl.dbgfil, DBGBIT(5, 8));

  if (gbl.maxsev < 3 && (flg.object || flg.code) && !DBGBIT(2, 4)) {

    if (flg.debug)
      process_global_lifetime_debug();

    gbl.multi_func_count++;
    gbl.nofperror = true;
    if (gbl.rutype == RU_BDATA) {
    } else {
      if (gbl.cuda_constructor) {
      } else {
        /*
         * direct_rou_load() checks for assigned goto's and variable
         * format specifiers.
         *
         * the optimizer doesn't handle assigned goto's correctly.
         * (Doesn't know where to put loop exit code if you assign
         * goto out of loop).
         * the optimizer doesn't handle the control flow correctly
         * for the variable-format functions.
         */
        direct_rou_load(gbl.currsub);
        cuda_emu_start();
        if (XBIT(14, 16) && !XBIT(89, 0x40)) {
        }

        if (XBIT(14, 0x20000) || !XBIT(14, 0x10000)) {
          {
            if (rdgilms(1) == 0) {
              if (flg.smp) {
                if (ll_reset_parfile()) {
                  gbl.eof_flag = false;
                  goto llvm_restart;
                } else if (!IS_PARFILE) {
                  goto llvm_restart;
                }
              }
              return false;
            }

          }
          if (XBIT(70, 0x18080)) {
            AssignAddresses(); /* exp_rte.c */
          }
        }
        TR(DNAME " EXPANDER begins\n");

        expand(); /* expand ILM's into ILI  */
        DUMP("expand");
#if DEBUG
        check_lineno("expand");
#endif

      }

      {

/* move call to intensity earlier to prevent too much loop restructuring */

        if (XBIT_USE_SCOPE_LABELS)
          remove_scope_labels();

        if (flg.opt && !XBIT(6, 0x40000)) {
          /* remove useless split blocks */
          unsplit();
#if DEBUG
          check_lineno("unsplit");
#endif
          DUMP("unsplit");
        }
        rm_smove();
        DUMP("rmsmove");

        TR(DNAME " SCHEDULER begins\n");
        DUMP("before-schedule");
        schedule();
        xtimes[5] += getcpu();
        DUMP("schedule");
      } /* CUDAG(GBL_CURRFUNC) & CUDA_HOST */
    }
    TR(DNAME " ASSEMBLER begins\n");
    assemble();
    xtimes[6] += getcpu();
    upper_save_syminfo();
  }
  if (DBGBIT(5, 4))
    symdmp(gbl.dbgfil, DBGBIT(5, 8));
  if (DBGBIT(5, 16))
    dmp_dtype();
  if (gbl.rutype != RU_BDATA) {
    cuda_emu_end();
      /* TDB: make it look better!*/
      if (!flg.smp)
        direct_rou_end();
      else if (!ll_has_more_outlined())
        direct_rou_end();
      else if (!ALLOW_NODEPCHK_SIMD)
        direct_rou_end();
  }
  if (!flg.smp || !ll_has_more_outlined())
  {
    ccff_close_unit();
  }

  if (flg.inliner && !XBIT(117, 0x10000)
      && !IS_PARFILE
      ) {
  }

  if (flg.xref) {
    xref(); /* write cross reference map */
    xtimes[7] += getcpu();
  }
  (void)summary(false, 0);
  cg_llvm_fnend();
  if (llProcessNextTmpfile()) {
    if (ll_reset_parfile())
      return true;
  }
  return true;
}

/** \brief Fortran backend main entry.
 */
int
main(int argc, char *argv[])
{
  static unsigned int ckey, rkey;
  bool findex = false;
  bool need_constructor = false;
  int accel_cnt, accel_vendor = 0;

  getcpu();
  init(argc, argv);

  saveoptflag = flg.opt;
  savevectflag = flg.vect;
  savex8flag = flg.x[8];
  saverecursive = flg.recursive;
  gbl.multiversion = 0;
  gbl.numversions = 1;

#if DEBUG & sun
  if (DBGBIT(7, 4))
    malloc_debug(2);
#endif
  if (XBIT(14, 0x20000) || !XBIT(14, 0x10000)) {
    init_global_ilm_mode();
  }

  if (STB_UPPER()) {
    stb_upper_init();
    gbl.findex = addfile(gbl.file_name, NULL, 0, 0, 0, 1, 0);
    process_stb_file();
    findex = true;
  }

  upper_init();
  if (!findex)
    gbl.findex = addfile(gbl.file_name, NULL, 0, 0, 0, 1, 0);

  do { /* loop once for each user program unit */

    if (!process_input(argv[0], &need_constructor))
      break;

  } while (!gbl.eof_flag);

  cg_llvm_end();

  if (flg.smp) {
    ll_unlink_parfiles();
  }

  finish(); /* finish does not return */
  return 0; /* never reached */
}

static void
reptime()
{
  char buf[80];
  int i;
  INT total;
  int prct;
  int tmp;

  total = 0;
  for (i = 0; i < _N_WHO; i++)
    total += xtimes[i];

  if (!DBGBIT(0, 8) || DBGBIT(14, 3))
    goto xbitcheck;

  if (flg.code || flg.list || flg.xref) {
    list_line("");
    list_line("  Timing stats:");
  } else if (gbl.dbgfil)
    fprintf(gbl.dbgfil, "  Timing stats:\n");
  for (i = 0; i < _N_WHO; i++) {
    if (xtimes[i]) {
      tmp = 100 * xtimes[i];
      prct = tmp / total;
      sprintf(buf, "    %-10.10s %15d millisecs %5d%%", who[i], xtimes[i],
              prct);
      if (flg.code || flg.list || flg.xref)
        list_line(buf);
      else if (gbl.dbgfil)
        fprintf(gbl.dbgfil, "%s\n", buf);
    }
  }

  sprintf(buf, "    Total time %15d millisecs", total);
  if (flg.code || flg.list || flg.xref) {
    list_line(buf);
  } else if (gbl.dbgfil)
    fprintf(gbl.dbgfil, "%s\n", buf);

xbitcheck:
  if (!XBIT(0, 1))
    return;
  fprintf(stderr, "  Timing stats:\n");

  for (i = 0; i < _N_WHO; i++) {
    if (xtimes[i]) {
      tmp = 100 * xtimes[i];
      prct = tmp / total;
      sprintf(buf, "    %-10.10s %15d millisecs %5d%%", who[i], xtimes[i],
              prct);
      fprintf(stderr, "%s\n", buf);
    }
  }
  sprintf(buf, "    Total time %15d millisecs", total);
  fprintf(stderr, "%s\n", buf);
}

/** \brief Dump symbols
 *
 * Wrapper that takes no arguments
 */
static void
dump_symbols(void)
{
  dsyms(0, 0);
}

/** Dump memory area
 *
 * Wrapper that takes not arguments
 */
static void
report_area(void)
{
  reportarea(0);
}

/** \brief Initialize Fortran backend at the begining of compilation.
 */
static void
init(int argc, char *argv[])
{
  int argindex;
  int next;
  char *argstring;
  int indice;
  char *sourcefile;
  char *listfile;
  char *stboutfile;
  char *cppfile;
  char *tempfile;
  char *asmfile;
  int c;
  int def_count = 0;  /* number of -def switches */
  int idir_count = 0; /* number of -idir switches */
  INT qval1;
  INT qval2;
  int val_follows;
  bool dbgflg;
  bool errflg;
  FILE *fd;
  int exlib_flag = 0;
  char *file_suffix;
  char *idfname;
  time_t now;

  file_suffix = FTNFILE; /* default suffix for source files */
                         /*
                          * initialize error and symbol table modules in case error messages are
                          * issued:
                          */
  errini();
  gbl.curr_file = NULL;
  sym_init();

  /* fill in date and time */
  time(&now);
  strftime(gbl.datetime, sizeof gbl.datetime, "%m/%d/%Y  %H:%M:%S",
           localtime(&now));

  dbgflg = false;
  errflg = false;

  bool arg_reentrant;  /* Argument to enable generating reentrant code */

  sourcefile = NULL;
  listfile = NULL;
  cppfile = NULL;
  objectfile = NULL;
  asmfile = NULL;
  stboutfile = NULL;

  /* Create a datastructure of various dump actions and their names */
  action_map_t *dump_map; /* Deallocated after arguments are parsed */
  create_action_map(&dump_map);
  add_action(dump_map, "bih", dbih);
  add_action(dump_map, "block", dbih);
  add_action(dump_map, "blocks", dbih);
  add_action(dump_map, "blockonly", dbihonly);
  add_action(dump_map, "blocksonly", dbihonly);
  add_action(dump_map, "diff", dumpdiff);
  add_action(dump_map, "dtype", dumpdtypes);
  add_action(dump_map, "dvl", dumpdvls);
  add_action(dump_map, "ilts", dumpilts);
  add_action(dump_map, "nme", dumpnmes);
  add_action(dump_map, "pblock", printblocks);
  add_action(dump_map, "pblocks", printblocks);
  add_action(dump_map, "printblocks", printblocks);
  add_action(dump_map, "stack", stackcheck);
  add_action(dump_map, "stackvars", stackvars);
  add_action(dump_map, "sym", dump_symbols);
  add_action(dump_map, "syms", dump_symbols);
  add_action(dump_map, "symtab", dump_symbols);
  add_action(dump_map, "area", report_area);
  add_action(dump_map, "gbl", dgbl);
  add_action(dump_map, "flg", dflg);
  /* Initialize the map that will be used by dump handler later */
  create_action_map(&phase_dump_map);

  /* Need to up optimization level if we modify unroller count */
  int old_unroller_cnt = flg.x[9];
  /* Target architecture string */
  char *tp;
  /* Vectorizer settings */
  int vect_val;

  /* Argument parser */
  arg_parser_t *arg_parser;

  /* Create argument parser, force errors on unknown flags (last argument is
   * true) */
  create_arg_parser(&arg_parser, true);

  /* Input file name */
  register_filename_arg(arg_parser, &sourcefile);
  /* Fortran source file */
  register_string_arg(arg_parser, "fn", &(gbl.file_name), NULL);
  /* Other files to input or output */
  register_string_arg(arg_parser, "stbfile", &stboutfile, NULL);
  register_combined_bool_string_arg(arg_parser, "asm", (bool *)&(flg.asmcode),
                                    &asmfile);

  /* Register version arguments */
  register_string_arg(arg_parser, "vh", (char**)&(version.host), "");

  /* x flags */
  register_xflag_arg(arg_parser, "x", flg.x,
                     (sizeof(flg.x) / sizeof(flg.x[0])));
  register_yflag_arg(arg_parser, "y", flg.x);
  /* Debug flags */
  register_qflag_arg(arg_parser, "q", flg.dbg,
                     (sizeof(flg.dbg) / sizeof(flg.dbg[0])));
  register_action_map_arg(arg_parser, "qq", phase_dump_map, dump_map);

  /* Other flags */
  register_integer_arg(arg_parser, "opt", &flg.opt, 1);
  register_integer_arg(arg_parser, "ieee", &flg.ieee, 0);
  register_inform_level_arg(arg_parser, "inform",
                            (inform_level_t *)&flg.inform, LV_Inform);
  register_integer_arg(arg_parser, "endian", &flg.endian, 0);
  register_boolean_arg(arg_parser, "mp", &flg.smp, false);
  register_boolean_arg(arg_parser, "reentrant", &arg_reentrant, false);
  register_integer_arg(arg_parser, "terse", &flg.terse, 1);
  register_boolean_arg(arg_parser, "quad", &flg.quad, false);
  register_boolean_arg(arg_parser, "save", &flg.save, false);
  register_string_arg(arg_parser, "tp", &tp, NULL);
  register_integer_arg(arg_parser, "astype", &flg.astype, 0);
  register_boolean_arg(arg_parser, "recursive", &flg.recursive, false);
  register_integer_arg(arg_parser, "vect", &vect_val, 0);
  register_string_arg(arg_parser, "cmdline", &cmdline, NULL);
  register_boolean_arg(arg_parser, "debug", &flg.debug, false);

  /* Run argument parser */
  parse_arguments(arg_parser, argc, argv);

  /* Process debug output settings */
  if (was_value_set(arg_parser, &(flg.dbg)) ||
      was_value_set(arg_parser, phase_dump_map)) {
#if DEBUG
    dodebug = 1;
#endif
    if (flg.dbg[0] & 1) {
      gbl.dbgfil = stderr;
    } else {
      char *s, *ss, *t;
      s = NULL;
      t = gbl.file_name;
      for (ss = gbl.file_name; *ss; ++ss) {
        if (*ss == '.')
          s = ss;
        if ((*ss == '/'
#if defined(HOST_WIN)
             || *ss == '\\'
#endif
             ) &&
            *(ss + 1))
          t = ss + 1;
      }
      if (s == NULL)
        s = ".f90";
        tempfile = mkfname(t, s, ".qdbg");
      if ((gbl.dbgfil = fopen(tempfile, "w")) == NULL)
        errfatal((error_code_t)5);
    }
  }

  /* Enable reentrant code */
  if (was_value_set(arg_parser, &arg_reentrant)) {
    if (arg_reentrant) {
      flg.x[7] |= 0x2;      /* inhibit terminal func optz. */
      flg.recursive = true; /* no static locals */
    } else {
      flg.x[7] &= ~(0x2);
      flg.recursive = false;
    }
  }

  /* Free memory */
  destroy_arg_parser(&arg_parser);
  destroy_action_map(&dump_map);

  /* Go ahead and up optimization level if user modified maximum count for
   * unroller */
  if (flg.x[9] != old_unroller_cnt && flg.opt < 2)
    flg.opt = 2;

  /* Check optimization level */
  if (flg.opt > 4) {
    fprintf(stderr, "%s-W-Opt levels greater than 4 are not supported\n", version.lang);
  }

  /* Vectorizer settings */
  flg.vect |= vect_val;
  if (flg.vect & 0x10)
    flg.x[19] &= ~0x10;
  if (flg.vect & 0x20)
    flg.x[19] &= ~8;
  set_yflag(34, 0x30);

/* Postprocess target architecture */
  if (tp) {
#ifdef TARGET_X86
    if (strcmp(tp, "x64") == 0) {
      set_tp("k8-64");
      set_tp("p7-64");
    } else {
      set_tp(tp);
    }
#endif
  }

  flg.x[79] = 16; /* Hardwire XBIT(79,16): CSE DP loads for a distance of 16 */

/* set flag defaults */
  /*
   * may be disabled by the command line processing:
   *   flg.x[15] |= 1;  '<x> / <const>' -> '<x> * (1 / <const>)'
   *   flg.x[15] |= 2;  'x/y/z'         -> 'x/(y*z)'
   *   flg.x[124] |= 0x400; treat int8 as full int8; default was to treat
   *                        int8 in storge with int4 ops
   *
   * 01/17/17 - control x/const -> x*(1/const) by -Mfprelaxed=div
   */
  flg.x[15] |= 2;
  flg.x[124] |= 0x400;

  if (flg.smp) {
    flg.x[7] |= 0x2;   /* inhibit terminal func optz. */
    flg.x[36] |= 0x1;  /* vcache is on the stack for -Mvect */
    flg.x[125] |= 0x1; /* -Miomutex */
  }

  if (XBIT(25, 0xf0)) {
    fprintf(stderr, "%s-I-Beta Release Optimizations Activated\n",
            version.lang);
  }

empty_cl:
  if (sourcefile == NULL) {
    gbl.src_file = sourcefile = "STDIN.f";
    gbl.srcfil = stdin;
    goto do_curr_file;
  }

  if (errflg)
    finish();

  /* open sourcefile */
  if ((gbl.srcfil = fopen(sourcefile, "r")) == NULL) {
    error((error_code_t)2, ERR_Fatal, 0, sourcefile, "");
  } else {
    char *s;
    gbl.src_file = (char *)malloc(strlen(sourcefile) + 1);
    strcpy(gbl.src_file, sourcefile);
    basenam(gbl.src_file, "", sourcefile);
    file_suffix = "";
    for (s = sourcefile; *s; ++s) {
      if (*s == '.')
        file_suffix = s;
    }
  }

do_curr_file:

  if (gbl.file_name == NULL)
    gbl.file_name = gbl.src_file;
  gbl.curr_file = gbl.file_name;
  gbl.module = mkfname(sourcefile, file_suffix, "");
  if (flg.smp) {
    ll_open_parfiles();
  }

  /* process  ilm  and object file: */

  gbl.ilmfil = gbl.objfil = NULL;
  if (!flg.es && (flg.object || flg.code)) {
    /* create temporary file for ilms */
    if ((gbl.ilmfil = tmpf("b")) == NULL)
      errfatal((error_code_t)5);
  }
  /* process listing file */
  if (flg.code || flg.list || flg.xref) {
    if (listfile == NULL) {
      /* make listing filename from sourcefile name */
      listfile = mkfname(sourcefile, file_suffix, LISTFILE);
    }
    if ((fd = fopen(listfile, "w")) == NULL)
      errfatal((error_code_t)3);
    list_init(fd);
  }
  /* process assembly output file */
  if (flg.asmcode) {
    if (asmfile == NULL) {
      /* make assembly filename from sourcefile name */
      asmfile = mkfname(sourcefile, file_suffix, ASMFILE);
    }
    if ((gbl.asmfil = fopen(asmfile, "w")) == NULL)
      errfatal((error_code_t)9);
  } else /* do this for compilers which write asm code to stdout */
    gbl.asmfil = stdout;

  if (stboutfile) {
    if ((gbl.stbfil = fopen(stboutfile, "r")) == NULL)
      error((error_code_t)2, ERR_Fatal, 0, stboutfile, "");
  } else {
    gbl.stbfil = NULL;
  }

#if DEBUG
  assert(flg.es == 0, "init:flg.esA", 0, ERR_unused);
#endif
  assemble_init(argc, argv, cmdline);

  gbl.func_count = 0;
  gbl.multi_func_count = 0;
  direct_init();

  if (XBIT(125, 0x8))
    gbl.ftn_true = 1;
  else
    gbl.ftn_true = -1;

  if (ccff_filename) {
    ccff_open(ccff_filename, gbl.file_name ? gbl.file_name : gbl.src_file);
    ccff_build(cmdline, "F90");
  }

}

/** \brief Perform initializations for new user subprogram unit:
 */
static void
reinit(void)
{

  /* initialize global variables:  */

  gbl.currsub = SPTR_NULL;
  gbl.arets = false;
  gbl.rutype = RU_PROG;
  gbl.cmblks = NOSYM;
  gbl.externs = NOSYM;
  gbl.consts = NOSYM;
  gbl.locals = NOSYM;
  gbl.statics = NOSYM;
  gbl.bssvars = NOSYM;
  gbl.basevars = NOSYM;
  gbl.outlined = 0;
  gbl.usekmpc = 0;
  gbl.typedescs = NOSYM;
  gbl.vfrets = 0;
  gbl.caddr = 0;
  gbl.locaddr = 0;
  gbl.saddr = 0;
  gbl.silibcnt = 0;
  gbl.asgnlbls = SPTR_NULL;
  gbl.loc_arasgn = 0;
  gbl.nofperror = false;
  gbl.pgfi_avail = 0;
  gbl.denorm = false;
  /* restore opt flag to its original value */
  flg.opt = saveoptflag;
  flg.vect = savevectflag;
  flg.x[8] = savex8flag;
  flg.recursive = saverecursive;

  errini(); /* initialize error module */
  if (!IS_PARFILE) /* use old symbold tables */
  {
    sym_init(); /* initialize symbol table module */
    gbl.threadprivate = NOSYM;
  }
  dinit_init();  /* initialize data init file module  */
  semant_init(); /* initialize semantic analyzer */
  if (flg.xref)
    xrefinit(); /* initialize cross reference module */
}

/** \Brief Write summary line to terminal, and exit.
 */
void
finish()
{
  int maxfilsev;

  if (!flg.es) {
    reptime();
    maxfilsev = summary(true, 1);
  } else
    maxfilsev = gbl.maxsev;

  if (maxfilsev >= 3) {
    /* remove objectfile if there were severe errors */
    if (flg.object && gbl.objfil)
      if (!DBGBIT(0, 16))
        unlink(objectfile);
  } else {
    if (gbl.objfil != NULL)
      fclose(gbl.objfil);
    if (!flg.es)
      assemble_end();
  }
  if (gbl.asmfil != NULL && gbl.asmfil != stdout)
    fclose(gbl.asmfil);

  if (gbl.ilmfil != NULL)
    fclose(gbl.ilmfil);
  gbl.ilmfil = NULL;
  if (gbl.srcfil != NULL && gbl.srcfil != stdin)
    fclose(gbl.srcfil);
  gbl.srcfil = NULL;

  if (gbl.stbfil != NULL)
    fclose(gbl.stbfil);
  gbl.stbfil = NULL;
  if (flg.smp) {
    ll_unlink_parfiles();
  }

  if (ccff_filename)
    ccff_close();

  freearea(8); /* temporary filenames and pathnames space  */
  destroy_action_map(&phase_dump_map);
  free_getitem_p();
  if (maxfilsev >= 3)
    exit(1);
  else
    exit(0);
}

static void
process_stb_file()
{
  bool wrote_llvm = false;

  if (!STB_UPPER())
    return;

  do {

    reinit();

    gbl.func_count++;

    TR(DNAME " STBFILE INPUT begins\n")
    upper(1); /* should we generate upper_stbfil()? */

    if (gbl.eof_flag)
      break;

    upper_assign_addresses();

    cg_llvm_init();
    stb_process_routine_parameters();
    fix_llvm_fptriface();
    cg_llvm_fnend();
    wrote_llvm = true;

    upper_save_syminfo();
    add_aguplevel_oldsptr();

  } while (!gbl.eof_flag);

  /* LLVM init needs to be called so we end up with a valid LLVM module
   * that we can compile (even if there is no user code) */
  if (!wrote_llvm) {
    cg_llvm_init();
  }

  gbl.eof_flag = false;
  gbl.func_count = 0;

  if (gbl.stbfil != NULL)
    fclose(gbl.stbfil);
  gbl.stbfil = NULL;

  return;

}

/* FIXME this function is only used in upper.c, but we are hopping through two
 * files to get it there */
void
add_llvm_uplevel_symbol(int sptr)
{
  _add_llvm_uplevel_symbol(sptr);
}

/* FIXME this function is only used in upper.c, but we are hopping through two
 * files to get it there */
void
fixup_llvm_uplevel_symbol()
{
  _fixup_llvm_uplevel_symbol();
}

/* helper functions for bottom-up inlining, which requires defining EXTRACTOR */
