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
    \brief Fortran module support.
 */

#include "gbldefs.h"
#include "global.h"
#include "error.h"
#include "symtab.h"
#include "dtypeutl.h"
#include "machar.h"
#include "semant.h"
#include "symutl.h"
#include "dinit.h"
#include "pd.h"
#include "interf.h"
#include "ast.h"
#include "rte.h"
#include "soc.h"
#include "state.h"
#include "lz.h"
#include "dbg_out.h"

static int module_id;
static char *modu_file_name;

#define MOD_SUFFIX ".mod"

static FILE *use_fd;
static char *use_file_name;
#define ALL_MODULES 3
#define ISO_C_MOD 3
#define IEEE_ARITH_MOD 4
#define IEEE_FEATURES_MOD 5
#define ISO_FORTRAN_ENV 6
#define NML_MOD 7
#define FIRST_USER_MODULE 8

static LOGICAL seen_contains;

/* collect 'only', 'only' with rename, or just rename */
typedef struct _rename {
  int local;  /* sptr representing local name; 0 if rename doesn't
               * occur
               */
  int global; /* sptr representing global name */
  int lineno;
  char complete;    /* set when found as an intrinsic (currently
                       iso_c only) */
  char is_operator; /* only/rename of the global is for an operator */
  struct _rename *next;
} RENAME;

/* describe entries in a module */
typedef struct {
  int nmptr;
  int sptr;
} MODENT;

#define MODNAME(m, i) (stb.n_base + usedb.base[m].modent[i].nmptr)

typedef struct {
  int module;           /* sptr representing the name of the module in the
                         * USE statement
                         */
  LOGICAL unrestricted; /* entire module file is read */
  RENAME *rename;
  char *fullname; /* full path name of the module file */
  MODENT *modent; /* only used for hpf_library &
                   * hpf_local_library
                   */
  int size;       /* size of MODENT table located by base */
} USED;

static struct {/* for recording modules used in a scoping unit */
  USED *base;
  int avl;
  int sz;
  int *ipasave_modname;
  int ipasave_avl, ipasave_sz;
} usedb = {NULL, 0, 0, NULL, 0, 0};

static int limitsptr;

static int get_entry(int, char *);
static void add_predefined_module(int, int);
static void add_predefined_isoc_module(void);
static void add_predefined_iso_fortran_env_module(void);
static void add_predefined_ieeearith_module(void);
static void apply_use(int);
static void read_module(LOGICAL, int *, int);
static int basedtype(int sym);
static void fix_module_common(void);
static void export_public_used_modules(int scopelevel);
static void add_to_common(int cmidx, int mem, int atstart);
static void export_all(void);
static void make_rte_descriptor(int obj, char *suffix);

/* ------------------------------------------------------------------ */
/*   USE statement  */

ref_symbol dbgref_symbol = {NULL, 0, NULL};

/* Allocate memory for reference symbols with size of stb.symavl */
void
allocate_refsymbol(int symavl)
{
  if (dbgref_symbol.symnum == NULL) {
    dbgref_symbol.symnum = (int *)(malloc((symavl + 10) * sizeof(int)));
    dbgref_symbol.altname =
        (mod_altptr *)(malloc((symavl + 10) * sizeof(mod_altptr)));
    dbgref_symbol.size = symavl + 10;
    BZERO((void *)dbgref_symbol.symnum, int, (dbgref_symbol.size));
    BZERO((void *)dbgref_symbol.altname, mod_altptr, (dbgref_symbol.size));
  } else if (dbgref_symbol.size <= symavl) {
    dbgref_symbol.symnum =
        (int *)(realloc(dbgref_symbol.symnum, (symavl + 10) * sizeof(int)));
    dbgref_symbol.altname = (mod_altptr *)(realloc(
        dbgref_symbol.altname, (symavl + 10) * sizeof(mod_altptr)));

    BZERO((void *)(dbgref_symbol.symnum + dbgref_symbol.size), int,
          symavl - dbgref_symbol.size + 10);
    BZERO((void *)(dbgref_symbol.altname + dbgref_symbol.size), mod_altptr,
          symavl - dbgref_symbol.size + 10);
    dbgref_symbol.size = symavl + 10;
  }
}

/* reinitialize reference symbols from symavl on,
 * we want to keep anything under symavl because that could come from module.
 */
static void
reinit_refsymbol(int symavl)
{
  int i;
  mod_altptr symptr;

  if (symavl > dbgref_symbol.size)
    return;

  /* zero out all symbols that are referenced in previous routine if any */
  BZERO((void *)dbgref_symbol.symnum, int, dbgref_symbol.size);

  /* Keep USEd names around for module */
  for (i = symavl; i < dbgref_symbol.size; ++i) {
    for (; dbgref_symbol.altname[i]; dbgref_symbol.altname[i] = symptr) {
      symptr = dbgref_symbol.altname[i]->next;
      FREE(dbgref_symbol.altname[i]);
    }
    dbgref_symbol.altname[i] = NULL;
  }
}

/* Create link list of renames */
void
set_modusename(int local, int global)
{
  char *localname;
  mod_altptr symptr;
  mod_altptr fr_ptr, to_ptr;

  if (dbgref_symbol.size <= stb.symavl) {
    allocate_refsymbol(stb.symavl);
  }

  /* To avoid duplicate names, because of _parser
   * symnum should be set -2
   */
  if (dbgref_symbol.symnum[local] == -2) {
    dbgref_symbol.symnum[local] = 0;
    return;
  }

  if (dbgref_symbol.altname[local]) {
    if (dbgref_symbol.altname[global] == NULL) {
      dbgref_symbol.altname[global] = dbgref_symbol.altname[local];
    } else {
      symptr = dbgref_symbol.altname[global];
      while (symptr->next) {
        symptr = symptr->next;
      }
      symptr->next = dbgref_symbol.altname[local];
    }
    dbgref_symbol.symnum[local] = -2;
    dbgref_symbol.altname[local] = NULL;
  } else {
    localname = SYMNAME(local);
    symptr = dbgref_symbol.altname[global];
    if (!symptr) {
      /* Don't do anything if name is not changed */
      if (strcmp(SYMNAME(global), localname) == 0) {
        dbgref_symbol.symnum[local] = -2;
        return;
      }
    }
    /* Check if localname is already in altname list */
    while (symptr) {
      if (strcmp(SYMNAME(symptr->sptr), localname) == 0)
        break;
      symptr = symptr->next;
    }
    if (!symptr) {
      symptr = (mod_altptr)malloc(sizeof(module_altname));
      symptr->sptr = local;
      symptr->next = dbgref_symbol.altname[global];
      dbgref_symbol.altname[global] = symptr;
    }
    dbgref_symbol.symnum[local] = -2;
  }
}

void
use_init(void)
{
  usedb.ipasave_avl = 0;
  reinit_refsymbol(stb.symavl);
}

/* initialize for a sequence of USE statements */
void
init_use_stmts(void)
{
  if (usedb.base == NULL) {
    usedb.sz = 32;
    NEW(usedb.base, USED, usedb.sz);
    /* entry 0 is wasted
     * entry 1 is reserved for hpf_library
     * entry 2 is reserved for hpf_local_library
     * entry 3 is reserved for iso_c_binding
     * entry 4 is rewerved for ieee_arithmetic
     * entry 5 is reserved for ieee_features
     * entry 6 is reserved for iso_fortran_env
     * entry 7 is reserved for namelist
     */
    usedb.avl = FIRST_USER_MODULE;
    BZERO(usedb.base, USED, FIRST_USER_MODULE);
  }
}

/* mark module as unrestricted, if no ONLY clause */
void
add_use_stmt(int only)
{
  if (module_id == 0)
    return;
  if (only == 0)
    usedb.base[module_id].unrestricted = TRUE;
}

#define VALID_RENAME_SYM(sptr)                            \
  (sptr > stb.firstusym &&                                \
   (ST_ISVAR(STYPEG(sptr)) || STYPEG(sptr) == ST_ALIAS || \
    STYPEG(sptr) == ST_PROC || STYPEG(sptr) == ST_MODPROC))

/* sptr of local name */
/* sptr of global name */
int
add_use_rename(int local, int global, int is_operator)
{
  RENAME *pr;
  int sptr;
  int sptrloop;
  int original_global = global;

  if (global == 0)
    return 0;
  pr = (RENAME *)getitem(USE_AREA, sizeof(RENAME));
  pr->complete = 0;
  pr->is_operator = is_operator;
  pr->next = usedb.base[module_id].rename;
  usedb.base[module_id].rename = pr;
  /*
   * NOTE: MAY want to skip the ensuing 'if' when the rename is
   * for an OPERATOR (is_operator is set) since an ST_OPERATOR is in
   * its own overloading class!
   */
  if (!VALID_RENAME_SYM(global)) {
    for (sptrloop = first_hash(global); sptrloop;
         sptrloop = HASHLKG(sptrloop)) {
      if (NMPTRG(sptrloop) == NMPTRG(global) && VALID_RENAME_SYM(sptrloop)) {
        if (ST_ISVAR(sptrloop) && SYMLKG(sptrloop) &&
            STYPEG(SYMLKG(sptrloop)) == ST_ALIAS &&
            SCOPEG(SYMLKG(sptrloop)) == usedb.base[module_id].module) {
          global = SYMLKG(sptrloop);
        } else {
          global = sptrloop;
        }
      }
    }
  }

  if (local && STYPEG(local) == ST_ALIAS && PRIVATEG(local) &&
      SCOPEG(local) != curr_scope()->sptr) {
    /* local is a private rename from another module
     * build and use a rename symbol in this scope.
     */
    int newlocal = insert_sym(local);
    DTYPEP(newlocal, DTYPEG(global));
    SCOPEP(newlocal, curr_scope()->sptr);
    pr->local = newlocal;
    HIDDENP(SYMLKG(local), 0);
    pr->global = SYMLKG(local);
    pr->lineno = gbl.lineno;
    return pr->global;
  }
  if (STYPEG(global) == ST_ALIAS && PRIVATEG(global) &&
      SCOPEG(global) != curr_scope()->sptr) {
    /* global is an alias from another scope, generate an alias for the
     * current scope */
    int newglobal = insert_sym(global);
    pr->global = newglobal;
    pr->local = local;
    SCOPEP(newglobal, curr_scope()->sptr);
    ENCLFUNCP(newglobal, SCOPEG(newglobal));
    DTYPEP(newglobal, DTYPEG(global));
    SYMLKP(newglobal, SYMLKG(global));
    HIDDENP(SYMLKG(newglobal), 0);
    pr->lineno = gbl.lineno;
    return pr->global;
  }

  if (!local && global != original_global && seen_contains &&
      STYPEG(original_global) == ST_UNKNOWN) {
    pr->local = original_global;
  } else {
    pr->local = local;
  }
  pr->global = global;
  pr->lineno = gbl.lineno;

  /* Add rename 'use module, abc=>b' */
  if (flg.debug && local && strcmp(SYMNAME(local), SYMNAME(global)) != 0)
    set_modusename(local, global);

  return global;
}

/* Look for other generic or operator symbols that should be added to
 * the 'only' list.
 */
static int
add_only(int listitem, int save_sem_scope_level)
{
  SCOPESTACK *scope;
  int sptr = SYMI_SPTR(listitem);
  int stype = STYPEG(sptr);
  int newglobal, nextnew;
  for (newglobal = HASHLKG(sptr); newglobal; newglobal = nextnew) {
    nextnew = HASHLKG(newglobal);
    if (HIDDENG(newglobal))
      continue;
    if (NMPTRG(newglobal) != NMPTRG(sptr))
      continue;
    switch (STYPEG(newglobal)) {
    case ST_ISOC:
    case ST_CRAY:
      /* predefined symbol, but not active in this routine */
      continue;
    case ST_MEMBER:
      /* can't rename a member name */
      continue;
    default:;
    }
    scope = next_scope_sptr(curr_scope(), SCOPEG(newglobal));
    /* found this in anything just USEd? */
    if (get_scope_level(scope) >= save_sem_scope_level) {
      /* check on 'except' list and private module variable */
      if (!is_except_in_scope(scope, newglobal) && !PRIVATEG(newglobal)) {
        /* look for generic with same name */
        int ng = newglobal;
        while ((STYPEG(ng) == ST_ALIAS || STYPEG(ng) == ST_MODPROC) &&
               SYMLKG(ng) && NMPTRG(SYMLKG(ng)) == NMPTRG(newglobal)) {
          ng = SYMLKG(ng);
        }
        if (STYPEG(ng) == ST_PROC && GSAMEG(ng) &&
            SCOPEG(GSAMEG(ng)) == SCOPEG(newglobal)) {
          /* generic with same name as specific, use the generic */
          newglobal = GSAMEG(ng);
        }
        if (STYPEG(newglobal) == ST_MODPROC && SYMLKG(newglobal)) {
          newglobal = SYMLKG(newglobal);
        }
        if (STYPEG(newglobal) == stype) {
          listitem = add_symitem(newglobal, listitem);
        }
      }
    }
  }
  return listitem;
}

/* We're at the beginning of the statement after a sequence of USE statements.
 * Apply the use statements seen.
 * Clean up after processing the sequence of USE statements.
 */
void
apply_use_stmts(void)
{
  int save_lineno;
  int m_id;

  /*
   * A user error could have occurred which created a situation where
   * sem.pgphase is still PHASE_USE (USE statements have appeared) and the
   * use table is empty.
   */
  if (usedb.base == NULL) {
    usedb.ipasave_avl = 0;
    return;
  }
  save_lineno = gbl.lineno;

  if (!gbl.currmod && gbl.internal <= 1) {
    init_use_tree();
  }
  if (usedb.base[ISO_C_MOD].module) {
    /* use iso_c_binding */
    add_predefined_isoc_module();
    if (sem.interface == 0 && IN_MODULE)
      exportb.iso_c_library = TRUE;
    apply_use(ISO_C_MOD);
  }
  if (usedb.base[IEEE_ARITH_MOD].module) {
    /* use ieee_arithmetic */
    add_predefined_ieeearith_module();
    if (sem.interface == 0 && IN_MODULE)
      exportb.ieee_arith_library = TRUE;
    apply_use(IEEE_ARITH_MOD);
  }
  if (usedb.base[IEEE_FEATURES_MOD].module) {
    /* use ieee_features */
    sem.ieee_features = TRUE;
    if (sem.interface == 0 && IN_MODULE)
      ;
    apply_use(IEEE_FEATURES_MOD);
  }
  if (usedb.base[ISO_FORTRAN_ENV].module) {
    /* use iso_fortran_env */
    add_predefined_iso_fortran_env_module();
    if (sem.interface == 0 && IN_MODULE)
      exportb.iso_fortran_env_library = TRUE;
    apply_use(ISO_FORTRAN_ENV);
  }

  for (m_id = FIRST_USER_MODULE; m_id < usedb.avl; m_id++) {
    apply_use(m_id);
  }
  adjust_symbol_accessibility(usedb.base[module_id].module);

  gbl.lineno = save_lineno;
  if (usedb.base) {
    if (XBIT(89, 2) && usedb.avl > FIRST_USER_MODULE) {
      usedb.ipasave_avl = 0;
      if (usedb.ipasave_modname == NULL) {
        usedb.ipasave_sz = usedb.sz;
        NEW(usedb.ipasave_modname, int, usedb.ipasave_sz);
      } else {
        NEED(usedb.ipasave_avl + usedb.avl, usedb.ipasave_modname, int,
             usedb.ipasave_sz, usedb.ipasave_sz + usedb.avl + 10);
      }
      for (module_id = FIRST_USER_MODULE; module_id < usedb.avl; ++module_id) {
        if (usedb.base[module_id].module) {
          usedb.ipasave_modname[usedb.ipasave_avl++] =
              usedb.base[module_id].module;
        }
      }
    }
    if (usedb.base[1].modent)
      FREE(usedb.base[1].modent);
    if (usedb.base[2].modent)
      FREE(usedb.base[2].modent);
    FREE(usedb.base);
    usedb.base = NULL;
    usedb.sz = usedb.avl = 0;
  }

  freearea(USE_AREA);
}

static int
find_def_in_most_recent_scope(int sptr, int save_sem_scope_level)
{
  int sptr1;
  SCOPESTACK *scope;

  for (sptr1 = first_hash(sptr); sptr1; sptr1 = HASHLKG(sptr1)) {
    if (NMPTRG(sptr1) != NMPTRG(sptr))
      continue;
    if (STYPEG(sptr1) == ST_ALIAS && aliased_sym_visible(sptr1)) {
      PRIVATEP(sptr1, 0);
      HIDDENP(SYMLKG(sptr1), 0);
    }
    if (STYPEG(sptr1) == ST_ALIAS) {
      if (PRIVATEG(sptr1))
        continue;
    } else if (HIDDENG(sptr1)) {
      continue;
    }

    switch (STYPEG(sptr1)) {
    case ST_ISOC:
    case ST_IEEEARITH:
    case ST_CRAY:
      /* predefined symbol, but not active in this routine */
      continue;
    case ST_MEMBER:
      /* can't rename a member name */
      continue;
    default:;
    }

    scope = curr_scope();
    while ((scope = next_scope_sptr(scope, SCOPEG(sptr1))) != 0) {
      int ng;
      int scopelevel = get_scope_level(scope);
      if (scopelevel < save_sem_scope_level) {
        break;
      }
      /* FS#14884  If sptr1 is ST_ALIAS then the PRIVATE
       * flag is not valid.  Look at the PRIVATE flag of the
       * symbol the alias points to.
       */
      ng = sptr1;
      while (STYPEG(ng) == ST_ALIAS && SYMLKG(ng) &&
             NMPTRG(SYMLKG(ng)) == NMPTRG(sptr)) {
        ng = SYMLKG(ng);
      }
      /* is the symbol visible in this scope: i.e. not on except list or
          in private USE or a private module variable */
      if (!is_except_in_scope(scope, sptr1) &&
          !is_private_in_scope(scope, sptr1) &&
          (STYPEG(ng) == ST_USERGENERIC || !PRIVATEG(ng))) {
        return sptr1;
      }
    }
  }
  return NOSYM;
}

static void
apply_use(int m_id)
{
  int save_sem_scope_level, exceptlist, onlylist;
  RENAME *pr;
  int ex;

  module_id = m_id;
  use_file_name = usedb.base[module_id].fullname;
  if (DBGBIT(0, 0x10000))
    fprintf(gbl.dbgfil, "Open module file: %s\n", use_file_name);
  use_fd = fopen(use_file_name, "r");
  /* -M option:  Print list of include files to stdout */
  /* -MD option:  Print list of include files to file <program>.d */
  if (sem.which_pass == 0 && ((XBIT(123, 2) || XBIT(123, 8)))) {
    if (gbl.dependfil == NULL) {
      if ((gbl.dependfil = tmpf("a")) == NULL)
        errfatal(5);
    } else
      fprintf(gbl.dependfil, "\\\n  ");
    if (!XBIT(123, 0x40000))
      fprintf(gbl.dependfil, "%s ", use_file_name);
    else
      fprintf(gbl.dependfil, "\"%s\" ", use_file_name);
  }
  if (use_fd == NULL) {
    set_exitcode(19);
    if (XBIT(0, 0x20000000))
      erremit(0);
    error(4, 0, gbl.lineno, "Unable to open MODULE file",
          SYMNAME(usedb.base[module_id].module));
    return;
  }
  /* save this so we can tell what new symbols were added below */
  save_sem_scope_level = sem.scope_level;
  read_module(FALSE, &(usedb.base[module_id].module), save_sem_scope_level);

  if ((seen_contains && sem.mod_cnt) || gbl.internal > 1 || sem.interface) {
    /*
       adjust symbol visibility if module has renames and processing a (module
       or subroutine)
       contained subroutine or a subroutine interface
    */
    adjust_symbol_accessibility(usedb.base[module_id].module);
  }

  exceptlist = 0;
  onlylist = 0;
  for (pr = usedb.base[module_id].rename; pr != NULL; pr = pr->next) {
    int newglobal, ng, oldglobal = pr->global, oldlocal = pr->local;
    char *name = SYMNAME(pr->global);
    int wrksptr;

    if (pr->complete) {
      /* already found as an iso_c intrinsic */
      continue;
    }

    newglobal = NOSYM;
    newglobal = find_def_in_most_recent_scope(pr->global, save_sem_scope_level);

    if (newglobal != NOSYM) {
      /* look for generic with same name */
      ng = newglobal;
      while ((STYPEG(ng) == ST_ALIAS || STYPEG(ng) == ST_MODPROC) &&
             SYMLKG(ng) && NMPTRG(SYMLKG(ng)) == NMPTRG(newglobal)) {
        ng = SYMLKG(ng);
      }
      if (STYPEG(ng) == ST_PROC && GSAMEG(ng) &&
          SCOPEG(GSAMEG(ng)) == SCOPEG(newglobal)) {
        /* generic with same name as specific, use the generic */
        newglobal = GSAMEG(ng);
      }
    }

    if (newglobal == NOSYM || newglobal < stb.firstosym ||
        STYPEG(newglobal) == ST_UNKNOWN) {
      if (!sem.which_pass)
        continue;
      error(84, 3, pr->lineno, name, "- not public entity of module");
      IGNOREP(newglobal, 1);
      continue;
    }

    if (newglobal != oldglobal && STYPEG(oldglobal) == ST_UNKNOWN) {
      /* ignore the fake symbol added by the 'use' clause */
      if (pr->local) {
        IGNOREP(oldglobal, 1);
        HIDDENP(oldglobal, 1);
      } else {
        pr->local = oldglobal;
      }
    }
    if (STYPEG(newglobal) == ST_MODPROC && SYMLKG(newglobal))
      newglobal = SYMLKG(newglobal);
    pr->global = newglobal;
    gbl.lineno = pr->lineno;
    if (!pr->local) {
      pr->local = insert_sym(pr->global);
    } else if (STYPEG(pr->local) != ST_UNKNOWN) {
      pr->local = insert_sym(pr->local);
    }
    SCOPEP(pr->local, stb.curr_scope);
    IGNOREP(pr->local, 0);
    if (!oldlocal)
      DCLDP(pr->local, 1); /* declared, not renamed */
    if (STYPEG(ng /*pr->global*/) == ST_OPERATOR) {
      STYPEP(pr->local, ST_OPERATOR);
      INKINDP(pr->local, INKINDG(pr->global));
      PDNUMP(pr->local, PDNUMG(pr->global));
      copy_specifics(ng, pr->local);
    } else if (STYPEG(ng /*pr->global*/) == ST_USERGENERIC) {
      if (NMPTRG(pr->local) == NMPTRG(pr->global)) {
        STYPEP(pr->local, ST_ALIAS);
        SYMLKP(pr->local, pr->global);
      } else {
        STYPEP(pr->local, ST_USERGENERIC);
        copy_specifics(ng, pr->local);
        IGNOREP(SYMLKG(pr->global), 1);
      }
    } else {
      STYPEP(pr->local, ST_ALIAS);
      if (STYPEG(pr->global) == ST_ALIAS) {
        SYMLKP(pr->local, SYMLKG(pr->global));
        IGNOREP(pr->global, 1);
      } else {
        SYMLKP(pr->local, pr->global);
      }
    }
    if (usedb.base[module_id].unrestricted) {
      /* add the original module symbol to its except list */
      exceptlist = add_symitem(pr->global, exceptlist);
    } else {
      onlylist = add_symitem(pr->global, onlylist);
    }
  }
  if (usedb.base[module_id].unrestricted) {
    /* add this stuff to the exception list */
    int nexte, e;
    for (e = exceptlist; e; e = nexte) {
      int sptr, scopelevel;
      SCOPESTACK *scope;
      nexte = SYMI_NEXT(e);
      sptr = SYMI_SPTR(e);
      scope = next_scope_sptr(curr_scope(), SCOPEG(sptr));
      if (get_scope_level(scope) >= save_sem_scope_level) {
        SYMI_NEXT(e) = scope->except;
        scope->except = e;
        if (STYPEG(sptr) == ST_ALIAS && STYPEG(SYMLKG(sptr)) == ST_PROC) {
          /* hide original alias for a renamed subprogram */
          int s;
          PRIVATEP(sptr, 1); /* hide original alias for a renamed subprogram */
          HIDDENP(SYMLKG(sptr), 1); /* hide subprogram itself,
                                         doesn't seem to be necessary */
          for (s = first_hash(sptr); s; s = HASHLKG(s)) {
            if (STYPEG(s) == ST_MODPROC && SYMLKG(s) == sptr) {
              HIDDENP(s, 1); /* hide any associated ST_MODPROC */
              break;
            }
          }
        }
      }
    }
    update_use_tree_exceptions();
  } else {
    /* the SCOPE_USE will be pushed at the scope
     * level of the old SCOPE_NORMAL */
    SCOPESTACK *scope = curr_scope();
    while ((scope = next_scope(scope)) != 0 &&
           get_scope_level(scope) >= save_sem_scope_level) {
      int o, nexto;
      scope->private = TRUE;
      for (o = onlylist; o; o = nexto) {
        nexto = SYMI_NEXT(o);
        if (SCOPEG(SYMI_SPTR(o)) == scope->sptr) {
          SYMI_NEXT(o) = scope->only;
          scope->only = add_only(o, save_sem_scope_level);
        }
      }
    }
  }
  fclose(use_fd);
}

static void
add_predefined_module(int mod_id, int stype)
{
  int i;
  int size;
  int sptr;
  RENAME *pr;

  if (usedb.base[mod_id].unrestricted) { /* do all */
    size = usedb.base[mod_id].size;
    for (i = 0; i < size; i++) {
      sptr = usedb.base[mod_id].modent[i].sptr;
      if (STYPEG(sptr) == stype)
        STYPEP(sptr, ST_PD);
    }
  }

  for (pr = usedb.base[mod_id].rename; pr != NULL; pr = pr->next) {
    sptr = pr->global;
    pr->global = get_entry(mod_id, SYMNAME(pr->global));
    if (pr->global == 0) {
      error(84, 3, pr->lineno, SYMNAME(sptr), "- not public entity of module");
      continue;
    }
    if (pr->local) {
      gbl.lineno = pr->lineno;
      pr->local = declsym(pr->local, ST_ALIAS, TRUE);
      SYMLKP(pr->local, pr->global);
    }
    /* Hide the symbol created when the ST_HL or ST_HLL is lex'd. */
    if (STYPEG(sptr) != stype)
      HIDDENP(sptr, 1);
  }
}

/* predefined  processing for the iso_c module only */
static void
add_predefined_isoc_module(void)
{
  int i;
  int size;
  int sptr;
  RENAME *pr;
  int found;

  if (usedb.base[ISO_C_MOD].unrestricted) { /* do all */
    size = usedb.base[ISO_C_MOD].size;
    for (i = 0; i < size; i++) {
      sptr = usedb.base[ISO_C_MOD].modent[i].sptr;
      if (strcmp(MODNAME(ISO_C_MOD, i), "c_sizeof") == 0) {
        STYPEP(sptr, ST_PD);
      } else {
        STYPEP(sptr, ST_INTRIN);
      }
    }
  }

  for (pr = usedb.base[ISO_C_MOD].rename; pr != NULL; pr = pr->next) {
    sptr = pr->global;
    found = get_entry(ISO_C_MOD, SYMNAME(pr->global));
    if (found) {
      pr->global = found;
      pr->complete = 1;
      if (pr->local) {
        gbl.lineno = pr->lineno;
        pr->local = declsym(pr->local, ST_ALIAS, TRUE);
        SYMLKP(pr->local, pr->global);
      }
      /* Hide the symbol created when the  ST_ISOC  is lex'd.
       * NOTE that get_entry() changes ST_ISOC to ST_INTRIN
       */
      /* c_sizeof is the only symbol in the ISO_C_MOD that is a
       * ST_PD (predefined) so it must be handled explicitly.
       */
      if ((STYPEG(found) == ST_INTRIN ||
           (STYPEG(found) == ST_PD &&
            strcmp(SYMNAME(pr->global), "c_sizeof") == 0)) &&
          sptr != found && STYPEG(sptr) == ST_UNKNOWN) {
        pop_sym(sptr);
        IGNOREP(sptr, 1); /* and do not send to .mod file */
      }
    }
  }
}

/* predefined  processing for the iso_fortran_env module only */
static void
add_predefined_iso_fortran_env_module(void)
{
  int i;
  int size;
  int sptr;
  RENAME *pr;
  int found;

  if (usedb.base[ISO_FORTRAN_ENV].unrestricted) { /* do all */
    size = usedb.base[ISO_FORTRAN_ENV].size;
    for (i = 0; i < size; i++) {
      sptr = usedb.base[ISO_FORTRAN_ENV].modent[i].sptr;
      if (STYPEG(sptr) == ST_ISOFTNENV)
        STYPEP(sptr, ST_PD);
    }
  }

  for (pr = usedb.base[ISO_FORTRAN_ENV].rename; pr != NULL; pr = pr->next) {
    sptr = pr->global;
    found = get_entry(ISO_FORTRAN_ENV, SYMNAME(pr->global));
    if (found) {
      pr->global = found;
      pr->complete = 1;
      if (pr->local) {
        gbl.lineno = pr->lineno;
        pr->local = declsym(pr->local, ST_ALIAS, TRUE);
        SYMLKP(pr->local, pr->global);
      }
      /* Hide the symbol created when the  ST_ISOFTNEV  is lex'd.
       * NOTE that get_entry() changes ST_ISOFTNEV to ST_PD
       */
      if (STYPEG(found) == ST_PD && sptr != found &&
          STYPEG(sptr) == ST_UNKNOWN) {
        pop_sym(sptr);
        IGNOREP(sptr, 1); /* and do not send to .mod file */
      }
    }
  }
}

void
add_isoc_intrinsics(void)
{
  int first, last, size;
  int i;
  int sptr;

  iso_c_lib_stat(&first, &last, ST_ISOC);
  size = last - first + 1;
  for (i = 0; i < size; i++) {
    sptr = first++;
    if (STYPEG(sptr) == ST_ISOC) {
      STYPEP(sptr, ST_INTRIN);
    }
  }
}

static void
add_predefined_ieeearith_module(void)
{
  int i;
  int sptr;
  RENAME *pr;
  int found;

  found = 0;
  if (usedb.base[IEEE_ARITH_MOD].unrestricted) { /* do all */
    found = get_ieee_arith_intrin("ieee_selected_real_kind");
  }
  for (pr = usedb.base[IEEE_ARITH_MOD].rename; pr != NULL; pr = pr->next) {
    sptr = pr->global;
    if (strcmp(SYMNAME(sptr), "ieee_selected_real_kind") == 0) {
      found = get_ieee_arith_intrin("ieee_selected_real_kind");
#if DEBUG
      assert(found, "ieee_arithmetic routine not found", sptr, 3);
#endif
      pr->global = found;
      pr->complete = 1;
      if (pr->local) {
        gbl.lineno = pr->lineno;
        pr->local = declsym(pr->local, ST_ALIAS, TRUE);
        SYMLKP(pr->local, pr->global);
      }
      /* Hide the symbol created when the  ST_IEEEARITH  is lex'd.
       */
      pop_sym(sptr);
      IGNOREP(sptr, 1); /* and do not send to .mod file */
    }
  }
  if (found) {
    STYPEP(found, ST_PD);
    SCOPEP(found, 0);
  }
}

/* 'use' - sym ptr of module identifer in use statement */
int
open_module(int use)
{
  int i;
  char *name;
  char *fullname;
  char *p;
  int first, last;
  int c_ptr, loc_sptr;

  if (STYPEG(use) != ST_MODULE && STYPEG(use) != ST_UNKNOWN &&
      SCG(use) != SC_NONE) {
    /* a variable of this name had been declared, perhaps in an enclosing
     * subprogram */
    int sptr;
    NEWSYM(sptr);
    NMPTRP(sptr, NMPTRG(use));
    SYMLKP(sptr, NOSYM);
    use = sptr;
  }
  name = SYMNAME(use);
  module_id = 0;

  for (module_id = ALL_MODULES; module_id < usedb.avl; module_id++)
    if (strcmp(SYMNAME(usedb.base[module_id].module), name) == 0)
      return module_id;

#define MAX_FNAME_LEN 2050

  use_fd = NULL;
  fullname = getitem(8, MAX_FNAME_LEN + 1);
  modu_file_name = getitem(8, strlen(name) + strlen(MOD_SUFFIX) + 1);
  strcpy(modu_file_name, name);
  strcat(modu_file_name, MOD_SUFFIX);
  if (!get_module_file_name(modu_file_name, fullname, MAX_FNAME_LEN)) {
    set_exitcode(19);
    if (XBIT(0, 0x20000000))
      erremit(0);
    error(4, 0, gbl.lineno, "Unable to open MODULE file", modu_file_name);
    return 0;
  }
  if (use < stb.firstusym) {
    /* if module has the same name as some predefined thing */
    use = insert_sym(use);
  }
  if (strcmp(name, "iso_c_binding") == 0) {
    module_id = ISO_C_MOD;
  } else if (strcmp(name, "ieee_arithmetic") == 0) {
    module_id = IEEE_ARITH_MOD;
  } else if (strcmp(name, "ieee_arithmetic_la") == 0) {
    module_id = IEEE_ARITH_MOD;
  } else if (strcmp(name, "ieee_features") == 0) {
    module_id = IEEE_FEATURES_MOD;
  } else if (strcmp(name, "iso_fortran_env") == 0) {
    module_id = ISO_FORTRAN_ENV;
  } else {
    module_id = usedb.avl++;
  }
  NEED(usedb.avl, usedb.base, USED, usedb.sz, usedb.sz + 8);
  usedb.base[module_id].module = use;
  usedb.base[module_id].unrestricted = FALSE;
  usedb.base[module_id].rename = NULL;
  usedb.base[module_id].fullname = fullname;

  if (module_id == ISO_C_MOD) {
    /* add the predefined intrinsic functions c_loc, etc */
    iso_c_lib_stat(&first, &last, ST_ISOC);
    usedb.base[module_id].size =
        last - first + 2; /* + 2 becasuse C_size is a PD */
    NEW(usedb.base[module_id].modent, MODENT, usedb.base[module_id].size);
    for (i = 0; i < usedb.base[module_id].size - 1; i++) {
      usedb.base[module_id].modent[i].nmptr = NMPTRG(first);
      usedb.base[module_id].modent[i].sptr = first++;
    }
    /* C_sizeof is from F2008 and is a  PD rather than a ST_ISOC */
    first = lookupsymbol("c_sizeof");
    usedb.base[module_id].modent[i].nmptr = NMPTRG(first);
    usedb.base[module_id].modent[i].sptr = first;
  }
  if (module_id == ISO_FORTRAN_ENV) {
    if (usedb.base[module_id].size)
      return module_id;

    usedb.base[module_id].size = 2;
    NEW(usedb.base[module_id].modent, MODENT, usedb.base[module_id].size);

    first = lookupsymbol("compiler_options");
    usedb.base[module_id].modent[0].nmptr = NMPTRG(first);
    usedb.base[module_id].modent[0].sptr = first;

    first = lookupsymbol("compiler_version");
    usedb.base[module_id].modent[1].nmptr = NMPTRG(first);
    usedb.base[module_id].modent[1].sptr = first;
  }
  /*
   * at this point, there is not similar processing for IEEE_ARITH_MOD
   * as ISO_C_MOD.  Only one ieee_arithmetic routine actually needs to
   * be represented as an intrinsic/predeclared.  That routine is
   * ieee_selected_real_kind; so, there is no need to have a sequence
   * of  'modent' entries for the ieee_arithmetic module.
   */
  return module_id;
}

static int
get_entry(int mod_id, char *name)
{
  int i;
  int sptr;
  int size;

  size = usedb.base[mod_id].size;
  for (i = 0; i < size; i++)
    if (strcmp(name, MODNAME(mod_id, i)) == 0) {
      if (mod_id == ISO_C_MOD) {
        sptr = usedb.base[mod_id].modent[i].sptr;
        if (STYPEG(sptr) == ST_ISOC) {
          if (strcmp(MODNAME(mod_id, i), "c_sizeof") == 0) {
            STYPEP(sptr, ST_PD);
          } else {
            STYPEP(sptr, ST_INTRIN);
          }
        }
        return sptr;
      }
      if (mod_id == ISO_FORTRAN_ENV) {
        sptr = usedb.base[mod_id].modent[i].sptr;
        if (STYPEG(sptr) == ST_ISOFTNENV)
          STYPEP(sptr, ST_PD);
        return sptr;
      }
      sptr = getsymbol(MODNAME(mod_id, i));
    }

  return 0;
}

void
close_module(void)
{
}

/* ------------------------------------------------------------------ */
/*   MODULE & CONTAINS statements - create module file */

static int modu_sym = 0;
static FILE *outfile;
static FILE *single_outfile = NULL;
static char *single_outfile_name = NULL;
static char *single_outfile_index_name = NULL;
static char modu_name[MAXIDLEN + 1];
static int mod_lineno;

#ifdef HOST_WIN
#define long_t long long
#define LLF "%lld"
#else
#define long_t long
#define LLF "%ld"
#endif
typedef struct mod_index {
  struct mod_index *next;
  char *module_name;
  long_t offset;
} mod_index;
static mod_index *mod_index_list = NULL;

typedef struct {
  int firstc; /* first character in range */
  int lastc;  /* last character in range */
  int dtype;  /* implicit dtype pointer: 0 => NONE */
} IMPL;

static struct {
  IMPL *base;
  int avl;
  int sz;
} impl;

/*
 * save the name to use for the combined .mod file
 */
void
mod_combined_name(char *name)
{
  single_outfile_name = name;
} /* mod_combined_name */

/*
 * save the name to use for the combined module index file
 */
void
mod_combined_index(char *name)
{
  single_outfile_index_name = name;
} /* mod_combined_index */

int
begin_module(int sptr)
{

  modu_sym = declsym((int)sptr, ST_MODULE, TRUE);
  DCLDP(modu_sym, 1);
  FUNCLINEP(modu_sym, gbl.lineno);
  mod_lineno = gbl.lineno;
  seen_contains = FALSE;
  strcpy(modu_name, SYMNAME(modu_sym));
  outfile = NULL;  /* only create if error free */
  gbl.currsub = 0; /* ==> module */
  impl.sz = 16;
  NEW(impl.base, IMPL, impl.sz);
  impl.avl = 0;
  sem.mod_dllexport = FALSE;
  init_use_tree();

  return modu_sym;
}

LOGICAL
get_seen_contains(void)
{
  return seen_contains;
}

/* first character in range */
/* last character in range */
/* implicit dtype pointer: 0 => NONE */
void
mod_implicit(int firstc, int lastc, int dtype)
{
  int i;

  i = impl.avl++;
  NEED(impl.avl, impl.base, IMPL, impl.sz, impl.sz + 16);
  impl.base[i].firstc = firstc;
  impl.base[i].lastc = lastc;
  impl.base[i].dtype = dtype;

}

static void
handle_mod_syms_dllexport(void)
{
  int sptr;

  if (!sem.mod_dllexport) {
    return;
  }

  for (sptr = stb.firstusym; sptr < stb.symavl; ++sptr) {
    switch (STYPEG(sptr)) {
    case ST_MODULE:
      if (sptr == gbl.currmod) {
        DLLP(sptr, DLL_EXPORT);
      }
      break;
    case ST_ENTRY:
      if (ENCLFUNCG(sptr) == gbl.currmod) {
        DLLP(sptr, DLL_EXPORT);
      }
      break;
    case ST_PROC:
      if (ENCLFUNCG(sptr) == gbl.currmod && INMODULEG(sptr)) {
        DLLP(sptr, DLL_EXPORT);
      }
      break;
    case ST_VAR:
    case ST_ARRAY:
      if (SCG(sptr) == SC_CMBLK && SCOPEG(sptr) == gbl.currmod &&
          HCCSYMG(CMBLKG(sptr))) {
        DLLP(sptr, DLL_EXPORT);
        break;
      }
      break;
    default:;
    }
  }
}

void
begin_contains(void)
{
  int len;

  if (seen_contains) {
    errsev(70);
    return;
  }
  seen_contains = TRUE;
  sem.mod_cnt = 2; /* ensure semfin() preforms all of its processing  */
  save_module_state1();
  fix_module_common();
  handle_mod_syms_dllexport();

  save_module_state2();
  save_implicit(FALSE);
  sem.mod_cnt = 1;
}

void
end_module(void)
{
  int sptr;

  if (!seen_contains) {
    sem.mod_cnt = 2;
    if (sem.accl.type == 'v') {
      /* default is private */
      sem.mod_public_flag = 0;
    } else {
      sem.mod_public_flag = 1;
    }
  }
  if (sem.mod_cnt == 2)
    FREE(impl.base);
  if (modu_sym == 0) {
    if (outfile != NULL && sem.mod_cnt == 2) {
      fclose(outfile);
      outfile = NULL;
    }
    goto exit;
  }
  export_public_used_modules(sem.scope_level);

  if (!seen_contains) {
    fix_module_common();
    handle_mod_syms_dllexport();
  }

  /* When use-associated, the ST_MODULE is turned into a ST_PROC. So,
   * NEEDMOD distinguishes between an ST_PROC created from a ST_MODULE
   * vs a real procedure.  When NEEDMOD is set, Fortran backend will not put
   * the ST_PROC in the 'ureferenced external' category.
   */
  NEEDMODP(modu_sym, 1);
  if (astb.df != NULL || dinit_ftell() > 0) {
    /*
     * Older versions of the compiler unconditionally set NEEDMOD.  The new
     * behavior of the backend is to generate a hard reference to the
     * global module name if NEEDMOD is set.  Need a method to distinguish
     * between the old and new interpretations of NEEDMOD.  The older
     * compilers never set the TYPD flag for ST_MODULEs!
     */
    TYPDP(modu_sym, 1);
  }

  export_all();
  if (seen_contains)
    gbl.currsub = 0;

  if (outfile != NULL && sem.mod_cnt == 2) {
    fclose(outfile);
    outfile = NULL;
  }
  if (sem.which_pass == 0 && ((XBIT(123, 2) || XBIT(123, 8)))) {
    if (gbl.moddependfil == NULL) {
      if ((gbl.moddependfil = tmpf("a")) == NULL)
        errfatal(5);
    }
    if (!XBIT(123, 0x40000)) {
      fprintf(gbl.moddependfil, "%s%s : ", modu_name, MOD_SUFFIX);
      fprintf(gbl.moddependfil, "%s\n", gbl.src_file);
    } else {
      fprintf(gbl.moddependfil, "\"%s%s\" : ", modu_name, MOD_SUFFIX);
      fprintf(gbl.moddependfil, "\"%s\"\n", gbl.src_file);
    }
  }
  modu_sym = 0;
  exportb.hpf_library = FALSE;
  exportb.hpf_local_library = FALSE;
  exportb.iso_c_library = FALSE;
  exportb.iso_fortran_env_library = FALSE;
  exportb.ieee_arith_library = FALSE;

  /* check for undefined module subprograms */
  for (sptr = stb.firstusym; sptr < stb.symavl; ++sptr) {
    if (!IGNOREG(sptr) && STYPEG(sptr) == ST_MODPROC && SYMLKG(sptr) == 0) {
      error(155, 2, gbl.lineno, "MODULE PROCEDURE not defined:", SYMNAME(sptr));
    }
  }

exit:
  init_use_tree();
}

/* ------------------------------------------------------------------ */
/*   Write .mod file  */

/*  getitem area for module temp storage; pick an area not used by
 *  semant.
 */

static int
make_module_common(int idx, int private, int threadprivate, int device,
                   int isconstant, int iscopyin, int islink)
{
  static char sfx[3];
  char modcm_name[MAXIDLEN + 2];
  int modcm;
  if (idx <= 9) {
    sfx[0] = '0' + idx;
    sfx[1] = 0;
  } else if (idx <= 19) {
    sfx[0] = '1';
    sfx[1] = '0' + (idx - 10);
    sfx[2] = 0;
  } else {
    sfx[0] = '2';
    sfx[1] = '0' + (idx - 20);
    sfx[2] = 0;
  }
  if (!XBIT(58, 0x80000)) {
    modcm_name[0] = '_';
    strcpy(modcm_name + 1, modu_name);
  } else {
    strcpy(modcm_name, modu_name);
  }
  modcm = get_next_sym(modcm_name, sfx);
  STYPEP(modcm, ST_CMBLK);
  SIZEP(modcm, 0);
  SYMLKP(modcm, gbl.cmblks);
  MODCMNP(modcm, 1);
  gbl.cmblks = modcm;
  PRIVATEP(modcm, private);
  THREADP(modcm, threadprivate);
#ifdef DEVICEP
  if (device)
    DEVICEP(modcm, 1);
  if (isconstant) {
    CONSTANTP(modcm, 1);
  } else if (islink) {
    ACCLINKP(modcm, 1);
  } else if (iscopyin) {
    ACCCOPYINP(modcm, 1);
  }
#endif
  CMEMFP(modcm, NOSYM);
  CMEMLP(modcm, NOSYM);
  if (flg.sequence)
    SEQP(modcm, 1);
  if (sem.mod_dllexport) {
    DLLP(modcm, DLL_EXPORT);
  }
  return modcm;
} /* make_module_common */

/* add a padding symbol with numeric or char type here */
static int
add_padding(int sptr, int dtype, ISZ_T padsize, int cmidx)
{
  int newdtype, i, padding;
  /* make a dummy symbol */
  padding = get_next_sym(SYMNAME(sptr), "pad");
  if (DTY(dtype) == TY_CHAR || DTY(dtype) == TY_NCHAR) {
    newdtype = get_type(2, DTY(dtype), mk_cval(padsize, DT_INT4));
    STYPEP(padding, ST_VAR);
  } else {
    newdtype = get_array_dtype(1, dtype);
    ADD_LWAST(newdtype, 0) = ADD_LWBD(newdtype, 0) = mk_cval(1, DT_INT);
    ADD_UPAST(newdtype, 0) = ADD_UPBD(newdtype, 0) = ADD_EXTNTAST(newdtype, 0) =
        mk_cval(padsize, DT_INT);
    ADD_NUMELM(newdtype) = mk_cval(padsize, DT_INT);
    STYPEP(padding, ST_ARRAY);
  }
  SCP(padding, SC_LOCAL);
  DTYPEP(padding, newdtype);
  DCLDP(padding, 1);
  SEQP(padding, 1);
#ifdef DEVICEG
  DEVICEP(padding, DEVICEG(sptr));
  MANAGEDP(padding, MANAGEDG(sptr));
  ACCCREATEP(padding, ACCCREATEG(sptr));
  ACCCOPYINP(padding, ACCCOPYING(sptr));
  ACCLINKP(padding, ACCLINKG(sptr));
  CONSTANTP(padding, CONSTANTG(sptr));
#endif
  add_to_common(cmidx, padding, 0);
  return padding;
} /* add_padding */

#ifdef DEVICEG
/*
 * if this symbol is in an equivalence statement,
 * propagate the DEVICEG, MANAGEDG, ACCCREATEG, ACCCOPYING, ACCRESIDENTG,
 * ACCLINKG,
 * and CONSTANTG flags from this symbol to any symbols in its overlap list,
 * and from any symbol in the overlap list to this symbol.
 */
static int
propagate_device_flags(int sptr)
{
  if (SOCPTRG(sptr)) {
    int dev = DEVICEG(sptr);
    int managed = MANAGEDG(sptr);
    int acccreate = ACCCREATEG(sptr);
    int acccopyin = ACCCOPYING(sptr);
    int acclink = ACCLINKG(sptr);
    int cnstant = CONSTANTG(sptr);
    int p;
    for (p = SOCPTRG(sptr); p; p = SOC_NEXT(p)) {
      int ovsptr = SOC_SPTR(p);
      if (DEVICEG(ovsptr))
        dev = 1;
      if (MANAGEDG(ovsptr))
        managed = 1;
      if (ACCCREATEG(ovsptr))
        acccreate = 1;
      if (ACCCOPYING(ovsptr))
        acccopyin = 1;
      if (ACCLINKG(ovsptr))
        acclink = 1;
      if (CONSTANTG(ovsptr))
        cnstant = 1;
    }
    DEVICEP(sptr, dev);
    MANAGEDP(sptr, managed);
    ACCCREATEP(sptr, acccreate);
    ACCCOPYINP(sptr, acccopyin);
    ACCLINKP(sptr, acclink);
    CONSTANTP(sptr, cnstant);
    for (p = SOCPTRG(sptr); p; p = SOC_NEXT(p)) {
      int ovsptr = SOC_SPTR(p);
      DEVICEP(ovsptr, dev);
      MANAGEDP(ovsptr, managed);
      ACCCREATEP(ovsptr, acccreate);
      ACCCOPYINP(ovsptr, acccopyin);
      ACCLINKP(ovsptr, acclink);
      CONSTANTP(ovsptr, cnstant);
    }
  }
  return FALSE;
} /* propagate_device_flags */
#endif

/*
 * module common combinations:
 *
 * not initd: pub-nonchar,  pub-char,  pub-long,  pub_threadprivate,
 *            priv-nonchar, priv-char, priv-long, priv_threadprivate,
 * initd    : pub-nonchar,  pub-char,  pub-long,  pub_threadprivate,
 *            priv-nonchar, priv-char, priv-long, priv_threadprivate,
 * device   : device, constant, copyin, link,
 *            threadprivate: device, constant, copyin, link
 * dev-initd: device, constant, device-threadprivate, constant-threadprivate
 * openacc create/resident data is treated like device data
 */
static int mod_cmn[32];
#define FIRST_DEV_COMMON 16
#define LAST_DEV_COMMON 28
static int
MOD_CMN_IDX(int xpriv, int xchar, int xlong, int xinitd, int thrd_priv,
            int xdev, int xconst, int xcopyin, int xlink)
{
  if ((xdev + xconst + xcopyin + xlink) == 0) {
    if (thrd_priv) /* don't separate int/char/long */
      return 4 * xpriv + 8 * xinitd + 3;
    return 4 * xpriv + xchar + 2 * xlong + 8 * xinitd;
  }
  if (xconst)
    return 16 + 1 + 2 * thrd_priv + 8 * xinitd;
  if (xlink)
    return 16 + 6 + 2 * thrd_priv;
  if (xcopyin)
    return 16 + 5 + 2 * thrd_priv;
  return 16 + 2 * thrd_priv + 8 * xinitd;
}

#define N_MOD_CMN sizeof(mod_cmn) / sizeof(int)
static int mod_cmn_naln[N_MOD_CMN];

typedef struct itemx {/* generic item record */
  int val;
  struct itemx *next;
} ITEMX;
static ITEMX *mdalloc_list;
static ITEMX *pointer_list;

static void
check_sc(int sptr)
{
  ITEMX *px;
  int dty;
  int ty, tysize;
  int acc;    /* access type: 0 = PUBLIC, 1 = PRIVATE */
  int chr;    /* 0 => non-character; 1 => character */
  int islong; /* 0 => not long; 1 => long */
  int initd;  /* 0 => not initd;  1 => initd */
  int idx, dev, con, link, cpyin;
  int modcm;

  if (IGNOREG(sptr))
    return;
  switch (SCG(sptr)) {
  case SC_BASED:
  case SC_DUMMY:
    dty = DTYG(DTYPEG(sptr));
    if (XBIT(58, 0x10000) ||
        (dty != TY_DERIVED && dty != TY_CHAR && dty != TY_NCHAR)) {
      if (POINTERG(sptr) && !F90POINTERG(sptr) && MIDNUMG(sptr) &&
          SCG(MIDNUMG(sptr)) != SC_CMBLK) {
        /* process pointer variables later; a pointer variable's
         * associated variables need to placed in its own common
         * block.  Can't process here since they would be added
         * to the module's common block.
         */
        px = (ITEMX *)getitem(0, sizeof(ITEMX));
        px->val = sptr;
        px->next = pointer_list;
        pointer_list = px;
        /*
         * Give the pointer attribute precedence over module
         * allocatable.
         */
        MDALLOCP(sptr, 0);
      }
    }
    if (ALLOCATTRG(sptr)) {
      /* process module allocatable arrays later; a variable's
       * associated variables need to placed in its own common
       * block.  Can't process here since they would be added
       * to the module's common block.
       */
      px = (ITEMX *)getitem(0, sizeof(ITEMX));
      px->val = sptr;
      px->next = mdalloc_list;
      mdalloc_list = px;
      break;
    }
  case SC_CMBLK:
    MDALLOCP(sptr, 0);
    break;
  case SC_NONE:
    /* see if we should handle these pointer vars or pass them through */
    dty = DTYG(DTYPEG(sptr));
    if (XBIT(58, 0x10000) ||
        (dty != TY_DERIVED && dty != TY_CHAR && dty != TY_NCHAR)) {
      if (POINTERG(sptr) && !F90POINTERG(sptr)) {
        /* process pointer variables later; a pointer variable's
         * associated variables need to placed in its own common
         * block.  Can't process here since they would be added
         * to the module's common block.
         */
        px = (ITEMX *)getitem(0, sizeof(ITEMX));
        px->val = sptr;
        px->next = pointer_list;
        pointer_list = px;
        /*
         * Give the pointer attribute precedence over module
         * allocatable.
         */
        MDALLOCP(sptr, 0);
        break;
      }
      if (ALLOCG(sptr) && !F90POINTERG(sptr)) {
        /* process module allocatable arrays later; a variable's
         * associated variables need to placed in its own common
         * block.  Can't process here since they would be added
         * to the module's common block.
         */
        px = (ITEMX *)getitem(0, sizeof(ITEMX));
        px->val = sptr;
        px->next = mdalloc_list;
        mdalloc_list = px;
        break;
      }
    }
  /* else fall thru */
  default:
#ifdef DEVICEG
    propagate_device_flags(sptr);
#endif
    if (EQVG(sptr)) {
      /* don't add to module common, its equivalenced var will be */
      break;
    }
    dev = 0;
    con = 0;
    cpyin = 0;
    link = 0;
#ifdef DEVICEG
    if (DEVICEG(sptr) || MANAGEDG(sptr) || ACCCREATEG(sptr) ||
        ACCCOPYING(sptr) || ACCRESIDENTG(sptr))
      dev = 1;
    if (ACCCOPYING(sptr))
      cpyin = 1;
    if (ACCLINKG(sptr)) {
      dev = 1;
      link = 1;
    }
    con = CONSTANTG(sptr);
#endif
    if (XBIT(57, 0x800000) && !dev && !con) {
      /* don't set this for device or constant commons? */
      if (DTY(DTYPEG(sptr)) == TY_ARRAY && !DESCARRAYG(sptr)) {
#ifdef QALNP
        QALNP(sptr, 1); /* quad-word align */
#endif
#ifdef PDALNP
        PDALNP(sptr, 4); /* quad-word align */
#endif
      }
    }
    ty = basedtype(sptr);
    if (ty == 0)
      return; /* don't add to module common */
    if (CFUNCG(sptr)) {
      SCP(sptr, SC_EXTERN);
      return; /* C visable module variable not
                           in common block */
    }
    tysize = size_of(ty);
    acc = PRIVATEG(sptr);
    chr = (DTY(ty) == TY_CHAR || DTY(ty) == TY_NCHAR);
    islong = chr ? 0 : size_of(ty) == 8;
    initd = DINITG(sptr);
    idx = MOD_CMN_IDX(acc, chr, islong, initd, THREADG(sptr), dev, con, cpyin,
                      link);
    if (mod_cmn[idx] == 0)
      mod_cmn[idx] =
          make_module_common(idx, acc, THREADG(sptr), dev, con, cpyin, link);

    if (SOCPTRG(sptr)) {
      /* may have to add 'padding' to the front of this symbol
       * if its offset is nonzero; may have to add 'padding' to
       * the end of this symbol if its overlap list has any
       * variables that extend over the end.
       * NOTE that the ADDRESS fields of the equivalenced variables
       * are still offsets relative to this symbol and the sptr's
       * relative offset from the beginning of its module common
       * has not been assigned.
       */
      ISZ_T offset = ADDRESSG(sptr);
      if (offset > 0) {
        ISZ_T arraysize = (offset + tysize - 1) / tysize;
        int p, pad;
        pad = add_padding(sptr, ty, arraysize, idx);
        for (p = SOCPTRG(sptr); p; p = SOC_NEXT(p)) {
          int overlap = SOC_SPTR(p);
          ISZ_T overlap_offset = ADDRESSG(overlap);
          ISZ_T overlap_size = size_of(DTYPEG(overlap));
          if (overlap_offset < offset) {
            NEED(soc.avail + 2, soc.base, SOC_ITEM, soc.size, soc.size + 1000);
            SOC_SPTR(soc.avail) = pad;
            SOC_NEXT(soc.avail) = SOCPTRG(overlap);
            SOCPTRP(overlap, soc.avail);
            ++soc.avail;
            SOC_SPTR(soc.avail) = overlap;
            SOC_NEXT(soc.avail) = SOCPTRG(pad);
            SOCPTRP(pad, soc.avail);
            ++soc.avail;
          }
        }
      }
    }
    add_to_common(idx, sptr, 0);
    if (SOCPTRG(sptr)) {
      /* may have to add padding after the variable to account
       * for the extra space taken up by the other variables
       * equivalenced to this one.
       * NOTE that the ADDRESS fields of the equivalenced variables
       * are still offsets relative to this symbol and the sptr's
       * relative offset from the beginning of its module common
       * has been assigned.
       */
      ISZ_T offset = ADDRESSG(sptr);
      ISZ_T sptrsize = size_of(DTYPEG(sptr));
      ISZ_T padsize = 0;
      int p;
      for (p = SOCPTRG(sptr); p; p = SOC_NEXT(p)) {
        int overlap = SOC_SPTR(p);
        ISZ_T overlap_offset = ADDRESSG(overlap) + offset;
        ISZ_T overlap_size = size_of(DTYPEG(overlap));
        if (overlap_offset + overlap_size > offset + sptrsize + padsize) {
          padsize = overlap_offset + overlap_size - offset - sptrsize;
        }
        /* add to common block also */
        ADDRESSP(overlap, overlap_offset);
        add_to_common(idx, overlap, 0);
      }
      if (padsize > 0) {
        int p, pad;
        padsize = (padsize + tysize - 1) / tysize;
        pad = add_padding(sptr, ty, padsize, idx);
        for (p = SOCPTRG(sptr); p; p = SOC_NEXT(p)) {
          int overlap = SOC_SPTR(p);
          ISZ_T overlap_offset = ADDRESSG(overlap);
          ISZ_T overlap_size = size_of(DTYPEG(overlap));
          if (overlap_offset + overlap_size > offset + sptrsize) {
            int sp;
            /* it may already have been added in add_padding */
            for (sp = SOCPTRG(overlap); sp; sp = SOC_NEXT(sp)) {
              if (SOC_SPTR(sp) == pad)
                break;
            }
            if (sp == 0) {
              NEED(soc.avail + 2, soc.base, SOC_ITEM, soc.size,
                   soc.size + 1000);
              SOC_SPTR(soc.avail) = pad;
              SOC_NEXT(soc.avail) = SOCPTRG(overlap);
              SOCPTRP(overlap, soc.avail);
              ++soc.avail;
              SOC_SPTR(soc.avail) = overlap;
              SOC_NEXT(soc.avail) = SOCPTRG(pad);
              SOCPTRP(pad, soc.avail);
              ++soc.avail;
            }
          }
        }
      }
    }
    break;
  }
} /* check_sc */

static ISZ_T
get_address(int sptr)
{
  ISZ_T addr;
  if (!EQVG(sptr) || SCOPEG(sptr) == stb.curr_scope)
    return ADDRESSG(sptr);
  addr = get_address(SCOPEG(sptr));
  addr += ADDRESSG(sptr);
  ADDRESSP(sptr, addr);
  SCOPEP(sptr, stb.curr_scope);
  return addr;
} /* get_address */

static void
fix_module_common(void)
{
  int sptr, symavl;
  int member;
  int ast;
  char *t_nm;
  int sptr1;
  int dtype;
  int ndim;
  ADSC *ad;
  int i;
  ITEMX *px;
  LOGICAL err;
  int pvar;
  int ovar;
  int arrdsc;
  int desc;
  int s, sl;
  int evp, firstevp, e;
  int evpcommon;

  if (gbl.maxsev >= 3) {
    gbl.currsub = modu_sym; /* trick semfin & summary */
    semfin();               /* to cleanup, free space, etc. */
    return;
  }

  BZERO(mod_cmn, char, sizeof(mod_cmn));
  BZERO(mod_cmn_naln, char, sizeof(mod_cmn_naln));

  for (sptr = stb.firstusym; sptr < stb.symavl; sptr++) {
    if (IGNOREG(sptr))
      continue;
    switch (STYPEG(sptr)) {
    case ST_PARAM:
      if (DTY(DTYPEG(sptr)) == TY_ARRAY) {
        /* emit the data inits for the named array constant */
        init_named_array_constant(sptr, modu_sym);
      }
      break;
    default:
      break;
    }
  }

  gbl.rutype = RU_SUBR;   /* trick semfin */
  gbl.currsub = modu_sym; /* trick semfin */

  semfin();

  mdalloc_list = pointer_list = NULL;
  symavl = stb.symavl;
  for (sptr = stb.firstusym; sptr < symavl; sptr++) {
    if (IGNOREG(sptr))
      continue;
    if (SCOPEG(sptr) != stb.curr_scope)
      continue;
    if (ENCLFUNCG(sptr) == 0)
      ENCLFUNCP(sptr, modu_sym);
    if (ENCLFUNCG(sptr) != modu_sym)
      continue;
    if (NOMDCOMG(sptr))
      continue;
    switch (STYPEG(sptr)) {
    case ST_ARRAY:
    case ST_VAR:
    case ST_STRUCT:
    case ST_UNION:
      err = 0;
      if (SCG(sptr) != SC_DUMMY) {
        int dtype, dty;
        dtype = DTYPEG(sptr);
        if (DTY(dtype) == TY_ARRAY && ADJARRG(sptr)) {
          error(310, 3, gbl.lineno,
                "Automatic arrays are not allowed in a MODULE -",
                SYMNAME(sptr));
          err = 1;
        }
        dty = DTYG(dtype);
        if ((dty == TY_CHAR || dty == TY_NCHAR) && ADJLENG(sptr)) {
          error(310, 3, gbl.lineno, "Adjustable-length character variables are "
                                    "not allowed in a MODULE -",
                SYMNAME(sptr));
          err = 1;
        }
      }
      if (!err)
        check_sc(sptr);
      break;
    case ST_IDENT:
      STYPEP(sptr, ST_VAR);
      err = 0;
      if (SCG(sptr) != SC_DUMMY) {
        int dtype, dty;
        dtype = DTYPEG(sptr);
        dty = DTYG(dtype);
        if ((dty == TY_CHAR || dty == TY_NCHAR) && ADJLENG(sptr)) {
          error(310, 3, gbl.lineno, "Adjustable-length character variables are "
                                    "not allowed in a MODULE -",
                SYMNAME(sptr));
          err = 1;
        }
      }
      if (!err)
        check_sc(sptr);
      break;
    case ST_UNKNOWN: /* ignore */
      break;
    case ST_NML:
      if (mod_cmn[NML_MOD] == 0)
        mod_cmn[NML_MOD] = make_module_common(NML_MOD, 0, 0, 0, 0, 0, 0);
      add_to_common(NML_MOD, ADDRESSG(sptr), 0);
      /* mark as referenced, so it gets declared everywhere */
      REFP(sptr, 1);
      break;
    default:
      break;
    }
  }
  /* make sure all overlapped variables are listed in the module common */
  for (i = 0; i < N_MOD_CMN; ++i) {
    if (mod_cmn[i] <= 0)
      continue;
    for (sptr = CMEMFG(mod_cmn[i]); sptr != NOSYM; sptr = SYMLKG(sptr)) {
      int p;
      for (p = SOCPTRG(sptr); p; p = SOC_NEXT(p)) {
        int s = SOC_SPTR(p);
        if (SCG(s) != SC_CMBLK)
          add_to_common(i, s, 0);
      }
    }
  }
  /* Get correct addresses in the module common blocks */
  /* Store in the SCOPE field a symbol pointer to the symbol to which
   * this symbol is equivalenced.  If SCOPEG(sptr)!=module then
   * SCOPEG(sptr) is the symbol to which sptr is equivalenced.
   * Also, ADDRESSG(sptr) is the byte offset of sptr relative to
   * the address of SCOPEG(sptr). */
  firstevp = 0;
  for (evp = sem.eqvlist; evp; evp = EQV(evp).next) {
    if (!HCCSYMG(CMBLKG(EQV(evp).sptr))) {
      /* skip user common blocks */
      continue;
    }
    if (EQV(evp).is_first < 0) {
      firstevp = 0;
    } else if (EQV(evp).is_first > 0) {
      firstevp = evp;
    } else if (firstevp != 0) {
      /* if EQVG(evp->sptr), set address of evp->sptr relative to
       * that of firstevp; otherwise, the other way around */
      if (EQVG(EQV(evp).sptr)) {
        /* see if we've already done this */
        if (SCOPEG(EQV(evp).sptr) == stb.curr_scope) {
          SCOPEP(EQV(evp).sptr, EQV(firstevp).sptr);
          ADDRESSP(EQV(evp).sptr,
                   EQV(firstevp).byte_offset - EQV(evp).byte_offset);
        }
      } else {
        if (SCOPEG(EQV(firstevp).sptr) == stb.curr_scope) {
          /* EQV(evp).sptr already has an address; set address of
           * firstevp relative to that of evp->sptr */
          ADDRESSP(EQV(firstevp).sptr, ADDRESSG(EQV(evp).sptr) +
                                           EQV(evp).byte_offset -
                                           EQV(firstevp).byte_offset);
        }
      }
    }
  }
  firstevp = 0;
  for (evp = sem.eqvlist; evp; evp = EQV(evp).next) {
    if (!HCCSYMG(CMBLKG(EQV(evp).sptr))) {
      /* skip user common blocks */
      continue;
    }
    if (EQV(evp).is_first < 0) {
      firstevp = 0;
    } else if (EQV(evp).is_first > 0) {
      firstevp = evp;
    } else if (firstevp != 0) {
      if (EQVG(EQV(evp).sptr) && SCOPEG(EQV(evp).sptr) != stb.curr_scope) {
        ISZ_T addr = get_address(SCOPEG(EQV(evp).sptr));
        addr += ADDRESSG(EQV(evp).sptr);
        ADDRESSP(EQV(evp).sptr, addr);
        SCOPEP(EQV(evp).sptr, stb.curr_scope);
      }
    }
  }
  for (px = mdalloc_list; px != NULL; px = px->next)
    /* for each allocatable variable, create its run-time descriptor
     *     "module-name$array-name$al"
     */
    make_rte_descriptor(px->val, "al");

  for (px = pointer_list; px != NULL; px = px->next)
    /* for each pointer variable, create its run-time descriptor
     *     "module-name$array-name$ptr"
     */
    make_rte_descriptor(px->val, "ptr");

  gbl.currsub = modu_sym; /* trick summary */
  gbl.rutype = RU_BDATA;  /* write blockdata for module */
}

LOGICAL
has_cuda_data(void)
{
#ifdef DEVICEG
  int cmblk;
  for (cmblk = FIRST_DEV_COMMON; cmblk < LAST_DEV_COMMON; ++cmblk)
    if (mod_cmn[cmblk])
      return TRUE;
  for (cmblk = gbl.cmblks; cmblk > NOSYM; cmblk = SYMLKG(cmblk)) {
    if (SCOPEG(cmblk) == gbl.currsub &&
        (DEVICEG(cmblk) || CONSTANTG(cmblk) || MANAGEDG(cmblk)))
      return TRUE;
  }
#endif
  return FALSE;
} /* has_cuda_data */

static void
export_all(void)
{
  char *t_nm;
  if (module_directory_list == NULL) {
    t_nm = getitem(8, strlen(modu_name) + strlen(MOD_SUFFIX) + 1);
    strcpy(t_nm, modu_name);
  } else {
    /* use first name on the module_directory list */
    int ml;
    ml = strlen(module_directory_list->module_directory);
    t_nm = getitem(8, ml + strlen(modu_name) + strlen(MOD_SUFFIX) + 2);
    if (ml == 0) {
      strcpy(t_nm, modu_name);
    } else {
      strcpy(t_nm, module_directory_list->module_directory);
      if (module_directory_list->module_directory[ml - 1] != '/') {
        strcat(t_nm, "/");
      }
      strcat(t_nm, modu_name);
    }
  }
  strcat(t_nm, MOD_SUFFIX);
  outfile = fopen(t_nm, "w+");
  if (outfile == NULL) {
    error(4, 0, gbl.lineno, "Unable to create MODULE file", t_nm);
    return;
  }
  if (sem.mod_dllexport) {
    /*
     * The DLL flag of the module will not set if the dllexport only occurs
     * within a contained procedure.
     */
    DLLP(modu_sym, DLL_EXPORT);
  }
  if (single_outfile_name) {
    mod_index *p;
    if (single_outfile == NULL) {
      single_outfile = fopen(single_outfile_name, "w+");
      if (single_outfile == NULL) {
        error(4, 0, gbl.lineno, "Unable to create MODULE file",
              single_outfile_name);
        return;
      }
    }
    if (mod_index_list && strcmp(modu_name, mod_index_list->module_name) == 0) {
      fseek(single_outfile, mod_index_list->offset, SEEK_SET);
    } else {
      p = (mod_index *)getitem(8, sizeof(mod_index));
      p->next = mod_index_list;
      p->module_name = strcpy(getitem(8, strlen(modu_name) + 1), modu_name);
      p->offset = ftell(single_outfile);
      mod_index_list = p;
    }
    export_module(single_outfile, modu_name, modu_sym, 0);
  }
  export_module(outfile, modu_name, modu_sym, 1);

#if DEBUG
  if (DBGBIT(4, 4096) || DBGBIT(5, 4096)) {
    fprintf(gbl.dbgfil, "\n>>>>>> export_all begin\n");
    if (DBGBIT(4, 4096))
      dump_ast();
    if (DBGBIT(5, 4096)) {
      symdmp(gbl.dbgfil, DBGBIT(5, 8));
      dmp_dtype();
    }
    fprintf(gbl.dbgfil, "\n>>>>>> export_all end\n");
  }
#endif
}

/*
 * close the single-output combined .mod file
 * write the combined .mod index file, if we're supposed to
 */
void
mod_fini(void)
{
  if (single_outfile) {
    fclose(single_outfile);
    if (single_outfile_index_name) {
      mod_index *p, *q;
      single_outfile = fopen(single_outfile_index_name, "w+");
      if (single_outfile == NULL) {
        error(4, 0, gbl.lineno, "Unable to create MODULE index file",
              single_outfile_index_name);
        return;
      }
      if (mod_index_list) {
        /* reverse the list */
        p = mod_index_list;
        mod_index_list = NULL;
        for (; p; p = q) {
          q = p->next;
          p->next = mod_index_list;
          mod_index_list = p;
        }
        for (p = mod_index_list; p; p = p->next) {
          fprintf(single_outfile, "%" GBL_SIZE_T_FORMAT ":%s " LLF "\n",
                  strlen(p->module_name), p->module_name, p->offset);
        }
      }
      fprintf(single_outfile, "%d:%s %d\n", 0, "", 0);
      fclose(single_outfile);
    }
    single_outfile = NULL;
  } else if (single_outfile_name) {
    /* make sure the file is written as an empty file */
    single_outfile = fopen(single_outfile_name, "w+");
    if (single_outfile)
      fclose(single_outfile);
    if (single_outfile_index_name) {
      single_outfile = fopen(single_outfile_index_name, "w+");
      if (single_outfile)
        fclose(single_outfile);
    }
  }
} /* mod_fini */

#define NO_PTR XBIT(49, 0x8000)
#define NO_CHARPTR XBIT(58, 0x1)
#define NO_DERIVEDPTR XBIT(58, 0x40000)
/*
 * A run-time descriptor is created for an object in the form of a common block
 * consisting of the object's pointer & offset variables and its static
 * descriptor.  The order of the common block members is:
 *     variable's pointer variable
 *     variable's pointer variable
 *     variable's static descriptor
 *          ...
 * Since this common block is created early, need to ensure that
 * the common is not rewritten (i.e., set its SEQ flag).
 *
 * The name of the common block is derived from the name of the module,
 * the name of the object, and the kind of object (module allocatable,
 * dynamic, pointer, etc.) which is denoted by 'suffix'.
 */
static void
make_rte_descriptor(int obj, char *suffix)
{
  int acc, idx, islong, initd, dev, con, cpyin, link;
  int s;

  if (SDSCG(obj) == 0) {
    get_static_descriptor(obj);
    get_all_descriptors(obj);
  }
  SCP(obj, SC_BASED); /* these objects are always pointer-based */

  acc = PRIVATEG(obj);
  islong = sizeof(DT_INT) == 8;
  initd = 0; /* DINITG(obj); -- POINTER could be init'd => NULL() but aux
              * components will be zero, i.e., do not have to explicitly
              * initialize.
              */
#ifdef DEVICEG
  dev = 0;
  cpyin = 0;
  if (DEVICEG(obj) || MANAGEDG(obj) || ACCCREATEG(obj) || ACCRESIDENTG(obj))
    dev = 1;
  if (ACCCOPYING(obj))
    cpyin = 1;
  link = 0;
  if (ACCLINKG(obj)) {
    dev = 1;
    link = 1;
  }
  /*
   * Descriptor for texture pointer is CONSTANT for performance.
   * Otherwise need to allow writing by ALLOCATE/DEALLOCATE in device code.
   * Unless the xbit is set.  Performance problem reported by Kato, FS#20305
   */
  if (TEXTUREG(obj) && POINTERG(obj)) {
    con = CONSTANTG(obj) || dev;
  } else {
    if ((MANAGEDG(obj) && !XBIT(137, 0x4000)) || XBIT(137, 0x40))
      con = CONSTANTG(obj) || dev;
    else
      con = CONSTANTG(obj);
  }
#else
  dev = 0;
  con = 0;
  cpyin = 0;
  link = 0;
#endif
  idx = MOD_CMN_IDX(acc, 0, islong, initd, THREADG(obj), dev, con, cpyin, link);
  if (mod_cmn[idx] == 0)
    mod_cmn[idx] =
        make_module_common(idx, acc, THREADG(obj), dev, con, cpyin, link);
  s = SDSCG(obj);
  add_to_common(idx, s, 1);
  PRIVATEP(s, acc);

  s = PTROFFG(obj);
  add_to_common(idx, s, 1);
  PRIVATEP(s, acc);

  s = MIDNUMG(obj);
  add_to_common(idx, s, 1);
  PRIVATEP(s, acc);

  if (F77OUTPUT) {
    int noptr, dtype, dty, chr;
    dtype = DTYPEG(obj);
    dty = DTYG(dtype);
    noptr = 0;
    chr = 0;
    if (NO_PTR) {
      noptr = 1;
    } else if ((dty == TY_NCHAR || dty == TY_CHAR) && NO_CHARPTR) {
      noptr = 1;
      chr = 1;
    } else if (dty == TY_DERIVED && NO_DERIVEDPTR) {
      noptr = 1;
    }
    if (noptr) {
      int dev, con, cpyin, link;
      islong = sizeof(dty) == 8;
#ifdef DEVICEG
      dev = 0;
      cpyin = 0;
      link = 0;
      if (DEVICEG(obj) || MANAGEDG(obj) || ACCCREATEG(obj) || ACCRESIDENTG(obj))
        dev = 1;
      if (ACCCOPYING(obj))
        cpyin = 1;
      if (ACCLINKG(obj)) {
        dev = 1;
        link = 1;
      }
      con = CONSTANTG(obj);
#else
      dev = 0;
      con = 0;
      cpyin = 0;
      link = 0;
#endif
      idx = MOD_CMN_IDX(acc, chr, islong, initd, THREADG(obj), dev, con, cpyin,
                        link);
      if (mod_cmn[idx] == 0)
        mod_cmn[idx] =
            make_module_common(idx, acc, THREADG(obj), dev, con, cpyin, link);
      add_to_common(idx, obj, 0);
    }
  }
}

/* return the DTYPEG(sym), except for arrays, return its base type */
static int
basedtype(int sym)
{
  int dtype;
  dtype = DTYPEG(sym);
  if (DTY(dtype) == TY_ARRAY)
    dtype = DTY(dtype + 1);
  return dtype;
} /* basedtype */

static void
add_to_common(int cmidx, int mem, int atstart)
{
  int cm;
  cm = mod_cmn[cmidx];
  SCP(mem, SC_CMBLK);
  CMBLKP(mem, cm);
  if (ENCLFUNCG(mem) == 0) {
    ENCLFUNCP(mem, modu_sym);
  }
  if (atstart) {
    if (CMEMLG(cm) <= NOSYM) {
      CMEMLP(cm, mem);
    } else {
      SYMLKP(mem, CMEMFG(cm));
    }
    CMEMFP(cm, mem);
    if (!EQVG(mem)) {
      ISZ_T size;
      size = SIZEG(cm);
      size += size_of_var(mem);
      SIZEP(cm, size);
    }
  } else {
    int s, sptr;
    ISZ_T maddr, msz;

    for (sptr = CMEMFG(mod_cmn[cmidx]); sptr != NOSYM; sptr = SYMLKG(sptr)) {
      if (sptr == mem) {
        goto skipmem; /* already process this member */
      }
    }

    if (CMEMFG(cm) <= NOSYM) {
      CMEMFP(cm, mem);
    } else {
      SYMLKP(CMEMLG(cm), mem);
    }
    CMEMLP(cm, mem);
    SYMLKP(mem, NOSYM);
    if (!EQVG(mem)) {
      ISZ_T size;
      int addr;
#ifdef PDALNG
      if (!XBIT(57, 0x1000000) && PDALNG(mem)) {
        if (PDALNG(cm) < PDALNG(mem))
          PDALNP(cm, PDALNG(mem));
      }
#endif
      size = SIZEG(cm);
      addr = alignment_of_var(mem);
      size = ALIGN(size, addr);
      ADDRESSP(mem, size);
      msz = size_of_var(mem);
      msz = pad_cmn_mem(mem, msz, &mod_cmn_naln[cmidx]);
      size += msz;
      SIZEP(cm, size);
    }
  skipmem:
    /* is there anything else in the common block that should
     * be in the SOC list for this member */
    maddr = ADDRESSG(mem);
    msz = size_of_var(mem);
    for (s = CMEMFG(cm); s > NOSYM; s = SYMLKG(s)) {
      ISZ_T saddr, ssz;
      saddr = ADDRESSG(s);
      ssz = size_of_var(s);
      /* is there an overlay? mem starting point within s space,
       * or s starting point within mem space */
      if (s != mem && ((maddr >= saddr && maddr < saddr + ssz) ||
                       (saddr >= maddr && saddr < maddr + msz))) {
        /* yes, make sure they are in each other's SOC list */
        int p;
        for (p = SOCPTRG(s); p; p = SOC_NEXT(p)) {
          if (SOC_SPTR(p) == mem)
            break;
        }
        if (p == 0) {
          /* not found; add mem to SOC(s), s to SOC(mem) */
          NEED(soc.avail + 2, soc.base, SOC_ITEM, soc.size, soc.size + 1000);
          SOC_SPTR(soc.avail) = mem;
          SOC_NEXT(soc.avail) = SOCPTRG(s);
          SOCPTRP(s, soc.avail);
          ++soc.avail;
          SOC_SPTR(soc.avail) = s;
          SOC_NEXT(soc.avail) = SOCPTRG(mem);
          SOCPTRP(mem, soc.avail);
          ++soc.avail;
        }
      }
    }
  }
  if (DINITG(mem)) {
    DINITP(cm, 1);
  }
}

/* ----------------------------------------------------------- */

/* true if reading the module file for the
 * subprograms within the 'contains' of the module
 * subprogram.
 */
void
readin_mod(LOGICAL within_contain)
{
  if (within_contain) {
    init_use_tree();
  }
  read_module(within_contain, NULL, 0);
  sem.mod_public_level = sem.scope_level - 1;
}

/* within_contain: true if reading the module file for the subprograms within
 *     the 'contains' of the module subprogram.
 */
static void
read_module(LOGICAL within_contain, int *pmodsym, int top_scope_level)
{
  FILE *fd;
  char *file_name;
  int i, j;
  char ch;
  int dscptr;
  LOGICAL ignore_private;

  if (within_contain) {
  } else if (use_fd == NULL)
    return;

  if (within_contain) {
    restore_module_state();
    limitsptr = stb.symavl;
  } else {
    fd = use_fd;
    ignore_private = TRUE;
    file_name = use_file_name;
    SCOPEP(*pmodsym, 0);
    *pmodsym = import_module(fd, file_name, within_contain, ignore_private,
                             *pmodsym, top_scope_level);
    DINITP(*pmodsym, 1);
  }

#if DEBUG
  if (DBGBIT(4, 8192) || DBGBIT(5, 8192)) {
    fprintf(gbl.dbgfil, ">>>>>>>>> begin readin_mod %s\n",
            within_contain ? "within contain" : "use");
    if (DBGBIT(4, 8192))
      dump_ast();
    if (DBGBIT(5, 8192)) {
      symdmp(gbl.dbgfil, DBGBIT(5, 8));
      dmp_dtype();
    }
    fprintf(gbl.dbgfil, ">>>>>>>>> end readin_mod\n");
  }
#endif

  if (within_contain) {
    if (exportb.hmark.maxast >= astb.avl) {
      /*
       * The max ast read from the module file is greater than the
       * the last ast created; allocate asts so that the available
       * ast # is 1 larger than the max ast read.
       */
      i = exportb.hmark.maxast - astb.avl;
      do {
        (void)new_node(A_ID);
      } while (--i >= 0);
    }
  }

}

int
mod_add_subprogram(int subp)
{
  int new_sb;
  char b1[MAXIDLEN + 1];
  char b2[MAXIDLEN + 1];
  int i;
  LOGICAL any_impl;

  /*
   * a 'procedure' of the same name as the contained procedure could
   * have been created in the module specification part.  One example
   * is when the procedure appears in a generic interface, i.e., from
   * FS#17246:
   *   interface constructor
   *     procedure subr
   *     !! moduleprocedure subr ! is a work-around
   *   end interface
   *   ...
   *   contains
   *     subroutine subr
   *   ...
   * In this situation, it's better to just represent the procedure
   * as an alias of the contained procedure, subp
   */
  for (new_sb = HASHLKG(subp); new_sb; new_sb = HASHLKG(new_sb)) {
    /*
     * search the hash list of the contained routine for a  ST_PROC
     * in the same scope; if found use it as the alias!
     */
    if (NMPTRG(new_sb) != NMPTRG(subp))
      continue;
    if (STYPEG(new_sb) == ST_PROC && SCOPEG(new_sb) == gbl.currmod) {
      int swp = subp;
      subp = new_sb;
      new_sb = swp;
      break;
    }
  }
  if (!new_sb) {
    /*  ST_PROC of the same name not found  */
    new_sb = insert_dup_sym(subp);
  }
  if (ENCLFUNCG(new_sb) == 0) {
    ENCLFUNCP(new_sb, gbl.currmod);
  }
  STYPEP(subp, ST_ALIAS);
  DPDSCP(subp, 0);
  PARAMCTP(subp, 0);
  FUNCLINEP(subp, 0);
#ifdef EXTRP
  EXTRP(subp, 0);
#endif
  FVALP(subp, 0);
  SYMLKP(subp, new_sb);
  INMODULEP(new_sb, 1);
  SCOPEP(subp, gbl.currmod);
  if (sem.mod_dllexport) {
    DLLP(subp, DLL_EXPORT);
    DLLP(new_sb, DLL_EXPORT);
  }
  export_append_sym(subp);

  any_impl = FALSE;
  for (i = 0; i < impl.avl; i++) {
    IMPL *ipl;
    ipl = impl.base + i;
    ast_implicit(ipl->firstc, ipl->lastc, ipl->dtype);
    if (ipl->dtype != 0)
      any_impl = TRUE;
  }
  /*
   * if there were any IMPLICITs associated with spec lists, adjust
   * the dtypes of function and dummy arguments if necessary.
   */
  if (any_impl) {
    int arg;
    int count;

    if (gbl.rutype == RU_FUNC && !DCLDG(subp)) {
      setimplicit(subp);
      DTYPEP(new_sb, DTYPEG(subp)); /* propogate */
    }

    i = DPDSCG(subp);
    for (count = PARAMCTG(subp); count > 0; count--) {
      arg = aux.dpdsc_base[i];
      if (!DCLDG(arg))
        setimplicit(arg);
      i++;
    }
  }
  if (XBIT(52,0x80)) {
    char linkage_name[2048];
    snprintf(linkage_name, sizeof(linkage_name),
             ".%s.%s", modu_name, SYMNAME(new_sb));
    ALTNAMEP(new_sb, getstring(linkage_name, strlen(linkage_name)));
  }
  return new_sb;
}

void
mod_end_subprogram(void)
{
  if (sem.mod_cnt == 1) {
    export_public_used_modules(sem.mod_public_level);
  }
}

static void
export_public_used_modules(int scopelevel)
{
  if (sem.mod_public_flag && sem.scope_stack) {
    SCOPESTACK *scope = get_scope(scopelevel);
    for (; scope != 0; scope = next_scope(scope)) {
      if (scope->kind == SCOPE_USE && !scope->private) {
        export_public_module(scope->sptr, scope->except);
      }
    }
  }
}

void
mod_end_subprogram_two(void)
{
  int subp;
  int count;
  int i, sptr, dpdsc, arg, link;
  ACCL *accessp;

  if (sem.mod_cnt == 1) {
    /* go through symbols, see if any should be private */
    if (!sem.mod_public_flag) {
      for (sptr = limitsptr; sptr < stb.symavl; ++sptr) {
        switch (STYPEG(sptr)) {
        case ST_UNKNOWN:
        case ST_NML:
        case ST_PROC:
        case ST_PARAM:
        case ST_TYPEDEF:
        case ST_OPERATOR:
        case ST_MODPROC:
        case ST_CMBLK:
        case ST_IDENT:
        case ST_VAR:
        case ST_ARRAY:
        case ST_DESCRIPTOR:
        case ST_STRUCT:
        case ST_UNION:
        case ST_ALIAS:
        case ST_ENTRY:
          PRIVATEP(sptr, 1);
          break;
        default:
          break;
        }
      }
    }
    for (accessp = sem.accl.next; accessp != NULL; accessp = accessp->next) {
      sptr = accessp->sptr;
      if (sptr >= limitsptr) {
        PRIVATEP(sptr, accessp->type == 'v');
      }
    }
    /* see if any should be marked public or private */
    for (sptr = stb.firstusym; sptr < stb.symavl; ++sptr) {
      switch (STYPEG(sptr)) {
      case ST_MODPROC:
      case ST_ALIAS:
        link = SYMLKG(sptr);
        if (link) {
          if (PRIVATEG(sptr)) {
            PRIVATEP(link, 1);
          } else {
            PRIVATEP(link, 0);
          }
        }
        break;
      case ST_PROC:
        /* mark the arguments */
        for (dpdsc = DPDSCG(sptr), i = PARAMCTG(sptr); i; --i, ++dpdsc) {
          arg = aux.dpdsc_base[dpdsc];
          PRIVATEP(arg, PRIVATEG(sptr));
        }
        break;
      default:;
      }
    }
    /* set 'DCLD' so it will not be implicitly typed; the leading
     * character has been changed by mangling, so implicit typing will fail */
    if (gbl.rutype == RU_FUNC) {
      if (STYPEG(gbl.currsub) == ST_ALIAS && SYMLKG(gbl.currsub) > NOSYM) {
        DCLDP(SYMLKG(gbl.currsub), 1);
      } else if (STYPEG(gbl.currsub) == ST_ENTRY) {
        DCLDP(gbl.currsub, 1);
      }
    }

    reset_module_state();
  }
}

void rw_mod_state(RW_ROUTINE, RW_FILE)
{
  int nw;
  RW_SCALAR(usedb.avl);
  if (usedb.avl) {
    if (ISREAD()) {
      if (usedb.sz == 0) {
        usedb.sz = usedb.avl + 5;
        NEW(usedb.base, USED, usedb.sz);
      } else {
        NEED(usedb.avl, usedb.base, USED, usedb.sz, usedb.avl + 5);
      }
    }
    RW_FD(usedb.base, USED, usedb.avl);
  }
} /* rw_mod_state */

#if DEBUG
void
moddmp(char *modfn, FILE *modfil)
{
  fprintf(stderr, "--- pgmoddmp of %s\n", modfn);
}
#endif
