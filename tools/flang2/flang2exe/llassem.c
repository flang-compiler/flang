/*
 * Copyright (c) 2014-2017, NVIDIA CORPORATION.  All rights reserved.
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
   LLVM backend routines. This backend is Fortran-specific.
 */

#include "gbldefs.h"
#include "global.h"
#include "error.h"
#include "symtab.h"
#include "dinit.h"
#include "syms.h"
#include "version.h"
#include "machreg.h"
#include "dbg_out.h"
#include "assem.h"
#include "fih.h"
#include "x86.h"
#include "ili.h"
#include "llutil.h"
#include "cgllvm.h"
#include "cg.h"
#include "llassem.h"
#include "ll_write.h"
#include "ll_structure.h"
#include "expand.h"
#include "outliner.h"
#include "upper.h"

/* --- AGB local --- */
static AGB_t agb_local;
#define AGL_SYMLK(s) agb_local.s_base[s].symlk
#define AGL_HASHLK(s) agb_local.s_base[s].hashlk
#define AGL_NMPTR(s) agb_local.s_base[s].nmptr
#define AGL_TYPENMPTR(s) agb_local.s_base[s].type_nmptr
#define AGL_ARGNMPTR(s) agb_local.s_base[s].farg_nmptr
#define AGL_DTYPE(s) agb_local.s_base[s].dtype
#define AGL_REF(s) agb_local.s_base[s].ref
#define AGL_NEEDMOD(s) agb_local.s_base[s].needmod
#define AGL_NAME(s) agb_local.n_base + agb_local.s_base[s].nmptr
#define AGL_TYPENAME(s) agb_local.n_base + agb_local.s_base[s].type_nmptr
#define AGL_ARGNAME(s) agb_local.n_base + agb_local.s_base[s].farg_nmptr
#define AGL_ARGDTLIST(s) agb_local.s_base.argdtlist

#define LAST_SEC 28
static const struct sec_t sections[LAST_SEC] = {
  [NVIDIA_FATBIN_SEC] = {".nvFatBinSegment", 8},
  [NVIDIA_MODULEID_SEC] = {"__nv_module_id", 8},
  [NVIDIA_RELFATBIN_SEC] = {"__nv_relfatbin", 8},
  [NVIDIA_OLDFATBIN_SEC] = {".nv_fatbin", 8}
};

/*  declare functions:  */

static void write_consts(void);
static void write_comm(void);
static void write_statics(void);
static void write_bss(void);
static void write_externs(void);
static void write_typedescs(void);
static void write_extern_inits(void);
static char *get_altname(int);
static char *get_struct_from_dsrt(int sptr, DSRT *, ISZ_T, int *, LOGICAL,
                                  ISZ_T);
static void dinits(void);
static LOGICAL llassem_struct_needs_cast(int sptr);
static void put_kstr(int sptr, int add_null);
static int align_dir_value();
static void upcase_name(char *);
static char *write_ftn_type(LL_Type *, char *, int);
static void write_module_as_subroutine(void);
static int get_ag_size(int gblsym);

/*
 * There are two possible object file formats:
 *		IS_COFF		IS_ELF
 *		-------		------
 *	coff =>	true		false
 *	elf  =>	false		true
 *
 * There are three possible debug formats:  stabs, coff, and dwarf.  Stabs or
 * dwarf may be generated for either coff or elf object file formats.
 * Stabs-generation is controlled only by an xflag; consequently, 'IS_STABS'
 * must be tested first.  Dwarf-generation is performed if the 'dwarf in coff'
 * xflag is set, or the 'dwarf2' xflag is set, or if the object type is ELF.
 * Coff-generation only occurs for coff object files if the 'dwarf in coff'
 * xflag is not set:
 * +   IS_STABS is true => stabs
 * +   IS_DWARF is true => dwarf in coff, dwarf2, or ELF object file type
 * +   otherwise, the debug format is coff.
 */
#define IS_STABS (XBIT(120, 0x20))

#define ASMFIL gbl.asmfil

char *comment_char;

extern char *current_module;
extern int current_debug_area;

static int static_name_initialized = 0;
static int static_name_global = 0;
static int static_base = 0;
static LL_ObjToDbgList *static_dbg_list;
static int bss_name_initialized = 0;
static int bss_name_global = 0;
static int bss_base = 0;
static char bss_name[MXIDLN];
static LL_ObjToDbgList *bss_dbg_list;
static int ag_ctors_cnt = 0;
#if defined(TARGET_OSX)
static int emitted_bss_name = 0;
static int emitted_static_name = 0;
static int emitted_outer_bss_name = 0;
static int emitted_outer_static_name = 0;
#endif
static char outer_static_name[MXIDLN]; /* Fortran: name of STATIC area for outer
                                          function */
static char contained_static_name[MXIDLN]; /* Fortran: name of STATIC area for
                                              contained function */
static char outer_bss_name[MXIDLN];
static char contained_bss_name[MXIDLN];
int print_stab_lines = FALSE; /* exported to dwarf output module */

#define PRVT_FIRST 32 /* run-time needs 32 bytes for storage */
static struct {
  int addr;   /* next available addr for private variable */
  int sym_sz; /* sym ptr representing size of private area */
} prvt = {PRVT_FIRST, 0};


#define DATA_ALIGN 15
#define MIN_ALIGN_SIZE (DATA_ALIGN + 1) /* flg.quad mininum size */

/* This make sure that common block and its threadprivate pointer each has its
 * own cache line.  If there were in the same cached line as other variables as
 * we saw in fma3d OpenMP where threadprivate pointer shares the same cache line
 * as common block, when there is a write to common block of master thread which
 * threadprivate pointer resides, it also invalidates threadprivate pointer
 * fetched by other threads and causes performance degradation.  We decide to
 * make 128 for all targets as it is safe to do so.
 */
static int max_cm_align = 15; /* max alignment for common blocks */
static int ptr_local = 0;     /* list of function pointer search name */
static int has_init = 0;
static int global_sptr; /* use to prepend for CUDA constructor static
                           initialized data such as ..cuda_constructor_1.BSS or
                           .SECTIONxxx which can be duplicate with other files
                           because name is not unique across file - we make it
                           global to avoid llvm optimization problem that make
                           it read only(aM). */


#ifdef TARGET_WIN
#define CACHE_ALIGN 31
#define ALN_UNIT 32
#elif TARGET_POWER
#define CACHE_ALIGN 127
#define ALN_UNIT 128
#else
#define CACHE_ALIGN 63
#define ALN_UNIT 64
#endif
#define ALN_MINSZ 128000
#define ALN_MAXADJ 4096
#define ALN_THRESH (ALN_MAXADJ / ALN_UNIT)
static int stk_aln_n = 1;
static int bss_aln_n = 1;

/* Information about the layout descriptor currently being written */
static struct {
  SPTR sptr;            /* the symbol that this is a layout descriptor for */
  int entries;          /* entries written so far in layout desc */
  int expected_entries; /* total number of entries to be written */
  LOGICAL wrote_tname;  /* has the layout type struct been written yet? */
  const char *tname;    /* name of layout type struct */
} layout_desc = {0, 0, 0, FALSE, "%struct.ld.memtype"};

/* ******************************************************** */

static int
name_to_hash(const char *ag_name, int len)
{
  int hashval = ag_name[len - 1] | (ag_name[0] << 16) | (ag_name[1] << 8);
  return hashval % AG_HASHSZ;
}

static int
add_ag_name(char *ag_name)
{
  int i, nptr, len, needed;
  char *np;

  len = strlen(ag_name);
  nptr = agb.n_avl;
  agb.n_avl += (len + 1);

  if ((len + 1) >= (32 * 16))
    needed = len + 1;
  else
    needed = 32 * 16;

  NEED(agb.n_avl, agb.n_base, char, agb.n_size, agb.n_size + needed);
  np = agb.n_base + nptr;
  for (i = 0; i < len; i++)
    *np++ = *ag_name++;
  *np = '\0';

  return nptr;
}

static int
add_ag_local_name(char *ag_name)
{
  int i, nptr, len, needed;
  char *np;

  len = strlen(ag_name);
  nptr = agb_local.n_avl;
  agb_local.n_avl += (len + 1);

  if ((len + 1) >= (32 * 16))
    needed = len + 1;
  else
    needed = 32 * 16;

  NEED(agb_local.n_avl, agb_local.n_base, char, agb_local.n_size,
       agb_local.n_size + needed);
  np = agb_local.n_base + nptr;
  for (i = 0; i < len; i++)
    *np++ = *ag_name++;
  *np = '\0';

  return nptr;
}

INLINE static ISZ_T
count_skip(ISZ_T old, ISZ_T new)
{
  return new - old;
}

static int
make_gblsym(int sptr, char *ag_name)
{
  int nptr, hashval, gblsym, dtype;

  gblsym = agb.s_avl++;
  NEED(agb.s_avl, agb.s_base, AG, agb.s_size, agb.s_size + 32);
  BZERO(&agb.s_base[gblsym], AG, 1);

  nptr = add_ag_name(ag_name);
  AG_NMPTR(gblsym) = nptr;
  AG_DLL(gblsym) = DLL_NONE;

  hashval = name_to_hash(ag_name, strlen(ag_name));
  AG_HASHLK(gblsym) = agb.hashtb[hashval];
  agb.hashtb[hashval] = gblsym;

  if (sptr) {
    AG_SC(gblsym) = SCG(sptr);
    AG_STYPE(gblsym) = STYPEG(sptr);
    if (CLASSG(sptr) && DESCARRAYG(sptr)) {
      dtype = get_ftn_typedesc_dtype(sptr);
      AG_LLTYPE(gblsym) = make_lltype_from_dtype(dtype);
    } else if (STYPEG(sptr) == ST_PROC) {
      dtype = get_return_type(sptr);
      AG_LLTYPE(gblsym) = make_lltype_from_dtype(dtype);
    } else
    {
      AG_LLTYPE(gblsym) = make_lltype_from_sptr(sptr);
    }
  }
  return gblsym;
}

static char *
get_ag_searchnm(int sptr)
{
  if (sptr == gbl.currsub && gbl.rutype == RU_PROG)
    return get_main_progname();
  return get_llvm_name(sptr);
}

int
get_typedef_ag(char *ag_name, char *typename)
{
  int gblsym = find_ag(ag_name);

  if (gblsym) {
    if (typename && !AG_TYPENMPTR(gblsym))
      AG_TYPENMPTR(gblsym) = add_ag_name(typename);
    return gblsym;
  }

  /* Enter new symbol into the global symbol table */
  gblsym = make_gblsym(0, ag_name);
  AG_STYPE(gblsym) = ST_TYPEDEF;
  AG_SYMLK(gblsym) = ag_typedef;
  ag_typedef = gblsym;
  if (typename) {
    AG_TYPENMPTR(gblsym) = add_ag_name(typename);
  }
  return 0;
}

int
find_ag(const char *ag_name)
{
  int gblsym, hashval = name_to_hash(ag_name, strlen(ag_name));

  for (gblsym = agb.hashtb[hashval]; gblsym; gblsym = AG_HASHLK(gblsym))
    if (!strcmp(ag_name, AG_NAME(gblsym)))
      return gblsym;

  return 0;
}

/*
 * The F90 front-end has allocated the private variable with respect to a base
 * offset of 0 -- need to adjust the offset so that it's with respect to
 * the first available private offset.
 */
void
fix_private_sym(int sptr)
{
#if DEBUG
  assert(SCG(sptr) == SC_PRIVATE, "fix_private_sym: sym not SC_PRIVATE", sptr,
         3);
#endif
  ADDRESSP(sptr, ADDRESSG(sptr) + 0);
}

void
assemble(void)
{
  if (DBGBIT(14, 128))
    return;

  cg_llvm_init();

  if (gbl.rutype == RU_BDATA) {
    assem_init();
    if (gbl.currsub) { /* need to print out the module as a subroutine */
      int gblsym = find_ag(get_ag_searchnm(gbl.currsub));
      if (!gblsym)
        gblsym = get_ag(gbl.currsub);
      else
        AG_STYPE(gblsym) = ST_ENTRY;
      write_module_as_subroutine();
    }

    assem_data();
  }
  if (has_init)
    assem_end();

} /* endroutine assemble */

/**
   \brief Initialize assem for the source file

   Guaranteed to be called only once per compilation
 */
void
assemble_init(int argc, char *argv[], char *cmdline)
{
  gbl.bss_addr = 0;
  ag_cmblks = 0;
  ag_procs = 0;
  ag_other = 0;
  ag_global = 0;
  ag_typedef = 0;
  ag_ctors_cnt = 0;
  ag_static = 0;
  ag_funcptr = 0;
  agb.s_size = 32;
  agb.s_avl = 1;
  agb.n_size = 32 * 16;
  agb.n_avl = 0;
  NEW(agb.s_base, AG, agb.s_size);
  NEW(agb.n_base, char, agb.n_size);

  /* Set the inital entry to a canary */
  add_ag_typename(0, "BADTYPE");

  gbl.paddr = 0;
}

/* Creates a dtype struct and adds it to the AG table */
static int
generate_struct_dtype(int size, char *name, char *typed)
{
  int ttype, gblsym;
  char gname[MXIDLN];
  LL_Type *llt;

  sprintf(gname, "struct%s", name);
  ttype = mk_struct_for_llvm_init(name, size);
  get_typedef_ag(gname, typed);
  gblsym = find_ag(gname);

  llt = make_lltype_from_dtype(ttype);
  set_ag_lltype(gblsym, llt);

  {
    char override[MXIDLN + 1];
    /* FIXME: LLVM will create its own "unique_name()"
     * This overrides it with fortran name stored in the AG table.
     */
    sprintf(override, "%%%s", gname);
    ll_override_type_string(llt, override);
  }

  if (gbl.currsub)
    AG_DTYPESC(gblsym) = find_ag(get_ag_searchnm(gbl.currsub));
  else
    AG_DTYPESC(gblsym) = 0;

  return gblsym;
}

/* Create a dtype for the type descriptor used to describe the type of sptr
 * This does not add the created symbol to the AG table
 */
int
get_ftn_typedesc_dtype(int sptr)
{
  return mk_struct_for_llvm_init(getsname(sptr), 0);
}

static LOGICAL
llassem_struct_needs_cast(int sptr)
{
  return sptr && ((STYPEG(sptr) == ST_STRUCT) || (STYPEG(sptr) == ST_UNION));
}

#define CHK_REALLOC(_buf, _total, _csz, _pad)   \
  do {                                          \
    if (strlen(_buf) >= _total) {               \
      _total += (strlen(_buf) - _total) + _csz; \
      asrt(strlen(_buf) < _total + _pad);       \
      _buf = realloc(_buf, _total + _pad);      \
    }                                           \
  } while (0)

/**
   \brief Create a struct type from the \c DSRT list
   \param sptr    symbol
   \param dsrtp   head of DSRT list
   \param size    ?
   \param align8  ? [output]
   \param stop_at_sect   When TRUE then return immediately when a new section
   type is encountered on the list. This flag is only useful for processing a
   list of named sections (specifically 'section_inits').
   \param addr    ?
   \return a string of the constructed type

   The struct type is built as follows:
     - Combine all non-pointer together as an array of bytes,
     - Each pointer type emitted as i8*

   All callers must call <tt>free()</tt> on the returned string.
 */
static char *
get_struct_from_dsrt(int sptr, DSRT *dsrtp, ISZ_T size, int *align8,
                     LOGICAL stop_at_sect, ISZ_T addr)
{
  int al, tdtype;
  size_t total_alloc;
  ISZ_T skip_size, repeat_cnt, loc_base;
  char *buf;
  DREC *p;
  ISZ_T i8cnt = 0, n_skip;
  int ptrcnt = 0;
  char tchar[20];
  const int csz = 256;
  const int pad = 32;

  if (llassem_struct_needs_cast(sptr)) {
    LL_Type *llty;
    // recursive call to prop side-effects (setting *align8, etc.)
    buf = get_struct_from_dsrt(0, dsrtp, size, align8, stop_at_sect, addr);
    free(buf);
    llty = make_lltype_from_sptr(sptr);
    assert(llty && (llty->data_type == LL_PTR),
           "type of object must be pointer", 0, ERR_Fatal);
    return strdup(llty->sub_types[0]->str);
  }
  /* This is using string ops (e.g., strcpy, strcat, strlen) therefore
   * we need to account for the terminator, so we add an additional pad
   * The pad should account for the cases where we might overrun the string
   * before we have time to realloc, such as when we append "[ %ld x i8]"
   */
  buf = malloc(csz + pad);
  total_alloc = csz;
  buf[0] = '\0';
  tchar[0] = '\0';
  loc_base = 0;
  repeat_cnt = 1;
  first_data = 1;

  for (; dsrtp; dsrtp = dsrtp->next) {
    loc_base = dsrtp->offset; /* assumes this is a DINIT_LOC */
    if (dsrtp->sectionindex != DATA_SEC) {
      switch (dsrtp->sectionindex) {
      case NVIDIA_FATBIN_SEC:
      case NVIDIA_RELFATBIN_SEC:
      case NVIDIA_OLDFATBIN_SEC:
        *align8 = 1;
      }
      gbl.func_count = dsrtp->func_count;
    } else {
      if (addr < dsrtp->offset) {
        if (ptrcnt) {
          if (!first_data)
            strcat(buf, ", ");
          if (!i8cnt)
            strcat(buf, "[" /*]*/);
          ptrcnt = 0;
        } else if (!i8cnt) {
          if (!first_data)
            strcat(buf, ", ");
          strcat(buf, "[" /*]*/);
        }
        i8cnt = i8cnt + count_skip(addr, dsrtp->offset);
        addr = dsrtp->offset;
        first_data = 0;
      } else if (addr > dsrtp->offset) {
        error(164, 2, 0, SYMNAME(dsrtp->sptr), CNULL);
        continue;
      }
    }
    dinit_fseek(dsrtp->filepos);
    while ((p = dinit_read())) {
      int size_of_item;

      tdtype = p->dtype;
      if (tdtype == DINIT_LOC || tdtype == DINIT_SLOC) {
        loc_base = ADDRESSG(p->conval);
        break;
      }

      if (tdtype == DINIT_SECT || tdtype == DINIT_DATASECT) {
        if (!first_data && stop_at_sect) {
          if (i8cnt) {
            sprintf(tchar, /*[*/ "%ld x i8] ", i8cnt);
            strcat(buf, tchar);
          }
          return buf;
        }
        break;
      }

      switch (p->dtype) {
      case 0: /* alignment record */
#if DEBUG
        assert(p->conval == 7 || p->conval == 3 || p->conval == 1 ||
                   p->conval == 0,
               "dinits:bad align", (int)p->conval, 3);
#endif
        skip_size = ALIGN(addr, p->conval) - addr;
        if (ptrcnt) {
          if (!first_data)
            strcat(buf, ", ");
          strcat(buf, "[" /*]*/);
          ptrcnt = 0;
        } else if (!i8cnt) {
          if (!first_data)
            strcat(buf, ", ");
          strcat(buf, "[" /*]*/);
        }
        first_data = 0;
        i8cnt = i8cnt + count_skip(addr, ALIGN(addr, p->conval));
        addr = ALIGN(addr, p->conval);
        break;
      case DINIT_ZEROES:
        if (ptrcnt) {
          if (!first_data)
            strcat(buf, ", ");
          strcat(buf, "[" /*]*/);
          ptrcnt = 0;
        } else if (!i8cnt) {
          if (!first_data)
            strcat(buf, ", ");
          strcat(buf, "[" /*]*/);
        }
        i8cnt = i8cnt + ((int)p->conval);
        first_data = 0;
        addr += p->conval;
        break;
      case DINIT_LABEL:
        /*  word to be init'ed with address of label 'tconval' */
        al = alignment(DT_CPTR);
        skip_size = ALIGN(addr, al) - addr;
        if (ptrcnt) {
          if (!first_data)
            strcat(buf, ", ");
          if (skip_size)
            strcat(buf, "[" /*]*/);
          ptrcnt = 0;
        } else if (!i8cnt) {
          if (!first_data)
            strcat(buf, ", ");
          if (skip_size)
            strcat(buf, "[" /*]*/);
        }
        i8cnt = i8cnt + count_skip(addr, ALIGN(addr, al));
        if (i8cnt) {
          sprintf(tchar, /*[*/ "%ld x i8] ", i8cnt);
          strcat(buf, tchar);
          strcat(buf, ", ");
          i8cnt = 0;
          first_data = 0;
        }
        addr = ALIGN(addr, al);
        ptrcnt++;
        strcat(buf, "i8* ");
        addr += size_of(DT_CPTR);
        first_data = 0;
        break;
#ifdef DINIT_FUNCCOUNT
      case DINIT_FUNCCOUNT:
        gbl.func_count = p->conval;
        break;
#endif
      case DINIT_OFFSET:
        n_skip = i8cnt + count_skip(addr, p->conval + loc_base);
        if (ptrcnt) {
          if (!first_data)
            strcat(buf, ", ");
          if (n_skip)
            strcat(buf, "[" /*]*/);
          ptrcnt = 0;
        } else if (!i8cnt) {
          if (!first_data)
            strcat(buf, ", ");
          if (n_skip)
            strcat(buf, "[" /*]*/);
        }
        if (n_skip)
          first_data = 0;
        else
          first_data = 1;
        i8cnt = n_skip;
        addr = p->conval + loc_base;
        break;
      case DINIT_REPEAT:
        repeat_cnt = p->conval;
        break;
      case DINIT_SECT:
        break;
      case DINIT_DATASECT:
        break;
      case DINIT_STRING:
        if (ptrcnt) {
          if (!first_data)
            strcat(buf, ", ");
          strcat(buf, "[" /*]*/);
          ptrcnt = 0;
        } else if (!i8cnt) {
          if (!first_data)
            strcat(buf, ", ");
          strcat(buf, "[" /*]*/);
        }
        addr += p->conval;
        i8cnt += p->conval;
        first_data = 0;
        dinit_fskip(p->conval);
        break;

      default:
        assert(tdtype > 0, "dinits:bad dinit rec", tdtype, 3);

        size_of_item = size_of(tdtype);

        do {
          if (DTY(tdtype) == TY_PTR && size_of_item) {
            if (i8cnt) {
              sprintf(tchar, /*[*/ "%ld x i8] ", i8cnt);
              strcat(buf, tchar);
              i8cnt = 0;
              first_data = 0;
            }
            if (!first_data)
              strcat(buf, ", ");
            strcat(buf, "i8* ");
            ptrcnt++;
          } else if (size_of_item) {
            if (ptrcnt || !i8cnt) {
              if (!first_data)
                strcat(buf, ", ");
              strcat(buf, "[" /*]*/);
              ptrcnt = 0;
            }
            i8cnt = i8cnt + size_of_item;
          }
          if (size_of_item) /* don't do for char*0 */
            first_data = 0;
          addr += size_of_item;
          CHK_REALLOC(buf, total_alloc, csz, pad);
        } while (--(repeat_cnt));
        repeat_cnt = 1;
      }

      CHK_REALLOC(buf, total_alloc, csz, pad);
    } /* end of while(dinit_read()) */

    CHK_REALLOC(buf, total_alloc, csz, pad);
  } /* end of for( ... dsrt) */

  if (size >= (INT)0 && (size >= addr)) {
    if (!i8cnt && (size - addr) > 0) {
      if (!first_data)
        strcat(buf, ", ");
      strcat(buf, "[" /*]*/);
      ptrcnt = 0;
    }
    i8cnt = i8cnt + count_skip(addr, size);
  }
  if (i8cnt) {
    if (ptrcnt) {
      if (!first_data)
        strcat(buf, ", ");
      strcat(buf, "[" /*]*/);
      ptrcnt = 0;
    } else {
      sprintf(tchar, /*[*/ "%ld x i8] ", i8cnt);
      strcat(buf, tchar);
    }
  }
  first_data = 0;
  return buf;
}

/**
   \brief Initialize assem for a function

   Called once per function.  This init is called immediately before any
   processing is performed for a function.
 */
void
assem_init(void)
{
  INT nmptr;
  int sptr, align8, mod_or_sub, subprog;
  char *typed;

  if (has_init == 1) {
    return;
  }

  has_init = 1;
  subprog = gbl.outersub ? gbl.outersub : gbl.currsub;
  mod_or_sub = INMODULEG(subprog) ? INMODULEG(subprog) : subprog;
  if (!mod_or_sub)
    return;

  if (!static_name_initialized) {
    {
      sprintf(static_name, ".STATICS%d", gbl.multi_func_count);
    }
    static_name_global = 0;
    static_base = 0;
  }
  if (!bss_name_initialized) {
    {
      sprintf(bss_name, ".BSS%d", gbl.multi_func_count);
    }
    bss_name_global = 0;
    bss_base = 0;
  }
  static_name_initialized = 1;
  bss_name_initialized = 1;
  if (!gbl.outlined) {
    if (gbl.internal <= 1) {
      strcpy(outer_static_name, static_name);
      strcpy(outer_bss_name, bss_name);
    }
  }
  if (gbl.internal > 1 || gbl.outlined) {
    generate_struct_dtype(0, outer_static_name, NULL);
    generate_struct_dtype(0, outer_bss_name, NULL);
    if (gbl.outlined) {
      generate_struct_dtype(0, contained_static_name, NULL);
      generate_struct_dtype(0, contained_bss_name, NULL);
    } else {
      strcpy(contained_static_name, static_name);
      strcpy(contained_bss_name, bss_name);
    }
  }

  generate_struct_dtype(0, static_name, NULL);
  generate_struct_dtype(0, bss_name, NULL);

  for (sptr = gbl.cmblks; sptr > NOSYM; sptr = SYMLKG(sptr)) {
    int gblsym;
    typed = NULL;
    typed =
        get_struct_from_dsrt(sptr, DSRTG(sptr), SIZEG(sptr), &align8, FALSE, 0);
    gblsym = generate_struct_dtype(0, get_llvm_name(sptr), typed);
    if (!DINITG(sptr)) {
      if (!AG_SIZE(gblsym)) {
        AG_SIZE(gblsym) = SIZEG(sptr);
      } else if (SIZEG(sptr) > AG_SIZE(gblsym)) {
        AG_SIZE(gblsym) = SIZEG(sptr);
        nmptr = add_ag_name(typed);
        AG_TYPENMPTR(gblsym) = nmptr;
      }
    }
    free(typed);
  }

  /* ag_local gets allocated and deallocate for every function */
  ag_local = 0;
  agb_local.s_size = 32;
  agb_local.s_avl = 1;
  agb_local.n_size = 32 * 16;
  agb_local.n_avl = 0;
  NEW(agb_local.s_base, AG, agb_local.s_size);
  NEW(agb_local.n_base, char, agb_local.n_size);
  BZERO(agb_local.hashtb, int, AG_HASHSZ);

  /* ptr_local - store name for function pointer per routine */
  ptr_local = 0;
  fptr_local.s_size = 5;
  fptr_local.s_avl = 1;
  fptr_local.n_size = 5 * 16;
  fptr_local.n_avl = 0;
  NEW(fptr_local.s_base, FPTRSYM, fptr_local.s_size);
  NEW(fptr_local.n_base, char, fptr_local.n_size);
  BZERO(fptr_local.hashtb, int, AG_HASHSZ);

} /* endroutine assem_init */

/**
   \brief Print directives and label for beginning of function.
 */
void
assem_begin_func(int sptr)
{
  /* only f90 host subprograms are global */
  if (gbl.internal > 1)
    return;
  get_ag(sptr);
}

void
assem_put_linux_trace(int sptr)
{
}

void
assem_data(void)
{
  assem_init(); /* put it here - won't hurt if it is already called
                   The reason we put it here because write_statics will
                   attempt to write static data for openacc constructor
                   we need to make sure the the static name is correct
                   with respect gbl.currsub.   This does not happen with
                   native because it does not need to write out static
                   if lcl_inits is empty.
                 */

  dinits();

  write_comm();

  write_extern_inits();
  write_bss(); /* There is a bug in llvm opt where it makes bss area
                  not writable "a", progbits - if we write after
                  the constants  and statics. It is OK if we write before.
                  Example test is f90_correct/dt42.f90
                */
  write_statics();
  write_consts();

  write_externs();

  write_typedescs();
}

void
assem_end(void)
{
  freearea(2);
  dinit_end();
  static_base = 0;
  static_name_global = 0;
  bss_base = 0;
  bss_name_global = 0;
  has_init = 0;
  ag_local = 0;
  FREE(agb_local.s_base);
  FREE(agb_local.n_base);
  agb_local.s_base = NULL;
  agb_local.n_base = NULL;
  agb_local.s_avl = 0;
  agb_local.n_avl = 0;
  agb_local.s_size = 0;
  agb_local.n_size = 0;

  ptr_local = 0;
  FREE(fptr_local.s_base);
  FREE(fptr_local.n_base);
  fptr_local.s_base = NULL;
  fptr_local.n_base = NULL;
  fptr_local.n_avl = 0;
  fptr_local.s_avl = 0;
  fptr_local.n_size = 0;
  fptr_local.s_size = 0;

  reset_equiv_var();
  reset_master_sptr();
  stk_aln_n = 1;
  bss_aln_n = 1;
  static_name_initialized = 0;
  bss_name_initialized = 0;

} /* endroutine assem_end */

/**
   \brief Complete assem for the source file

   Guaranteed to be called only once per compilation
 */
void
assemble_end(void)
{
  int gblsym, tdefsym, align_value;
  char *name, *typed, gname[MXIDLN + 50];
  char *tls = " thread_local";

  if (gbl.has_program) {
    /* If huge page table support (-Mhugetlb) emit the constructor init */
    if (XBIT(129, 0x10000000))
      init_huge_tlb();
#if defined(TARGET_X8664)
    /* -Mflushz */
    if (XBIT(129, 0x2))
      init_flushz();
    /* -Mdaz */
    if (mach.feature[FEATURE_DAZ])
      init_daz();
#endif
    if (XBIT(24, 0x1f9)) { /* any of -Ktrap=... */
      init_ktrap();
    }
  }

  write_external_function_declarations(TRUE);
  llvm_write_ctors();

  /* write out common block which is not initialized */
  align_value = CACHE_ALIGN + 1;
  for (gblsym = ag_cmblks; gblsym; gblsym = AG_SYMLK(gblsym)) {
    if (AG_DSIZE(gblsym))
      continue;
    if (AG_SC(gblsym) == SC_EXTERN) {
      fprintf(ASMFIL, "@%s = linkonce global %s undef\n", AG_NAME(gblsym),
              AG_TYPENAME(gblsym));
    } else {
      ISZ_T sz;
      char tname[20];
      if (AG_ALLOC(gblsym))
        sz = 8;
      else
        sz = AG_SIZE(gblsym);
      name = AG_NAME(gblsym);
      sprintf(gname, "struct%s", name);
      sprintf(tname, "[%ld x i8]", sz);
      get_typedef_ag(gname, tname);
      tdefsym = find_ag(gname);
      typed = AG_TYPENAME(tdefsym);
      fprintf(ASMFIL, "%%struct%s = type < { %s } > \n", name, typed);
      fprintf(ASMFIL, "@%s = %s global %%struct%s ", name,
              AG_ISMOD(gblsym) ? "external" : "common", name);
      fprintf(ASMFIL, "%s, align %d\n",
              AG_ISMOD(gblsym) ? "" : " zeroinitializer", align_value);
      AG_DSIZE(gblsym) = 1;
    }
  }

  for (gblsym = ag_intrin; gblsym; gblsym = AG_SYMLK(gblsym)) {
    print_line(AG_NAME(gblsym));
  }

  /* If this type descriptor has been defined (written to asm) skip,
   * else declare as extern.
   */
  for (gblsym = ag_global; gblsym; gblsym = AG_SYMLK(gblsym)) {
    if (AG_TYPEDESC(gblsym) && !AG_DEFD(gblsym)) {
      fprintf(ASMFIL, "%%%s = type opaque\n", AG_TYPENAME(gblsym));
      fprintf(ASMFIL, "@%s = external global %%%s\n", AG_NAME(gblsym),
              AG_TYPENAME(gblsym));
    }
  }
  for (gblsym = ag_typedef; gblsym; gblsym = AG_SYMLK(gblsym)) {
    if (AG_FINAL(gblsym) && !AG_DEFD(gblsym))
      fprintf(ASMFIL, "@%s = extern_weak global %s \n", AG_NAME(gblsym),
              AG_TYPENAME(gblsym));
    else if (AG_TYPEDESC(gblsym) && !AG_DEFD(gblsym)) {
      fprintf(ASMFIL, "%%%s = type opaque\n", AG_TYPENAME(gblsym));
      fprintf(ASMFIL, "@%s = external global %%%s\n", AG_NAME(gblsym),
              AG_TYPENAME(gblsym));
    }
  }
  for (gblsym = ag_other; gblsym; gblsym = AG_SYMLK(gblsym)) {
    name = AG_NAME(gblsym);
    typed = AG_TYPENAME(gblsym);
    if (AG_ISTLS(gblsym)) {
      fprintf(ASMFIL, "@%s = common%s global %s ", name, tls, typed);
    } else {
      fprintf(ASMFIL, "@%s = common global %s ", name, typed);
    }
    fprintf(ASMFIL, " zeroinitializer , align %d\n", align_value);
  }

  FREE(agb.s_base);
  FREE(agb.n_base);
} /* endroutine assemble_end */

static void
write_consts(void)
{
  if (gbl.consts > NOSYM) {
    SPTR  sptr;
    for (sptr = gbl.consts; sptr > NOSYM; sptr = SYMLKG(sptr)) {
      DTYPE dtype = DTYPEG(sptr);
      if (DTY(dtype) == TY_CHAR) {
        put_fstr(sptr, XBIT(124, 0x8000));
        fputc('\n', ASMFIL);
      } else if (DTY(dtype) == TY_NCHAR) {
        put_kstr(sptr, XBIT(124, 0x8000));
        fputc('\n', ASMFIL);
      } else if (DTY(dtype) != TY_PTR) {
        const char *tyName = char_type(dtype, sptr);
        fprintf(ASMFIL, "@%s = internal constant %s ", getsname(sptr), tyName);
        write_constant_value(sptr, 0, CONVAL1G(sptr), CONVAL2G(sptr), FALSE);
        fputc('\n', ASMFIL);
      }
    }
    if (flg.smp || XBIT(34, 0x200 || gbl.usekmpc)) {
      SPTR tsptr = 0;
      for (sptr = gbl.consts; sptr > NOSYM; sptr = SYMLKG(sptr)) {
        if (tsptr)
          SYMLKP(tsptr, 0);
        tsptr = sptr;
      }
      if (tsptr)
        SYMLKP(tsptr, 0);
    }
  }
  gbl.consts = NOSYM;
}

static DSRT *
process_dsrt(DSRT *dsrtp, ISZ_T size, char *cptr, LOGICAL stop_at_sect,
             ISZ_T addr)
{
  int al, tdtype, putval;
  INT loc_base, skip_cnt;
  ISZ_T repeat_cnt;
  DREC *p;
  ISZ_T i8cnt = 0;
  int ptrcnt = 0;
  char *cptrCopy = strdup(cptr);
  char *ptr = cptrCopy;

  loc_base = 0;
  repeat_cnt = 1;
  first_data = 1;
  for (; dsrtp; dsrtp = dsrtp->next) {
    loc_base = dsrtp->offset; /* assumes this is a DINIT_LOC */

    if (dsrtp->sectionindex != DATA_SEC) {
      gbl.func_count = dsrtp->func_count;
    } else {
      if (addr < dsrtp->offset) {
        skip_cnt = dsrtp->offset - addr;
        if (ptrcnt) {
          if (!first_data && skip_cnt)
            fputs(", ", ASMFIL);
          if (!i8cnt) {
            ptr = put_next_member(ptr);
            fputc('[', ASMFIL);
          }
          ptrcnt = 0;
        } else if (!i8cnt) {
          if (!first_data && skip_cnt)
            fputs(", ", ASMFIL);
          ptr = put_next_member(ptr);
          fputc('[', ASMFIL);
        } else if (i8cnt) {
          if (!first_data && skip_cnt)
            fputs(", ", ASMFIL);
        }
        i8cnt = i8cnt + put_skip(addr, dsrtp->offset);
        first_data = 0;
        addr = dsrtp->offset;
      } else if (addr > dsrtp->offset) {
        error(164, 2, 0, SYMNAME(dsrtp->sptr), CNULL);
        continue;
      }
    }

    dinit_fseek(dsrtp->filepos);
    while ((p = dinit_read())) {
      tdtype = p->dtype;
      if (tdtype == DINIT_LOC || tdtype == DINIT_SLOC) {
        loc_base = ADDRESSG(p->conval);
        break;
      }
      if (tdtype == DINIT_SECT || tdtype == DINIT_DATASECT) {
        if (stop_at_sect) {
          if (i8cnt)
            fputs("] ", ASMFIL);
          return dsrtp;
        }
        break;
      }

      if (DBGBIT(5, 32))
        fprintf(gbl.dbgfil, "call emit_init: i8cnt:%ld ptrcnt:%d\n", i8cnt,
                ptrcnt);

      emit_init(p->dtype, p->conval, &addr, &repeat_cnt, loc_base, &i8cnt,
                &ptrcnt, &ptr);
    }
  }

  if (size >= 0) {
    INT skip_size = size - addr;
    if (skip_size > 0) {
      if (ptrcnt) {
        if (!first_data && skip_size)
          fprintf(ASMFIL, ", ");
        if (!i8cnt) {
          ptr = put_next_member(ptr);
          fprintf(ASMFIL, "[ ");
        }
        ptrcnt = 0;
      } else if (!i8cnt) {
        if (!first_data && skip_size)
          fprintf(ASMFIL, ", ");
        ptr = put_next_member(ptr);
        fprintf(ASMFIL, "[");
      } else if (i8cnt) {
        if (!first_data && skip_size)
          fprintf(ASMFIL, ", ");
      }
    } else if (i8cnt) {
      fprintf(ASMFIL, "] ");
    }
    put_skip(addr, size);
    i8cnt = skip_size;
  }
  free(cptrCopy);
  if (i8cnt)
    fprintf(ASMFIL, "] ");

  return dsrtp;
}

/* Contains the functionality of process_extern_dsrt() */
static void
write_extern_inits(void)
{
  int sptr, vargblsym, typegblsym, align8, needsCast;
  DSRT *dsrtp;
  char gname[256], *typed;
  const char *prefix;

  if (!extern_inits)
    return; /* nothing to do */

  /* Output the initialized values of the externals */
  for (dsrtp = extern_inits; dsrtp; dsrtp = dsrtp ? dsrtp->next : dsrtp) {
    sptr = dsrtp->sptr;
    if (DBGBIT(5, 32))
      fprintf(gbl.dbgfil, "write_extern_inits: %s\n", getsname(sptr));
    sprintf(gname, "struct%s", getsname(sptr));

    /* Get the global symbol or create it if it does not yet exist */
    vargblsym = get_ag(sptr);

    /* Set 'addr' to dsrtp->offset, to avoid generating 'skip' bytes */
    if (DT_ISBASIC(DTYPEG(sptr)) || (STYPEG(sptr) == ST_ARRAY)) {
      typed = strdup(make_lltype_from_dtype(DTYPEG(sptr))->str);
      needsCast = TRUE;
    } else {
      typed = get_struct_from_dsrt(sptr, dsrtp, SIZEG(sptr), &align8, TRUE,
                                   dsrtp->offset);
      needsCast = llassem_struct_needs_cast(sptr);
    }

    /* Save the typedef (if it hasn't already been saved) */
    get_typedef_ag(gname, typed);
    typegblsym = find_ag(gname);
    if (CFUNCG(sptr) && SCG(sptr) == SC_EXTERN) {
      int ttype;
      if (DT_ISBASIC(DTYPEG(sptr))) {
        ttype = DTYPEG(sptr);
      } else {
        ttype = mk_struct_for_llvm_init(getsname(sptr), SIZEG(sptr));
      }
      set_ag_lltype(typegblsym, make_lltype_from_dtype(ttype));
    }

#ifdef CUDAG
    /* Prefix: If cuda then emit internal global (for acc.plat0) */
    if (CUDAG(gbl.currsub) && CFUNCG(sptr) && SCG(sptr) == SC_STATIC)
      prefix = "internal global ";
    else if (CFUNCG(sptr) && SCG(sptr) == SC_STATIC) /* openacc */
      prefix = "internal global ";
    else
      prefix = "global ";
#else
    prefix = "global ";
#endif
    /* Output the struct and data for the struct */
    if (needsCast) {
      int dummy;
      char *bare = get_struct_from_dsrt(0, dsrtp, SIZEG(sptr), &dummy, TRUE,
                                        dsrtp->offset);
      char *alTy = "";
      char *alSep = "";
      fprintf(ASMFIL, "%%struct%s = type %s\n"
                      "@%s.%d = internal %s<{%s}> <{ ",
              getsname(sptr), typed, getsname(sptr), sptr, prefix, bare);
      dsrtp = process_dsrt(dsrtp, -1, bare, FALSE, dsrtp->offset);
      if (get_llvm_version() >= LL_Version_3_8) {
        alTy = typed;
        alSep = ", ";
      }
      fprintf(ASMFIL, " }>\n@%s = alias %s%sbitcast (<{%s}>* @%s.%d to %s*)",
              getsname(sptr), alTy, alSep, bare, getsname(sptr), sptr, typed);
      free(bare);
    } else {
      fprintf(ASMFIL, "%%struct%s = type <{ %s }>\n@%s = %s%%struct%s <{ ",
              getsname(sptr), typed, getsname(sptr), prefix, getsname(sptr));
      /* Setting size to -1, to ignore 'skip' bytes */
      dsrtp = process_dsrt(dsrtp, -1, typed, FALSE, dsrtp->offset);
      fputs(" }>", ASMFIL);
      /* mark it that it has been emitted */
      if (AG_DSIZE(vargblsym) <= 0)
        AG_DSIZE(vargblsym) = 1;
    }
#ifdef CUDAG
    if (CUDAG(gbl.currsub) && CFUNCG(sptr) && SCG(sptr) == SC_STATIC)
      fputs(", align 16", ASMFIL);
#endif
    fputc('\n', ASMFIL);
    free(typed);
  }
}

static void
write_bss(void)
{
  /* XXX: "global" and not "internal global"
   *      hack until llvm opt allows us to specify section attribute flags
   *      LLVM opt is marking certain variables constant and others remain
   *      mutable.  The user defined section will get the attributes (write or
   *      read-only) based on the first object being added to the section.  If
   *      the first object is read-only and subsequent objects are writeable,
   *      a segfault will ensue, as llvm will emit the section as read-only in
   *      this case: http://llvm.org/bugs/show_bug.cgi?id=17246
   */
  int gblsym;
  char *type_str = "internal global";
  char *bss_nm = bss_name;
  
  if (gbl.bss_addr) {
    fprintf(ASMFIL, "%%struct%s = type <{[%" ISZ_PF "d x i8]}>\n", bss_nm,
            gbl.bss_addr);
    fprintf(ASMFIL, "@%s = %s %%struct%s <{[%" ISZ_PF "d x i8] "
            "zeroinitializer }> , align 32", bss_nm, type_str, bss_nm,
            gbl.bss_addr);
    ll_write_object_dbg_references(ASMFIL, cpu_llvm_module, bss_dbg_list);
    bss_dbg_list = NULL;
    fputc('\n', ASMFIL);
    gbl.bss_addr = 0;
  }
} /* write_bss */

/**
   \brief get the altname string for the given \p sptr
   \param sptr  the symbol
 */
static char *
get_altname(int sptr)
{
  int ss, len;
  static char name[MXIDLN];

  ss = ALTNAMEG(sptr);
  len = DTY(DTYPEG(ss) + 1);
  if (len >= MXIDLN)
    len = MXIDLN - 1;
  strncpy(name, stb.n_base + CONVAL1G(ss), len);
  name[len] = '\0';
#if defined(TARGET_WIN)
  if (DECORATEG(sptr)) {
    int can_annotate = ((ARGSIZEG(sptr) == -1) || (ARGSIZEG(sptr) > 0));
    int arg_size = (ARGSIZEG(sptr) > 0) ? ARGSIZEG(sptr) : 0;
    if (can_annotate) {
      sprintf(name, "%s@%d", name, arg_size);
    }
  }
#endif
  return name;
} /* get_altname */

static void
write_statics(void)
{
  /* XXX: "global" and not "internal global"
   *      hack until llvm opt allows us to specify section attribute flags
   *      LLVM opt is marking certain variables constant and others remain
   *      mutable.  The user defined section will get the attributes (write or
   *      read-only) based on the first object being added to the section.  If
   *      the first object is read-only and subsequent objects are writeable,
   *      a segfault will ensue, as llvm will emit the section as read-only in
   *      this case: http://llvm.org/bugs/show_bug.cgi?id=17246
   */
  char *type_str = "internal global";
  char gname[MXIDLN + 50];
  char *typed = NULL;
  int align8 = 16;
  int gblsym, sptr;
  DSRT *dsrtp;
  int count = 0;
  char *static_nm = static_name;

  if (lcl_inits) {
    if (DBGBIT(5, 32)) {
      fprintf(gbl.dbgfil, "write_statics:%s\n", static_nm);
    }
    sprintf(gname, "struct%s", static_nm);
    typed = get_struct_from_dsrt(0, lcl_inits, gbl.saddr, &align8, FALSE, 0);
    get_typedef_ag(gname, typed);
    free(typed);
    gblsym = find_ag(gname);
    typed = AG_TYPENAME(gblsym);
    fprintf(ASMFIL, "%%struct%s = type <{ %s }>\n", static_nm, typed);
    fprintf(ASMFIL, "@%s = %s %%struct%s <{ ", static_nm, type_str, static_nm);
    process_dsrt(lcl_inits, gbl.saddr, typed, FALSE, 0);
    fprintf(ASMFIL, " }>, align 16");
    ll_write_object_dbg_references(ASMFIL, cpu_llvm_module, static_dbg_list);
    static_dbg_list = NULL;
    fputc('\n', ASMFIL);
    count++;
  } else if (gbl.saddr && !gbl.outlined) {
    fprintf(ASMFIL, "%%struct%s = type <{ [%ld x i8] }>\n", static_name,
            (long)gbl.saddr);
    fprintf(ASMFIL, "@%s = %s %%struct%s <{ [%ld x i8] zeroinitializer }>"
            ", align 16", static_name, type_str, static_name, (long)gbl.saddr);
    ll_write_object_dbg_references(ASMFIL, cpu_llvm_module, static_dbg_list);
    static_dbg_list = NULL;
    fputc('\n', ASMFIL);
  }

  for (dsrtp = section_inits; dsrtp; dsrtp = dsrtp->next) {
    sptr = dsrtp->sptr;
    count++;
    if (DBGBIT(5, 32)) {
      fprintf(gbl.dbgfil, "write_statics (section_inits): %s\n",
              getsname(sptr));
    }
    typed = get_struct_from_dsrt(sptr, dsrtp, SIZEG(sptr), &align8, TRUE, 0);
    sprintf(gname, "struct%s", getsname(sptr));
    get_typedef_ag(gname, typed);
    free(typed);
    gblsym = find_ag(gname);
    typed = AG_TYPENAME(gblsym);

    fprintf(ASMFIL, "%%struct%s = type < { %s } >\n", getsname(sptr), typed);
    fprintf(ASMFIL, "@%s = %s %%struct%s ", getsname(sptr), type_str,
            getsname(sptr));
    fprintf(ASMFIL, " <{ ");
    process_dsrt(dsrtp, gbl.saddr, typed, TRUE, 0);
    fprintf(ASMFIL, " }>");
    fprintf(ASMFIL, ", section \"%s\"", sections[dsrtp->sectionindex].name);
    if (sections[dsrtp->sectionindex].align)
      fprintf(ASMFIL, ", align %d", sections[dsrtp->sectionindex].align);
    //ll_write_object_dbg_references(ASMFIL, cpu_llvm_module, get_section_debug_list(sptr));
    //get_section_debug_list(sptr) = NULL;
    fputc('\n', ASMFIL);
  }

  /* Only create when count > 1,  it only creates when section_inits is present.
   *
   * NOTE: If we were to have llvm.used on other variable - we may have updated
   *       our implementation so that it only collect information here and print
   *       in assemble_end.  It only allows one instance per file.
   */
  if (count > 1) {
    if (count) {
      fprintf(ASMFIL, "@llvm.used = appending global [%d x i8*] [\n", count);
      if (lcl_inits) {
        fprintf(ASMFIL, "i8* bitcast (%%struct%s* @%s to i8*)", static_nm,
                static_nm);
        if (section_inits)
          fputc(',', ASMFIL);
        fputc('\n', ASMFIL);
      }
      for (dsrtp = section_inits; dsrtp; dsrtp = dsrtp->next) {
        sptr = dsrtp->sptr;
        fprintf(ASMFIL, "i8* bitcast (%%struct%s* @%s to i8*)", getsname(sptr),
                getsname(sptr));
        if (dsrtp->next)
          fputc(',', ASMFIL);
        fputc('\n', ASMFIL);
      }
      fputs("], section \"llvm.metadata\"\n", ASMFIL);
    }
  }
  lcl_inits = NULL;
  section_inits = NULL;
  extern_inits = NULL;

} /* write_statics */

static void
write_comm(void)
{
  int sptr, gblsym, cmsym;
  int align8;
  char *name;
  int align_value;
  char *typed;
  char gname[MXIDLN + 50];

  for (sptr = gbl.cmblks; sptr > NOSYM; sptr = SYMLKG(sptr)) {
    int cmem;

    first_data = 1;
    process_sptr(sptr);
    if ((cmsym = get_ag(sptr)) == 0)
      continue; /* name conflict occurred */

    if (!DINITG(sptr)) /* process this only when dinit */
      continue;

    if (AG_DSIZE(cmsym))
      continue; /* already init'd, get_ag issues error */

    AG_DSIZE(cmsym) = SIZEG(sptr);

    name = get_llvm_name(sptr);
    sprintf(gname, "struct%s", name);

    /* size may varies - redo if init */
    typed =
        get_struct_from_dsrt(sptr, DSRTG(sptr), SIZEG(sptr), &align8, FALSE, 0);
    get_typedef_ag(gname, typed);
    gblsym = find_ag(gname);

    align_value = CACHE_ALIGN + 1;

    fprintf(ASMFIL, "%%struct%s = type < { %s } > \n", name, typed);
    fprintf(ASMFIL, "@%s = global %%struct%s", name, name);
    fprintf(ASMFIL, " < { ");
    process_dsrt(DSRTG(sptr), SIZEG(sptr), typed, FALSE, 0);
    fprintf(ASMFIL, " } > ");

    DSRTP(sptr, NULL);

    fprintf(ASMFIL, ", align %d\n", align_value);

    for (cmem = CMEMFG(sptr); cmem > NOSYM; cmem = SYMLKG(cmem)) {
      if (MIDNUMG(cmem)) /* some member does not have midnum/no name */
        process_sptr(cmem);
    }

    free(typed);
  }

  for (sptr = gbl.threadprivate; sptr > NOSYM; sptr = TPLNKG(sptr)) {
    if (SCG(sptr) != SC_STATIC) {
      /* find it and it is not found create it. */
      name = get_llvm_name(sptr);
      gblsym = find_ag(name);
      if (!gblsym) {
        gblsym = make_gblsym(sptr, get_llvm_name(sptr));
        AG_SYMLK(gblsym) = ag_other;
        ag_other = gblsym;
        AG_SIZE(gblsym) = size_of(DTYPEG(sptr));
        if (XBIT(69, 0x80))
          AG_ISTLS(gblsym) = 1;
        else
          AG_ISTLS(gblsym) = 0;
      }
      add_ag_typename(gblsym, (char *)char_type(DTYPEG(sptr), 0));
    }
  }
}

static int
has_final_members(int sptr, int visit_flag)
{

  typedef struct visitDty {
    int dty;
    struct visitDty *next;
  } VISITDTY;

  static VISITDTY *visit_list = 0;
  VISITDTY *curr, *new_visit, *prev;

  int rslt;
  int dtype = DTYPEG(sptr);
  int member;

  if (DTY(dtype) == TY_ARRAY)
    dtype = DTY(dtype + 1);

  if (DTY(dtype) != TY_STRUCT)
    return 0;

  if (visit_list) {
    for (curr = visit_list; curr; curr = curr->next) {
      if (curr->dty == dtype)
        return 0;
    }
  }

  NEW(new_visit, VISITDTY, 1);
  new_visit->dty = dtype;
  new_visit->next = visit_list;
  visit_list = new_visit;

  rslt = 0;
  for (member = DTY(dtype + 1); member > NOSYM; member = SYMLKG(member)) {
    if (FINALG(member)) {
      rslt = 1;
      break;
    } else if (has_final_members(member, 1)) {
      rslt = 1;
      break;
    }
  }

  if (!visit_flag && visit_list) {
    for (prev = curr = visit_list; curr;) {

      curr = curr->next;
      FREE(prev);
      prev = curr;
    }
    visit_list = 0;
  }

  return rslt;
}

/* Compute the number of entries that will be written by write_layout_desc().
 * If the logic here doesn't match write_layout_desc() we will fail an assert
 * in end_layout_desc(). */
static int
count_members(DTYPE dtype)
{
  SPTR member;
  int count = 0;
  for (member = DTY(dtype + 1); member > NOSYM; member = SYMLKG(member)) {
    DTYPE dty = DTYPEG(member);
    if (PARENTG(member)) {
      count += count_members(dty);
    } else if (POINTERG(member) || has_final_members(member, 0)) {
      count += 1;
    } else if (DTY(dty) == TY_STRUCT && !CCSYMG(member)) {
      count += count_members(dty);
    }
  }
  return count;
}

static LOGICAL
is_BIGOBJ()
{
  return XBIT(68, 0x1);
}

/* Call this before write_layout_desc(). */
static void
begin_layout_desc(SPTR sptr, DTYPE dtype)
{
  int members = count_members(dtype);

  layout_desc.sptr = sptr;
  layout_desc.entries = 0;
  layout_desc.expected_entries = members;
  if (members > 0) {
    char name[256], buf[256];
    int gblsym;
    int subscript_size = is_BIGOBJ() ? 64 : 32;

    if (!layout_desc.wrote_tname) {
      /* First time, write the layout type: Each member is a struct */
      fprintf(ASMFIL, "%s = type < { [6 x i%d], i8* } >\n", layout_desc.tname,
              subscript_size);
      layout_desc.wrote_tname = TRUE;
    }

    /* Write the array of members (the actual layout descriptor) */
    sprintf(name, "%s$ld", SYMNAME(layout_desc.sptr));
    sprintf(buf, "%%struct.ld.%s", getsname(layout_desc.sptr));
    fprintf(ASMFIL, "%s = type < { [%d x %s], [7 x i%d] } >\n", buf, members,
            layout_desc.tname, subscript_size);

    /* The layout description instance */
    fprintf(ASMFIL, "@%s = global %s < {\n", name, buf);
    fprintf(ASMFIL, "  [%d x %s] [\n", members, layout_desc.tname);

    /* Add to the ag list */
    get_typedef_ag(name, buf);
    gblsym = find_ag(name);
    AG_DEFD(gblsym) = 1;
  }
}

/**
   \brief If there were any entries in the layout descriptor, terminate with
   all-0 entry and return TRUE.
 */
static LOGICAL
end_layout_desc(void)
{
  LOGICAL any_entries = layout_desc.entries > 0;
#if DEBUG
  /* if this fails, logic in count_members doesn't match write_layout_desc */
  assert(layout_desc.entries == layout_desc.expected_entries,
         "end_layout_desc: wrong number of layout descriptor entries", 0,
         ERR_Fatal);
#endif
  if (any_entries) {
    int subscript_size = is_BIGOBJ() ? 64 : 32;
    /* The end of the layout descriptor */
    fprintf(ASMFIL, "  ],\n");
    fprintf(
        ASMFIL,
        "  [7 x i%d] [i%d 0, i%d 0, i%d 0, i%d 0, i%d -1, i%d 0, i%d 0]\n} >\n",
        subscript_size, subscript_size, subscript_size, subscript_size,
        subscript_size, subscript_size, subscript_size, subscript_size);
  }
  layout_desc.sptr = 0;
  layout_desc.entries = 0;
  return any_entries;
}

/**
   \brief Write an entry in the layout desc for this member
 */
static void
write_layout_desc_entry(char tag, int offset, SPTR member, int length,
                        SPTR sdsc)
{
  int subscript_size = is_BIGOBJ() ? 64 : 32;
  int desc_offset = -1;
  int mem_offset = offset + ADDRESSG(member);

  if (SDSCG(member)) {
    desc_offset = offset + ADDRESSG(SDSCG(member));
#if DEBUG
    assert(desc_offset > 0, "write_layout_desc_entry: desc_offset is 0",
           desc_offset, ERR_Severe);
#endif
  }

#if DEBUG
  fprintf(ASMFIL, "    ; member: '%s'\n", SYMNAME(member));
#endif
  /* Write the member data */
  fprintf(ASMFIL, "    %s < {\n", layout_desc.tname);
  fprintf(ASMFIL, "      [6 x i%d] [", subscript_size);
  fprintf(ASMFIL, "i%d %d, ", subscript_size, tag);
  fprintf(ASMFIL, "i%d 0, ", subscript_size);
  fprintf(ASMFIL, "i%d %d, ", subscript_size, mem_offset);
  fprintf(ASMFIL, "i%d %d, ", subscript_size, length);
  fprintf(ASMFIL, "i%d %d, ", subscript_size, desc_offset);
  fprintf(ASMFIL, "i%d 0],\n", subscript_size);

  if (sdsc == 0) {
    fprintf(ASMFIL, "      i8* null\n");
  } else { /* Else a pointer to the typedef which is of type: struct<name> */
    process_sptr(sdsc);
    fprintf(ASMFIL, "      i8* bitcast(%%struct%s* @%s to i8*)\n",
            getsname(sdsc), getsname(sdsc));
  }
  fprintf(ASMFIL, "    } >");
  if (++layout_desc.entries < layout_desc.expected_entries)
    fprintf(ASMFIL, ",");
  fprintf(ASMFIL, "\n");
}

/* Write a layout desc for this dtype, recursing into nested derived types.
   offset is the distance of this dtype from the start of the outermost one.
   Call begin_layout_desc() and end_layout_desc() before and after this. */
static void
write_layout_desc(DTYPE dtype, int offset)
{
  SPTR member;

  for (member = DTY(dtype + 1); member > NOSYM; member = SYMLKG(member)) {
    LOGICAL finals = has_final_members(member, 0);
    DTYPE dty = DTYPEG(member);
    TY_KIND ty = DTY(dty);
    if (PARENTG(member)) {
      write_layout_desc(dty, offset);
    } else if (POINTERG(member) || finals) {
      char tag;
      SPTR sdsc;
      LOGICAL unknown;
      int length;
      DTYPE dty2 = DDTG(dty);

      if (!POINTERG(member)) {
        tag = 'F'; /* finalized object */
      } else if (ty == TY_STRUCT && dtype == dty) {
        tag = 'R'; /* recursive pointer to derived type */
      } else if (ALLOCATTRG(member) || TPALLOCG(member)) {
        tag = 'T';
      } else if (ty == TY_STRUCT) {
        tag = 'D'; /* regular pointer to derived type */
      } else if (ty == TY_PTR) {
        tag = 'S'; /* procedure ptr */
      } else {
        tag = 'P';
      }
      if (DTY(dty2) == TY_STRUCT) {
        int ty = DTY(dty2 + 3);
        sdsc = SDSCG(ty);
      } else {
        sdsc = 0;
      }
      unknown = dty2 == DT_ASSCHAR || dty2 == DT_DEFERCHAR;
      length = CLASSG(member) || unknown ? 0 : (int)size_of(dty);
      write_layout_desc_entry(tag, offset, member, length, sdsc);
    } else if (ty == TY_STRUCT && !CCSYMG(member)) {
      write_layout_desc(dty, ADDRESSG(member));
    }
  }
}

static int
count_parent_pointers(int parent, int level)
{
  const int dtype = DTYPEG(parent);
  int member;
  if (DTY(dtype) != TY_STRUCT)
    return level;
  member = DTY(dtype + 1);
  ++level;
  if (!PARENTG(member))
    return level;
  return count_parent_pointers(PARENTG(member), level);
}

static void
write_parent_pointers(int parent, int level)
{
  int member, tag, gblsym, desc;
  char tdtname[MAXIDLEN];
  const int dtype = DTYPEG(parent);

  if (DTY(dtype) != TY_STRUCT)
    return;

  member = DTY(dtype + 1);
  tag = DTY(dtype + 3);
  desc = SDSCG(tag);
  fprintf(ASMFIL, "    i8* bitcast(%%struct%s* @%s to i8*)",
          get_llvm_name(SDSCG(tag)), get_llvm_name(SDSCG(tag)));

  if (SCG(desc) == SC_EXTERN && CLASSG(desc) && DESCARRAYG(desc)) {
    sprintf(tdtname, "struct%s", get_llvm_name(desc));
    if (get_typedef_ag(get_llvm_name(desc), tdtname) == 0) {
      /* If newly added... (i.e., above get_typedef_ag returns zero) */
      gblsym = find_ag(get_llvm_name(desc));
      AG_TYPEDESC(gblsym) = 1;
    }
  }

  if (level > 1)
    fprintf(ASMFIL, ",");
  --level;
  fprintf(ASMFIL, "\n");

  if (!PARENTG(member))
    return;

  write_parent_pointers(PARENTG(member), level);
}

/* final table size is max dimensions plus 2. The 0th element holds the
 * scalar subroutine and the last element holds the elemental subroutine.
 */
#define FINAL_TABLE_SZ 9

static int
build_final_table(int dtype, int ft[FINAL_TABLE_SZ])
{

  int mem, i, j;

  for (i = 0; i < FINAL_TABLE_SZ; ++i)
    ft[i] = 0;
  for (j = 0, mem = DTY(dtype + 1); mem > NOSYM; mem = SYMLKG(mem)) {
    if (CLASSG(mem) && (i = FINALG(mem))) {
      if (i < 0)
        return -1;
      ft[i - 1] = VTABLEG(mem);
      j++;
    }
  }
  return j;
}

/* Returns the number of entries in the finalizer table */
static int
write_final_table(int sptr, int dtype)
{
  int i, ft[FINAL_TABLE_SZ], entry, gblsym;
  char tname[256];
  LL_Type* ttype;

  if ((i = build_final_table(dtype, ft)) > 0) {
    /* Check to see if this table has already been generated */
    get_typedef_ag(getsname(sptr), NULL);
    gblsym = find_ag(getsname(sptr));
    if (AG_DEFD(gblsym))
      return 0;

    /* Add type name to ag table and define this table */
    sprintf(tname, "[%d x i8*]", FINAL_TABLE_SZ);
    if ((gblsym = get_typedef_ag(getsname(sptr), tname)) ||
        (gblsym = find_ag(getsname(sptr))))
      AG_DEFD(gblsym) = 1;

    fprintf(ASMFIL, "@%s = weak global %s [", getsname(sptr), tname);
    for (i = 0; i < FINAL_TABLE_SZ; ++i) {
      entry = ft[i];
      if (entry) {
        const char *fntype;
        LL_ABI_Info *abi = ll_proto_get_abi(ll_proto_key(entry));
        gblsym = get_ag(entry);
        AG_DEFD(gblsym) = 1;
        fntype = abi ? ll_abi_function_type(abi)->str : "(i8*)";
        fprintf(ASMFIL, "i8* bitcast(%s* @%s to i8*)", fntype, get_llvm_name(entry));
      } else
        fprintf(ASMFIL, "i8* null");

      if (i < FINAL_TABLE_SZ - 1)
        fprintf(ASMFIL, ", ");
    }
    fprintf(ASMFIL, "]\n");

    if (!LLTYPE(sptr)) {
      ttype = make_array_lltype(
          FINAL_TABLE_SZ, make_ptr_lltype(make_lltype_from_dtype(DT_INT)));
      LLTYPE(sptr) =  ttype;
      /* make sure it is i32 */
      // FIXME: why is the pointer being coerced to 32 bits here? On 64 bit
      // systems, how is this correct?
    }
  }

  /* Return the number of entries created */
  if (i < 0)
    return i;

  return 0;
}

static int
has_final_procedures(int sptr)
{
  /* Return true if dtype associated with sptr has final procedures that
   * are ready to be written to assembly file (they have been processed)
   */

  int dtype, mem;
  char *name;
  int len;

  name = SYMNAME(sptr);
  len = strlen(name);

  if (len < 3 || strcmp(name + (len - 3), "$ft") != 0)
    return 0;

  dtype = DTYPEG(sptr);
  dtype = DTY(dtype + 1);

  for (mem = DTY(dtype + 1); mem > NOSYM; mem = SYMLKG(mem)) {
    if (CLASSG(mem) && FINALG(mem) > 0)
      return 1;
  }
  return 0;
}

static int
has_pending_final_procedures(int sptr)
{

  /* Return true if dtype associated with sptr has final procedures but
   * they have not been fully processed yet.
   */

  int dtype, mem;

  dtype = DTYPEG(sptr);
  dtype = DTY(dtype + 1);

  for (mem = DTY(dtype + 1); mem > NOSYM; mem = SYMLKG(mem)) {
    if (CLASSG(mem) && FINALG(mem) < 0)
      return 1;
  }
  return 0;
}

static int
build_vft(int dtype, int **vft)
{

  int vf, vf2, offset;
  int *tmp, *buf;
  static int sz;
  int vf_cnt;
  int member = DTY(dtype + 1);
  int parent = PARENTG(member);

  if (parent) {
    vf_cnt = build_vft(DTYPEG(parent), vft);
  } else {
    vf_cnt = 0;
  }

  buf = *vft;
  if (!buf) {
    sz = 0;
  }

  for (vf = member; vf > NOSYM; vf = SYMLKG(vf)) {
    if (CCSYMG(vf) && CLASSG(vf)) {
      int bind = TBPLNKG(vf);
      int proc = VTABLEG(vf);
      if (bind) {
        offset = VTOFFG(bind) - 1;
        if (offset < 0)
          continue;
        if (offset >= sz) {
          sz = offset + 16;
          NEW(tmp, int, sz);
          memset(tmp, 0, sz * sizeof(int));
          for (vf2 = 0; vf2 < vf_cnt; ++vf2) {
            tmp[vf2] = buf[vf2];
          }
          if (buf)
            FREE(buf);
          buf = tmp;
        }
        if (!buf[offset] && offset >= vf_cnt)
          vf_cnt = (offset + 1);
        buf[offset] = proc;
      }
    }
  }

  *vft = buf;
  return vf_cnt;
}

static int
write_vft(int sptr, int dtype)
{
  int i, vf, *vft, vft_sz, gblsym;
  char *nmptr, tname[MXIDLN + 50], name[MXIDLN];
  const char *fntype;

  vft = 0;
  vft_sz = build_vft(dtype, &vft);
  assert(vft_sz >= 0, "write_vft: Invalid vft size", vft_sz, 4);

  if (vft_sz == 0)
    return 0;

  sprintf(name, "%s$vft", SYMNAME(sptr));
  sprintf(tname, "[%d x i8*]", vft_sz);
  fprintf(ASMFIL, "@%s = global %s [", name, tname);

  /* Add to ag table */
  get_typedef_ag(name, tname);
  gblsym = find_ag(name);
  AG_DEFD(gblsym) = 1;

  /* Check dtype of getsname(vf) and bitcast accordingly */
  fntype = NULL;
  for (i = 0; i < vft_sz; ++i) {
    vf = vft[i];
    if (vf) {
      LL_ABI_Info *abi = ll_proto_get_abi(ll_proto_key(vf));
      if (abi)
        fntype = ll_abi_function_type(abi)->str;
    }
    if (vf && !fntype) {
      if (STYPEG(vf) == ST_PROC)
        fntype = "void()";
      else if (SCG(vf) == SC_CMBLK) {
        /* example: oop219 - shape_mode_0 is in vft table */
        gblsym = find_ag(get_llvm_name(vf));
        nmptr = AG_NAME(gblsym);
        sprintf(tname, "struct%s", nmptr);
        if (!find_ag(tname)) {
          fntype = "i8* null";
          continue;
        }
        sprintf(tname, "%%struct%s", nmptr);
      }
    }

    /* Emit the vft entry */
    if (vf && fntype)
      fprintf(ASMFIL, "i8* bitcast(%s* @%s to i8*)", fntype, getsname(vf));
    else
      fprintf(ASMFIL, "i8* null");

    if (i < (vft_sz - 1))
      fprintf(ASMFIL, ", ");
  }

  fprintf(ASMFIL, "]\n");
  FREE(vft);
  return vft_sz;
}

/* Create a string in ll to reference the start of a table with
 * name @<name><suffix>.
 *
 * If is_struct is true, then the table is actually a struct and
 * n_elts will be ignored.
 *
 * The only use of is_struct is to generate a pointer to the finalizer created
 * in write_final_table().
 */
static void
put_ll_table_addr(const char *name, const char *suffix, LOGICAL is_struct,
                  int n_elts, LOGICAL explicit_gep_type)
{
  int gblsym;
  char buf[256];
  char *elem_type;

  elem_type = "";
  /* Decide if we need extra element type argument to GEP */
  if (explicit_gep_type)
    elem_type = "i8, ";

  asrt(!(n_elts && is_struct));

  sprintf(buf, "%s%s", name, suffix);
  gblsym = find_ag(buf);

  if (n_elts && gblsym)
    fprintf(ASMFIL, "i8* bitcast(i8* getelementptr("
                    "%si8* bitcast(%s* @%s to i8*), i32 0) to i8*)",
            elem_type, AG_TYPENAME(gblsym), AG_NAME(gblsym));
  else if (n_elts && !gblsym) /* Usually the case for finalizers */
    fprintf(ASMFIL, "i8* bitcast(i8* getelementptr("
                    "%si8* bitcast([%d x i8*]* @%s%s to i8*), i32 0) to i8*)",
            elem_type, n_elts, name, suffix ? suffix : "");
  else if (is_struct)
    fprintf(ASMFIL, "i8* bitcast(%s* @%s to i8*)", AG_TYPENAME(gblsym),
            AG_NAME(gblsym));
  else
    fprintf(ASMFIL, "i8* null");
}

static void
write_typedescs(void)
{
  int sptr, dtype, tag, member, level, vft;
  char *name, *sname, *suffix, tdtname[MXIDLN];
  int len, inmod, gblsym, eq;
  int has_layout_desc;
  int ft, size, integer_size, subscript_size;
  int subprog;

  integer_size = subscript_size = 32;
  integer_size = 64;
  if (XBIT(68, 0x1)) {
    subscript_size = 64;
  }

  for (sptr = gbl.typedescs; sptr > NOSYM; sptr = TDLNKG(sptr)) {
    if (UPLEVELG(sptr))
      continue;

    gblsym = 0;
    subprog =
        (gbl.outersub && SCG(sptr) == SC_EXTERN) ? gbl.outersub : gbl.currsub;
    if (has_final_procedures(sptr)) {
      dtype = DTYPEG(sptr);
      dtype = DTY(dtype + 1);
      gblsym = get_ag(sptr);
      if (!gblsym)
        gblsym = find_ag(get_ag_searchnm(sptr));
      if (gblsym)
        ft = write_final_table(sptr, dtype);
      continue;
    } else {
      ft = has_pending_final_procedures(sptr);
    }
    inmod = INMODULEG(subprog);
    if (inmod) {
      name = SYMNAME(sptr);
      if (strncmp(SYMNAME(inmod), name, strlen(SYMNAME(inmod))) != 0) {
        continue;
      }
    } else {
      name = SYMNAME(sptr);
      if (strncmp(SYMNAME(subprog), name, strlen(SYMNAME(subprog))) != 0) {
        continue;
      }
    }
    len = strlen(SYMNAME(sptr)) + 1;
    NEW(name, char, len);
    strcpy(name, SYMNAME(sptr));
    suffix = strchr(name, '$');
    if (suffix)
      *suffix = '\0';
    eq = strcmp(SYMNAME(inmod), name);
    if (inmod && eq != 0) {
      FREE(name);
      continue;
    } else if (eq && strcmp(SYMNAME(subprog), name) != 0) {
      FREE(name);
      continue;
    }
    FREE(name);
    if (SCG(sptr) == SC_EXTERN) {
      gblsym = get_ag(sptr);
      if (!gblsym && !(gblsym = find_ag(get_llvm_name(sptr))))
        continue;
    } else {
      gblsym = 0;
    }

    if (gblsym && AG_DEFD(gblsym))
      continue;

    dtype = DTYPEG(sptr);
    dtype = DTY(dtype + 1);
    tag = DTY(dtype + 3);
    member = DTY(dtype + 1);
    begin_layout_desc(sptr, dtype);
    write_layout_desc(dtype, 0);
    has_layout_desc = end_layout_desc();

    vft = write_vft(sptr, dtype);
    level = 0;

    if (ft) {
      int gs;
      char suffix[5];
      char *name = getsname(sptr);
      if (!strchr(name, '$')) {
        if (*(name + (strlen(name) - 1)) == '_')
          strcpy(suffix, "ft_");
        else
          strcpy(suffix, "_ft");
      } else {
        if (*(name + (strlen(name) - 1)) == '_')
          strcpy(suffix, "$ft_");
        else
          strcpy(suffix, "$ft");
      }
      /* make sure it is not in ag table first */
      sprintf(tdtname, "%s%s", getsname(sptr), suffix);
      gs = find_ag(tdtname);
      if (!gs) {
        char typename[20];
        sprintf(typename, "[%d x i8*]", FINAL_TABLE_SZ);
        get_typedef_ag(tdtname, typename);
        gs = find_ag(tdtname);
        AG_FINAL(gs) = 1;
      }
    }

    name = getsname(sptr);
    sname = SYMNAME(sptr);

    /* Create a type name and struct for the type descriptor data type */
    sprintf(tdtname, "%%struct%s", name);
    level = count_parent_pointers(PARENTG(member), 0);

    /* Array of pointers: the types this inherits/extends (parents) */
    if (level) {
      fprintf(ASMFIL, "%%struct%s$parents = type < { [%d x i8*] } >\n", name,
              level);
      fprintf(ASMFIL, "@%s$parents = global %%struct%s$parents < {\n", name,
              name);
      fprintf(ASMFIL, "  [%d x i8*] [\n", level);
      write_parent_pointers(member, level);
      fprintf(ASMFIL, "  ]\n");
      fprintf(ASMFIL, "} >, align 8\n");
    }

    /* Create the type for the type descriptor (in ll) */
    size = level * sizeof(void *);
    size += (9 * 4) + (5 * sizeof(void *)) + sizeof(strlen(sname));
    fprintf(ASMFIL, "%s = type ", tdtname);

    /* keep entry in ag table even though we print it here - just to keep
     * track */
    if (!find_ag(tdtname)) {
      int gs;
      int ttype;
      char *ptr;
      char typename[100];
      LL_Type *llt;

      sprintf(typename, "[8 x i%d], i%d, [5 x i8*], [%d x i8]",
              subscript_size, integer_size, (int)strlen(sname));

      ptr = tdtname + 1; /* move past first letter '%' */
      get_typedef_ag(ptr, typename);
      ttype = mk_struct_for_llvm_init(name, 0);
      llt = make_lltype_from_dtype(ttype);
      gs = get_typedef_ag(ptr, NULL);
      set_ag_lltype(gs, llt);
    }

    fprintf(ASMFIL, "< { [8 x i%d], [6 x i8*], [%d x i8] } >\n",
            subscript_size, (int)strlen(sname));

    /* Create the global instance of the type descriptor */
    fprintf(ASMFIL, "@%s = global %s < {\n", name, tdtname);

    /* First array of values */
    fprintf(ASMFIL, "  [8 x i%d] [", subscript_size);
    fprintf(ASMFIL, "i%d 43, ", subscript_size);
    fprintf(ASMFIL, "i%d %d, ", subscript_size, !UNLPOLYG(tag) ? 33 : 43);
    fprintf(ASMFIL, "i%d %d, ", subscript_size, level);
    fprintf(ASMFIL, "i%d %d, ", subscript_size, (int)size_of(dtype));
    fprintf(ASMFIL, "i%d 0, i%d 0, i%d 0, i%d 0],\n", subscript_size,
            subscript_size, subscript_size, subscript_size);

    /* Pointer array: symbol address and tables (vft, ft, layout) */
    fprintf(ASMFIL, "  [6 x i8*] [\n");
    if (TYPDEF_INITG(tag) > NOSYM) {
      /* pointer to initialized prototype */
      const char *initname = getsname(TYPDEF_INITG(tag));
      fprintf(ASMFIL, "     i8* bitcast(i8* getelementptr(i8, i8* "
              "bitcast(%%struct%s* @%s to i8*), i32 %ld) to i8*),\n",
              initname, initname, (long)ADDRESSG(TYPDEF_INITG(tag)));
    } else {
      fprintf(ASMFIL, "     i8* null,\n");
    }

    fprintf(ASMFIL, "    i8* bitcast(%s* @%s to i8*),\n", tdtname,
            getsname(sptr));

    /* Pointer to vft */
    fprintf(ASMFIL, "    ");
    put_ll_table_addr(sname, "$vft", FALSE, vft,
                      ll_feature_explicit_gep_load_type(&cpu_llvm_module->ir));
    fprintf(ASMFIL, ",\n");
    fprintf(ASMFIL, "    i8* null,\n"); /* 0 */

    /* Pointer to finializer table (always same size) */
    fprintf(ASMFIL, "    ");
    put_ll_table_addr(getsname(sptr), "ft_", FALSE, ft ? FINAL_TABLE_SZ : 0,
                      ll_feature_explicit_gep_load_type(&cpu_llvm_module->ir));
    fprintf(ASMFIL, ",\n");

    /* Pointer to layout descriptor */
    fprintf(ASMFIL, "    ");
    if (has_layout_desc)
      put_ll_table_addr(sname, "$ld", TRUE, 0,
                       ll_feature_explicit_gep_load_type(&cpu_llvm_module->ir));
    else
      fprintf(ASMFIL, "i8* null");
    fprintf(ASMFIL, "\n");

    /* Third array (string symbol name) */
    fprintf(ASMFIL, "  ],\n");
    fprintf(ASMFIL, "  [%d x i8] c\"%s\"\n", (int)strlen(sname), sname);
    fprintf(ASMFIL, "} >");
    if (level)
      fprintf(ASMFIL, ", align 1");
    fprintf(ASMFIL, "\n");

    /* Add name and its type (gname) to global symbol table */
    if (gblsym) {
      AG_DEFD(gblsym) = 1;
      AG_SIZE(gblsym) = size;
      AG_TYPEDESC(gblsym) = 1; /* This is a type descriptor */
      AG_DTYPESC(gblsym) = 0;
    }
    process_sptr(sptr);
  }

  gbl.typedescs = NOSYM;
}

/* TODO: get_ag will add sptr to the AG table.  We have to do this or we will
 * get undefined references to externally defined type descriptors.
 */
LOGICAL
is_typedesc_defd(int sptr)
{
  int gblsym;

  if ((gblsym = get_ag(sptr))) /* Force add sptr to the ag table */
    return AG_DEFD(gblsym);
  return AG_DEFD(find_ag(getsname(sptr)));
}

static void
write_externs(void)
{
  int sptr, gblsym;
  INT nmptr;
  char typeptr[10], *ifacenm;
  LL_Type* llt;

  for (sptr = gbl.externs; sptr > NOSYM; sptr = SYMLKG(sptr)) {
    /* upper.c will place internal procedures on this list since
     * unifed.c needs to see the internal procedures on this
     * list.
     */
    if (SCG(sptr) != SC_STATIC)
    {

      /* find an interface first */
      ifacenm = get_llvm_ifacenm(sptr);
      gblsym = find_ag(ifacenm);

      if (!gblsym) {
        gblsym = find_ag(get_llvm_name(sptr));
        if (!gblsym && REFG(sptr))
          gblsym = get_ag(sptr);
      }

      if (AG_TYPENMPTR(gblsym) == 0) {
        if (STYPEG(sptr) != ST_PROC) {
          llt = get_ftn_extern_lltype(sptr);
          nmptr = add_ag_name((char*)llt->str);
          AG_TYPENMPTR(gblsym) = nmptr;
          continue;
        }
        if (LLTYPE(sptr) && (LLTYPE(sptr)->data_type == LL_VOID)) {
          nmptr = add_ag_name(
              (char *)char_type(get_return_dtype(DT_NONE, NULL, 0), 0));
          AG_TYPENMPTR(gblsym) = nmptr;
        } else if (get_return_type(sptr) == 0) {
          nmptr = add_ag_name(
              (char *)char_type(get_return_dtype(DT_NONE, NULL, 0), 0));
          AG_TYPENMPTR(gblsym) = nmptr;
        } else if (CFUNCG(sptr) && LLTYPE(sptr) && STYPEG(sptr) == ST_PROC) {
          write_ftn_type(LLTYPE(sptr), typeptr, 0);
          nmptr = add_ag_name(typeptr);
          AG_TYPENMPTR(gblsym) = nmptr;
          /* Use the following else-if once we rely on better stb data for
           * CFUNC return values. This includes enabling GARGRET:
           *
           * else if (CFUNCG(sptr) && STYPEG(sptr) == ST_PROC) {
           *  llt = make_lltype_from_dtype(DTYPEG(sptr));
           *  assert(llt && llt->alt_type, "write_externs: Invalid LL_Type",
           * sptr, 4);
           *  AG_TYPENMPTR(gblsym) = add_ag_name((char *)llt->alt_type->str);
           */
        } else {
          nmptr = add_ag_name((char *)char_type(
              get_return_dtype(DTYPEG(sptr), NULL, 0), 0));
          AG_TYPENMPTR(gblsym) = nmptr;
        }
      }
    }
  }
  for (sptr = gbl.basevars; sptr > NOSYM; sptr = SYMLKG(sptr))
    (void)get_ag(sptr);
}

/**
   \brief Read thru Data Initialization File and ...
 */
static void
dinits(void)
{
  DREC *p;
  int tdtype;
  ISZ_T tconval;
  int sptr, sectionindex = DATA_SEC;
  DSRT *dsrtp;
  DSRT *item;
  DSRT *prev;

  int save_funccount = gbl.func_count;

  lcl_inits = NULL;
  section_inits = NULL;
  extern_inits = NULL;

  while ((p = dinit_read())) {
    tdtype = p->dtype;
    tconval = p->conval;
    if (tdtype != DINIT_LOC && tdtype != DINIT_SLOC) {
      if (tdtype == DINIT_STRING) {
        /* skip over the string */
        dinit_fskip(tconval);
      } else if (tdtype == DINIT_SECT) {
        sectionindex = tconval;
      } else if (tdtype == DINIT_DATASECT) {
        sectionindex = DATA_SEC;
#ifdef DINIT_FUNCCOUNT
      } else if (tdtype == DINIT_FUNCCOUNT) {
        gbl.func_count = tconval;
#endif
      }
      continue;
    }
    sptr = tconval;
#if DEBUG
    assert(sptr > 0, "dinits:bad sptr", sptr, 3);
#endif
    if (SCG(sptr) == SC_CMBLK) {
      int cmblk;
#if DEBUG
      assert(DINITG(sptr), "assem.dinits cmblk DINIT flag 0", sptr, 3);
#endif
      item = GET_DSRT;
      item->sptr = sptr;
      item->offset = ADDRESSG(sptr);
      item->filepos = dinit_ftell();
      item->sectionindex = sectionindex;
      item->func_count = gbl.func_count;
      p = dinit_read();
      /*
       * if next dinit record is an offset, then the offset applies
       * to this symbol; update the the item's offset and file
       * position.  NOTE that this does not interfere with the
       * remaining dinit_read since records are skipped until we
       * get to the next LOC (or eof).
       */
      if (p->dtype == DINIT_OFFSET) {
        item->offset += p->conval;
        item->filepos = dinit_ftell();
      }
      cmblk = MIDNUMG(sptr);
#if DEBUG
      assert(STYPEG(cmblk) == ST_CMBLK, "assem.dinits NOT ST_CMBLK", sptr, 3);
#endif
      prev = NULL;
      dsrtp = DSRTG(cmblk);
      if (dsrtp && dsrtp->ladd->offset < item->offset) {
        dsrtp = dsrtp->ladd;
      }
      for (; dsrtp; dsrtp = dsrtp->next) {
        if (dsrtp->offset > item->offset)
          break;
        if (dsrtp->offset == item->offset) {
          /* check for zero-sized object */
          if (size_of(DTYPEG(sptr)) != 0 && size_of(DTYPEG(dsrtp->sptr)) != 0) {
            error(164, 2, 0, SYMNAME(sptr), CNULL);
            goto Continue;
          }
        }
        prev = dsrtp;
      }
      if (prev == NULL) {
        item->next = DSRTG(cmblk);
        DSRTP(cmblk, item);
      } else {
        item->next = prev->next;
        prev->next = item;
      }
      DSRTG(cmblk)->ladd = item;
    }
    else if (SECTG(sptr)) {
      /* initialized variable in a named section */
      item = GET_DSRT;
      item->sptr = sptr;
      item->offset = ADDRESSG(sptr);
      item->filepos = dinit_ftell();
      item->sectionindex = sectionindex;
      item->func_count = gbl.func_count;
      prev = NULL;
      for (dsrtp = section_inits; dsrtp; dsrtp = dsrtp->next)
        prev = dsrtp;
      if (prev == NULL) {
        item->next = section_inits;
        section_inits = item;
      } else {
        item->next = prev->next;
        prev->next = item;
      }
    }
    else if (REFG(sptr) && !CFUNCG(sptr)) {
      /* ref'd local var */
      item = GET_DSRT;
      item->sptr = sptr;
      item->offset = ADDRESSG(sptr);
      item->filepos = dinit_ftell();
      item->sectionindex = sectionindex;
      item->func_count = gbl.func_count;
      p = dinit_read();

      /*
       * if next dinit record is an offset, then the offset applies
       * to this symbol; update the the item's offset and file
       * position.  NOTE that this does not interfere with the
       * remaining dinit_read since records are skipped until we
       * get to the next LOC (or eof).
       */
      if (p->dtype == DINIT_OFFSET) {
        item->offset += p->conval;
        item->filepos = dinit_ftell();
      }
      prev = NULL;
      for (dsrtp = lcl_inits; dsrtp; dsrtp = dsrtp->next) {
        if (dsrtp->offset > item->offset)
          break;
        if (dsrtp->offset == item->offset) {
          int sptr = dsrtp->sptr;
          if (sptr && DTY(DTYPEG(sptr)) == TY_ARRAY && SCG(sptr) == SC_STATIC &&
              extent_of(DTYPEG(sptr)) == 0)
            goto Continue;
          error(164, 2, 0, SYMNAME(sptr), CNULL);
          goto Continue;
        }
        prev = dsrtp;
      }
      if (prev == NULL) {
        item->next = lcl_inits;
        lcl_inits = item;
      } else {
        item->next = prev->next;
        prev->next = item;
      }

    } else if (CFUNCG(sptr)) {
      /* inited BIND(C) module variable */
      item = GET_DSRT;
      item->sptr = sptr;
      item->offset = ADDRESSG(sptr);
      item->sectionindex = sectionindex;
      item->filepos = dinit_ftell();
      item->func_count = gbl.func_count;

      p = dinit_read();
      /*
       * if next dinit record is an offset, then the offset applies
       * to this symbol; update the the item's offset and file
       * position.  NOTE that this does not interfere with the
       * remaining dinit_read since records are skipped until we
       * get to the next LOC (or eof).
       */
      if (p->dtype == DINIT_OFFSET) {
        item->offset += p->conval;
        item->filepos = dinit_ftell();
      }

      prev = NULL;
      for (dsrtp = extern_inits; dsrtp; dsrtp = dsrtp->next) {
        if (sptr != dsrtp->sptr)
          break;
        if (dsrtp->offset > item->offset)
          break;
        prev = dsrtp;
      }
      if (prev == NULL) {
        item->next = extern_inits;
        extern_inits = item;
      } else {
        item->next = prev->next;
        prev->next = item;
      }
    }
  Continue:;
    /* we may have read ahead to another dinit record, check if it's a STRING */
    if (p->dtype == DINIT_STRING) {
      /* skip over the string */
      dinit_fskip(p->conval);
    }
  }

  gbl.func_count = save_funccount;
} /* endroutine dinits */

extern DINIT_REC *dsrtbase, *dsrtend, *dsrtfree;

static int 
align_dir_value(b) int b; /* 'b'-byte boundary */
{
  int j, i;
  if (XBIT(119, 0x10)) {/* linux */
    for (j = 1, i = 0; j < b; j *= 2, ++i)
      ;
    return i;
  }
  return b;
}

extern void 
assem_emit_align(int n) /* 'n'-byte alignment */
{
  int i = align_dir_value(n);
  if (i)
    fprintf(ASMFIL, "\t.align\t%d\n", i);
}

void
put_section(int sect)
{
}

int
get_hollerith_size(int sptr)
{
  int add_null = 0;
  if (HOLLG(sptr)) {
    int len = DTY(DTYPEG(sptr) + 1);
    if (flg.quad && len >= MIN_ALIGN_SIZE) {
      add_null = ALIGN(len, DATA_ALIGN) - len;
    } else {
      add_null = ALIGN(len, alignment(DT_INT)) - len;
    }
    return add_null;
  }
  return DTY(DTYPEG(sptr) + 1);
}

/**
   \param sptr is a Fortran character constant or Hollerith constant.
   \param add_null is 1 if null character is added, otherwise 0.
 */
void
put_fstr(int sptr, int add_null)
{
  const char *retc = (char *)char_type(DTYPEG(sptr), sptr);
  int len = 0;

#ifdef HOLLG
  if (HOLLG(sptr)) {
    len = get_hollerith_size(sptr);
  }
#endif
  fprintf(ASMFIL, "@%s = internal constant %s [", get_llvm_name(sptr), retc);
  put_string_n(stb.n_base + CONVAL1G(sptr), DTY(DTYPEG(sptr) + 1) + add_null,
               0);
#ifdef HOLLG
  if (HOLLG(sptr)) {
    while (len) {
      fputc(',', ASMFIL);
      put_string_n("               ", 1, 0);
      --len;
    }
  }
#endif
  fputc(']', ASMFIL);
}

static void
put_kstr(int sptr, int add_null)
/*  put out data initializations for kanji string (2 bytes/char)  */
{
  unsigned char *p;
  const char *retc;
  int len;
  int bytes;

  retc = char_type(DTYPEG(sptr), sptr);
  fprintf(ASMFIL, "@%s = internal constant %s [", get_llvm_name(sptr), retc);

  sptr = CONVAL1G(sptr);
  assert(STYPEG(sptr) == ST_CONST && DTY(DTYPEG(sptr)) == TY_CHAR,
         "assem/put_kstr(): bad sptr", sptr, 3);

  len = DTY(DTYPEG(sptr) + 1);
  p = (unsigned char *)stb.n_base + CONVAL1G(sptr);
  while (len > 0) {
    int val = kanji_char(p, len, &bytes);

    p += bytes;
    len -= bytes;

    fprintf(ASMFIL, "i16 %d", val);
    if (len)
      fprintf(ASMFIL, ",");
  }
  fputc(']', ASMFIL);
}

/* from scc assem.c : */

/*
 * return the maximum alignment suitable for the symbol
 * with respect to its size.
 *
 */
static int
max_align(int sptr)
{
  int dtype;
  ISZ_T sz;
  int align;

  dtype = DTYPEG(sptr);
  sz = size_of_sym(sptr);
  if (!PDALN_IS_DEFAULT(sptr)) {
    align = (1 << PDALNG(sptr)) - 1;
  } else if (sz > max_cm_align) {
    align = max_cm_align;
  } else if (sz >= MIN_ALIGN_SIZE) {
    align = DATA_ALIGN;
  } else {
    align = align_unconstrained(dtype);
  }
  return align;
}

#if DEBUG
/* Dump an entry in the AG table */
static void
dump_gblsym(int gblsym)
{
  printf("gblsym:%d, %s, %s, typedesc:%d\n", gblsym, AG_NAME(gblsym),
         AG_TYPENMPTR(gblsym) ? AG_TYPENAME(gblsym) : "N/A",
         AG_TYPEDESC(gblsym));
}

/* Dump the AG table, TODO: Add to coding.n for DBGBIT and gbl.dbgfil */
static void
dump_ag(void)
{
  int i;
  for (i = 0; i < agb.s_avl; ++i)
    if (AG_HASHLK(i))
      dump_gblsym(i);
}

static void
dump_allag(void)
{
  int i;
  for (i = 0; i < agb.s_avl; ++i)
    dump_gblsym(i);
}
#endif /* Debug */

/*
 * return ptr to assem's global symtab.
 */

int
get_ag(int sptr)
{
  int gblsym, stype;
  char *ag_name;
  ISZ_T size;

  stype = STYPEG(sptr);
  if (gbl.internal == 1 && gbl.rutype == RU_PROG && sptr == gbl.currsub)
    ag_name = get_main_progname();
  else
    ag_name = get_llvm_name(sptr);
  gblsym = find_ag(ag_name);

  if (gblsym)
    goto Found;

  /* Enter new symbol into the global symbol table */
  gblsym = make_gblsym(sptr, ag_name);
  if (CLASSG(sptr) && DESCARRAYG(sptr)) {
    /* add type descriptor to global list */
    char tdtname[MXIDLN];
    AG_SYMLK(gblsym) = ag_global;
    ag_global = gblsym;
    AG_SIZE(gblsym) = 0;
    AG_TYPEDESC(gblsym) = 1; /* This is a type descriptor */
    AG_DEFD(gblsym) = 0;

    /* Default value used for when we have an external reference to
     * a type descriptor in assemble_end().
     */
    sprintf(tdtname, "struct%s", ag_name);
    add_ag_typename(gblsym, tdtname);
  } else
      if (stype == ST_CMBLK) {
    AG_SYMLK(gblsym) = ag_cmblks;
    ag_cmblks = gblsym;
    AG_SIZE(gblsym) = SIZEG(sptr);
    AG_ALLOC(gblsym) = ALLOCG(sptr);
#if defined(TARGET_WIN)
    AG_DLL(gblsym) = DLLG(sptr);
#endif
    if (!MODCMNG(sptr) || DEFDG(sptr))
      AG_DEFD(gblsym) = 1;
    if (FROMMODG(sptr) && MODCMNG(sptr)) {
      /* set flag to emit an external reference */
      AG_ISMOD(gblsym) = 1;
    }
#if defined(TARGET_WIN)
    /* windows hack (see f19172) - for now, mark all module commmons as
     * defined; need to solve having non-dll/dll versions of a .mod file.
     */
    AG_DEFD(gblsym) = 1;
#endif
    if (!XBIT(57, 0x10000000) && CCSYMG(sptr) && PDALNG(sptr) == 4) {
      AG_ALIGN(gblsym) = max_cm_align + 1;
    }
  } else if ((stype == ST_ARRAY) & !CFUNCG(sptr)) {
    AG_SYMLK(gblsym) = ag_other;
    ag_other = gblsym;
    AG_SIZE(gblsym) = size_of(DTYPEG(sptr));
  }
  else if (stype == ST_BASE) {
    /* base address symbol */
    AG_SYMLK(gblsym) = ag_global;
    ag_global = gblsym;
    AG_SIZE(gblsym) = 0;
  }
  else if ((stype == ST_VAR) || (stype == ST_STRUCT) || (stype == ST_ARRAY)) {
    /* CFUNCG() : BIND(C) module variables visible
       externally
     */

    if (!CFUNCG(sptr))
      return 0;

    AG_SYMLK(gblsym) = ag_cmblks;
    ag_cmblks = gblsym;
    AG_SIZE(gblsym) = size_of_sym(sptr);
    AG_ALIGN(gblsym) = max_align(sptr) + 1;

    if (DINITG(sptr))
      AG_DSIZE(gblsym) = size_of_sym(sptr);

    AG_ALLOC(gblsym) = 0;
    AG_DEFD(gblsym) = 1;
  }

  else
#ifdef CUDAG
      if (!(CUDAG(sptr) & CUDA_BUILTIN))
#endif
  {
    /*  NOTE: ST_ENTRY and ST_PROC added to the same list */
    AG_SYMLK(gblsym) = ag_procs;
    ag_procs = gblsym;

    if (stype == ST_PROC) {
      /* check for iface */
      int dtype = DTYPEG(sptr);
      if (DTY(dtype) == TY_PROC && DTY(dtype + 2) == sptr) {
        AG_ISIFACE(gblsym) = 1; /* check this when datatype is processed. */
        AG_SIZE(gblsym) = 0;
        AG_DEVICE(gblsym) = 0;
#if defined(TARGET_WIN)
        AG_DLL(gblsym) = DLLG(sptr);
#endif
        return gblsym;
      }
    }
    if (stype == ST_ENTRY) {
      AG_SIZE(gblsym) = 1; /* subprogram defined in file */
      if (SCG(sptr) != SC_STATIC) {
        global_sptr = gblsym;
        llvm_set_unique_sym(gblsym);
      }
    } else {
      AG_SIZE(gblsym) = 0;
      AG_DEVICE(gblsym) = 0;
#ifdef CUDAG
      if (CUDAG(sptr) == CUDA_DEVICE || (CUDAG(sptr) & CUDA_GLOBAL))
        AG_DEVICE(gblsym) = 1;
      if (CUDAG(gbl.currsub) == CUDA_DEVICE ||
          (CUDAG(gbl.currsub) & CUDA_GLOBAL))
        AG_DEVICE(gblsym) = 1;
#endif
      if (NEEDMODG(sptr)) {
        AG_ISMOD(gblsym) = 1;
#if defined(TARGET_WIN)
        if (TYPDG(sptr)) {
          AG_REF(gblsym) = 1;
          AG_NEEDMOD(gblsym) = 1;
        }
#else
        AG_REF(gblsym) = 1;
        if (TYPDG(sptr))
          AG_NEEDMOD(gblsym) = 1;
#endif
      } else if (REFG(sptr))
        AG_REF(gblsym) = SCG(sptr) != SC_NONE;
    }
#if defined(TARGET_WIN)
    AG_DLL(gblsym) = DLLG(sptr);
#endif
  }
  return gblsym;

Found:
  if (CLASSG(sptr) && DESCARRAYG(sptr)) {
    return 0;
  }
  switch (stype) {
  case ST_PROC:
  case ST_ENTRY:
    if (AG_STYPE(gblsym) == ST_CMBLK) {
      error(166, 3, 0, SYMNAME(sptr), CNULL);
      return 0;
    }
    /* if a ST_PROC and ST_ENTRY occur in the same file, make sure
     * that the symbol is recorded as ST_ENTRY.
     */
    if (stype == ST_ENTRY) {
      AG_STYPE(gblsym) = ST_ENTRY;
      if (SCG(sptr) != SC_STATIC) {
        global_sptr = gblsym;
        llvm_set_unique_sym(gblsym);
      }
      AG_SIZE(gblsym) = 1;
    } else if (REFG(sptr))
      AG_REF(gblsym) |= SCG(sptr) != SC_NONE;
    break;
  case ST_ARRAY:
    /*
     * an array declared in a module declared as visable to c
     * with BIND(C) : marked CFUNCG()
     */
    if (!CFUNCG(sptr))
      break;
  /* else fall through */
  case ST_VAR:
  case ST_STRUCT:
    if (!CFUNCG(sptr))
      return 0;
  /* fall through */
  case ST_CMBLK:
    if (AG_STYPE(gblsym) != stype) {
      error(166, 3, 0, SYMNAME(sptr), CNULL);
      return 0;
    }
    size = SIZEG(sptr);
    if (DINITG(sptr)) {
      /* common block is init'd in subprogram */
      if (AG_DSIZE(gblsym))
        ; /* already dinit'd */
      else {
        if (size < AG_SIZE(gblsym))
          /* dinit size < previous size */
          error(168, 3, 0, SYMNAME(sptr), CNULL);
        AG_SIZE(gblsym) = size;
      }
      AG_DEFD(gblsym) = 1;
    } else if (AG_DSIZE(gblsym) && AG_DSIZE(gblsym) < size)
      /* prev dinit size < size */
      error(155, 3, 0, "Same name common blocks with different sizes in same "
                       "file not supported",
            "");
    else if (AG_SIZE(gblsym) < size) {
      AG_SIZE(gblsym) = size;
    }
    if (!MODCMNG(sptr) || DEFDG(sptr))
      AG_DEFD(gblsym) = 1;
#if defined(TARGET_WIN)
    AG_DEFD(gblsym) = 1;
/* windows hack (see f19172) - for now, mark all module commmons as
 * defined; need to solve having non-dll/dll versions of a .mod file.
 */
#endif
    break;
  case ST_BASE:
    break;
  default:
    interr("assem get_ag, bad stype of ", sptr, 3);
  }

  return gblsym;
}

LOGICAL
has_typedef_ag(int gblsym)
{
  return AG_TYPENMPTR(gblsym) > 0;
}

void
set_ag_lltype(int gblsym, LL_Type *llt)
{
  assert(gblsym, "set_ag_lltype: Invalid gblsym", gblsym, 4);
  AG_LLTYPE(gblsym) = llt;
}

LL_Type *
get_ag_lltype(int gblsym)
{
#if DEBUG
  if (!AG_LLTYPE(gblsym)) {
    char bf[100];
    sprintf(bf, "get_ag_lltype: No LLTYPE set for gblsym %s", AG_NAME(gblsym));
    interr(bf, gblsym, 4);
  }
#endif
  return AG_LLTYPE(gblsym);
}

void
set_ag_return_lltype(int gblsym, LL_Type *llt)
{
  assert(gblsym, "set_ag_return_lltype: Invalid gblsym", gblsym, 4);
  AG_RET_LLTYPE(gblsym) = llt;
}

LL_Type *
get_ag_return_lltype(int gblsym)
{
  assert(gblsym, "get_ag_return_lltype: Invalid gblsym", gblsym, 4);
  return AG_RET_LLTYPE(gblsym);
}

static int
find_local_ag(char *ag_name)
{
  int gsym, hashval = name_to_hash(ag_name, strlen(ag_name));

  for (gsym = agb_local.hashtb[hashval]; gsym; gsym = AGL_HASHLK(gsym))
    if (!strcmp(ag_name, AGL_NAME(gsym)))
      return gsym;
  return 0;
}

static int
add_ag_fptr_name(char *ag_name)
{
  int i, nptr, len, needed;
  char *np;

  len = strlen(ag_name);
  nptr = fptr_local.n_avl;
  fptr_local.n_avl += (len + 1);

  if ((len + 1) >= (32 * 16))
    needed = len + 1;
  else
    needed = 32 * 16;

  NEED(fptr_local.n_avl + 1, fptr_local.n_base, char, fptr_local.n_size,
       fptr_local.n_size + needed);
  np = fptr_local.n_base + nptr;
  for (i = 0; i < len; i++)
    *np++ = *ag_name++;
  *np = '\0';

  return nptr;
}

#if defined(TARGET_WIN)
void
dllexport_mod(int modu)
{
  int gg;
  gg = get_ag(modu);
  if (gg && AG_STYPE(gg) != ST_ENTRY) {
    AG_STYPE(gg) = ST_ENTRY;
    AG_DLL(gg) = DLL_EXPORT;
  }
}
#endif

/*
 * return external name for procedure
 */
char *
getextfuncname(int sptr)
{
  static char name[MXIDLN]; /* 1 for null, 3 for extra '_' , */
  char *p, *q, ch;
  LOGICAL has_underscore = 0;
  int stype, m;
  stype = STYPEG(sptr);
  if (ALTNAMEG(sptr)) {
    return get_altname(sptr);
  }
  if (gbl.internal && CONTAINEDG(sptr)) {
    p = name;
    m = INMODULEG(gbl.outersub);
    if (m) {
      q = SYMNAME(m);
      while ((ch = *q++)) {
        if (ch == '$')
          *p++ = flg.dollar;
        else
          *p++ = ch;
      }
      *p++ = '_';
    }
    q = SYMNAME(gbl.outersub);
    while ((ch = *q++)) {
      if (ch == '$')
        *p++ = flg.dollar;
      else
        *p++ = ch;
    }
    *p++ = '_';
    q = SYMNAME(sptr);
    while ((ch = *q++)) {
      if (ch == '$')
        *p++ = flg.dollar;
      else
        *p++ = ch;
    }
    *p = '\0';
    return name;
  }
  if (XBIT(119, 0x1000)) { /* add leading underscore */
    name[0] = '_';
    p = name + 1;
  } else
    p = name;
  m = INMODULEG(sptr);
  if (m) {
    q = SYMNAME(m);
    while ((ch = *q++)) {
      if (ch == '$')
        *p++ = flg.dollar;
      else
        *p++ = ch;
    }
    *p++ = '_';
  }
  if (stype != ST_ENTRY || gbl.rutype != RU_PROG) {
    q = SYMNAME(sptr);
  } else {
#if defined(TARGET_WIN)
    /* we have a mix of undecorated and decorated names on win32 */
    strcpy(name, "MAIN_");
    return name;
#else
    q = "MAIN";
#endif
  }
  while ((ch = *q++)) {
    if (ch == '$')
      *p++ = flg.dollar;
    else
      *p++ = ch;
    if (ch == '_')
      has_underscore = TRUE;
  }
/*
 * append underscore to name??? -
 * - always for entry,
 * - procedure if not compiler-created and not a "C" external..
 * - modified by -x 119 0x0100000 or -x 119 0x02000000
 */
  if (stype != ST_PROC || (!CCSYMG(sptr) && !CFUNCG(sptr))) {
    /* functions marked as !DEC$ ATTRIBUTES C get no underbar */
    if (!XBIT(119, 0x01000000) && !CFUNCG(sptr) && !CREFG(sptr)) {
      *p++ = '_';
      if (XBIT(119, 0x02000000) && has_underscore)
        *p++ = '_';
    }
  }
  *p = '\0';
  return name;
} /* getextfuncname */

static char *
getfuncname(int sptr)
{
  if (!sptr)
    return "xxxxxx";
  if (gbl.outlined || ISTASKDUPG(GBL_CURRFUNC))
    return SYMNAME(sptr);
  return getextfuncname(sptr);
}

/*
 * return ptr to symbol name, suitable for assembly code listing. For
 * strings and constants, a name must be created:
 *
 * BIG FAT WARNING: This routine formats the name into a static buffer
 * whose address is returned.  Don't capture this result and reuse
 * the string in any context where getsname() might be called again,
 * because the buffer will be overwritten with a new name!
 */
char *
getsname(int sptr)
{
  static char name[MXIDLN]; /* 1 for null, 3 for extra '_' ,
                             * 4 for @### with mscall
                             */
  char *p, *q, ch;
  LOGICAL has_underscore = 0;
  int stype, m;
  char *prepend = "\0";

  switch (stype = STYPEG(sptr)) {
  case ST_LABEL:
    sprintf(name, "%sB%d_%d", ULABPFX, gbl.func_count, sptr);
    break;
  case ST_CONST:
  case ST_PARAM:
      sprintf(name, ".C%d_%s", sptr, getfuncname(gbl.currsub));
    break;
  case ST_BASE:
    return SYMNAME(sptr);
  case ST_VAR:
  case ST_ARRAY:
  case ST_STRUCT:
  case ST_UNION:
  case ST_PLIST:
    switch (SCG(sptr)) {
    case SC_EXTERN:
      if (ALTNAMEG(sptr) && CFUNCG(sptr))
        return get_altname(sptr);
      goto xlate_name;
    case SC_CMBLK:
      if (ALTNAMEG(sptr)) {
        return get_altname(sptr);
      }
      /* modification needed on this name ? */
      if (CFUNCG(sptr))
        return SYMNAME(sptr);
      return getsname((int)MIDNUMG(sptr));
    case SC_STATIC:
      if (CLASSG(sptr) && DESCARRAYG(sptr))
        goto xlate_name;
#ifdef BASEADDRG
      if (BASEADDRG(sptr)) {
        return SYMNAME(BASESYMG(sptr));
      }
#endif
      if (ALTNAMEG(sptr))
        return get_altname(sptr);
      if (UPLEVELG(sptr) || (gbl.outlined && gbl.internal <= 1)) {
        if (DINITG(sptr)) {
          if (ENCLFUNCG(sptr) && ENCLFUNCG(sptr) == gbl.currsub)
            return static_name;
          return outer_static_name;
        }
        return outer_bss_name;
      }
      if (SECTG(sptr)) {
#ifdef CUDAG
        if (gbl.currsub && (CUDAG(gbl.currsub) & CUDA_CONSTRUCTOR)) {
          if (global_sptr) { /* prepend a module or routine name defined in this
                                file */
            prepend = AG_NAME(global_sptr);
          }
        }
#endif
        sprintf(name, ".SECTION%d_%d_%s", gbl.func_count, sptr, prepend);
        return name;
      }
      if (ALTNAMEG(sptr)) {
        return get_altname(sptr);
      }
      if (DINITG(sptr)) {
        if (static_name_global == 1) {
          /* zero sized array reference, use BSS instead of STATICS */
          if ((DTY(DTYPEG(sptr)) == TY_ARRAY) && SCG(sptr) == SC_STATIC &&
              extent_of(DTYPEG(sptr)) == 0) {
            bss_name_global = 2;
            SYMLKP(bss_base, gbl.basevars);
            gbl.basevars = bss_base;
            ADDRESSP(sptr, gbl.bss_addr);
            if (gbl.bss_addr == 0)
              gbl.bss_addr = 4;
          } else {
            static_name_global = 2;
            SYMLKP(static_base, gbl.basevars);
            gbl.basevars = static_base;
          }
        }
        /* zero sized array reference, use BSS instead of STATICS */
        if ((DTY(DTYPEG(sptr)) == TY_ARRAY) && SCG(sptr) == SC_STATIC &&
            extent_of(DTYPEG(sptr)) == 0) {

          ADDRESSP(sptr, gbl.bss_addr);
          if (gbl.bss_addr == 0)
            gbl.bss_addr = 4;
          return bss_name;
        }
        if (gbl.outlined)
          return outer_static_name;
        return static_name;
      }
      if (bss_name_global == 1) {
        /* make sure the bss_name gets output */
        bss_name_global = 2;
        SYMLKP(bss_base, gbl.basevars);
        gbl.basevars = bss_base;
      }
      return bss_name;
    case SC_PRIVATE:
      sprintf(name, "%s_%d", SYMNAME(sptr), sptr);
      return name;
    default:
      sprintf(name, ".V%d_%d", gbl.func_count, sptr);
    }
    break;
  case ST_CMBLK:
#if defined(TARGET_OSX)
    if (FROMMODG(sptr)) { /* common block is from a module */
      int md;
      md = SCOPEG(sptr);
      if (md && NEEDMODG(md)) {
        /*  module is use-associated */
        TYPDP(md, 1);
      }
    }
#endif
    if (ALTNAMEG(sptr))
      return (get_altname(sptr));
    if
      CFUNCG(sptr)
      {
        /* common block C name compatibility : no underscore */
        return SYMNAME(sptr);
      }

  xlate_name:
    if (XBIT(119, 0x1000)) { /* add leading underscore */
      name[0] = '_';
      p = name + 1;
    } else
      p = name;
    q = SYMNAME(sptr);
    while ((ch = *q++)) {
      if (ch == '$')
        *p++ = flg.dollar;
      else
        *p++ = ch;
      if (ch == '_')
        has_underscore = TRUE;
    }
/*
 * append underscore to name??? -
 * - always for common block (note - common block may have CCSYM set),
 * - not compiler-created external variable,
 * - modified by -x 119 0x0100000 or -x 119 0x02000000
 */
    if ((STYPEG(sptr) == ST_CMBLK || !CCSYMG(sptr)) && !CFUNCG(sptr)) {
      if (!XBIT(119, 0x01000000)) {
        *p++ = '_';
        if (XBIT(119, 0x02000000) && has_underscore && !CCSYMG(sptr))
          *p++ = '_';
      }
    }
    *p = '\0';
#if defined(TARGET_WIN)
    if (!XBIT(121, 0x200000) && STYPEG(sptr) == ST_CMBLK && !CCSYMG(sptr) &&
        XBIT(119, 0x01000000))
      upcase_name(name);
#endif
    break;
  case ST_ENTRY:
  case ST_PROC:
    if (ALTNAMEG(sptr)) {
      return get_altname(sptr);
    }
    if ((flg.smp || XBIT(34, 0x200)) && OUTLINEDG(sptr)) {
      sprintf(name, "%s", SYMNAME(sptr));
      p = name;
    }
    else if (gbl.internal && CONTAINEDG(sptr)) {
      p = name;
      if (gbl.outersub) {
        m = INMODULEG(gbl.outersub);
        if (m) {
          q = SYMNAME(m);
          while ((ch = *q++)) {
            if (ch == '$')
              *p++ = flg.dollar;
            else
              *p++ = ch;
          }
          *p++ = '_';
        }
        q = SYMNAME(gbl.outersub);
        while ((ch = *q++)) {
          if (ch == '$')
            *p++ = flg.dollar;
          else
            *p++ = ch;
        }
        *p++ = '_';
      }
      q = SYMNAME(sptr);
      while ((ch = *q++)) {
        if (ch == '$')
          *p++ = flg.dollar;
        else
          *p++ = ch;
      }
      *p = '\0';
      return name;
    }
    if (XBIT(119, 0x1000)) { /* add leading underscore */
      name[0] = '_';
      p = name + 1;
    } else
      p = name;
    m = INMODULEG(sptr);
    if (m) {
      q = SYMNAME(m);
      while ((ch = *q++)) {
        if (ch == '$')
          *p++ = flg.dollar;
        else
          *p++ = ch;
      }
      *p++ = '_';
    }
    if (stype != ST_ENTRY || gbl.rutype != RU_PROG) {
      q = SYMNAME(sptr);
    } else if ((flg.smp || XBIT(34, 0x200)) && OUTLINEDG(sptr)) {
      q = SYMNAME(sptr);
    } else {
#if defined(TARGET_WIN)
      /* we have a mix of undecorated and decorated names on win32 */
      strcpy(name, "MAIN_");
      return name;
#else
      q = "MAIN";
#endif
    }
    while ((ch = *q++)) {
      if (ch == '$')
        *p++ = flg.dollar;
      else
        *p++ = ch;
      if (ch == '_')
        has_underscore = TRUE;
    }
/*
 * append underscore to name??? -
 * - always for entry,
 * - procedure if not compiler-created and not a "C" external..
 * - modified by -x 119 0x0100000 or -x 119 0x02000000
 */
    if (stype != ST_PROC || (!CCSYMG(sptr) && !CFUNCG(sptr))) {
      /* functions marked as !DEC$ ATTRIBUTES C get no underbar */
      if (!XBIT(119, 0x01000000) && !CFUNCG(sptr) && !CREFG(sptr) &&
          !CONTAINEDG(sptr)) {
        *p++ = '_';
        if (XBIT(119, 0x02000000) && has_underscore)
          *p++ = '_';
      }
    }
    *p = '\0';
    if (MSCALLG(sptr) && !CFUNCG(sptr) && !XBIT(119, 0x4000000)) {
      if (ARGSIZEG(sptr) == -1)
        sprintf(name, "%s@0", name);
      else if (ARGSIZEG(sptr) > 0) {
        sprintf(name, "%s@%d", name, ARGSIZEG(sptr));
      }
    }
    if (!XBIT(121, 0x200000) &&
        ((MSCALLG(sptr) && !STDCALLG(sptr)) ||
         (CREFG(sptr) && !CFUNCG(sptr) && !CCSYMG(sptr))))
      /* if WINNT calling conventions are used, the name must be
       * uppercase unless the subprogram has the STDCALL attribute.
       * All cref intrinsic are lowercase.
       */
      upcase_name(name);

    break;
  default:
    interr("getsname: bad stype for", sptr, 3);
    strcpy(name, "b??");
  }
  return name;
}

static void
upcase_name(char *name)
{
  char *p;
  int ch;
  for (p = name; (ch = *p); p++)
    if (ch >= 'a' && ch <= 'z')
      *p = ch + ('A' - 'a');
}

char *
get_main_progname(void)
{
  static char name[MXIDLN];
  char *nm = SYMNAME(gbl.currsub);
  sprintf(name, "%s", nm);
  if (!XBIT(119, 0x01000000)) {
    strcat(name, "_");
  }
  return name;
}

static void assn_stkoff(int, int, ISZ_T);
static void assn_static_off(int, int, ISZ_T);

static void
set_ag_ref(int sptr)
{
  int gblsym;
  char *ifacenm;
  if (gbl.currsub)
    ifacenm = get_llvm_ifacenm(sptr);
  else
    ifacenm = get_llvm_name(sptr);
  gblsym = find_ag(ifacenm);
  if (gblsym) {
    AG_REF(gblsym) = 1;
  }
}

void
sym_is_refd(int sptr)
{
  ISZ_T size;
  DTYPE dtype = DTYPEG(sptr);
  int stype = STYPEG(sptr);

  switch (stype) {
  case ST_PLIST:
  case ST_VAR:
  case ST_ARRAY:
  case ST_STRUCT:
  case ST_UNION:
    if (REFG(sptr))
      break;
    switch (SCG(sptr)) {
    case SC_DUMMY:

      if (!is_passbyval_dummy(sptr))
        arg_is_refd(sptr);
      break;
    case SC_LOCAL:
      /*
       * assign address to automatic variable: auto offsets are
       * negative relative to the frame pointer. the current size of
       * of the stack frame is saved as a positive value; the last
       * offset assigned is the negative of the current frame size.
       * The negative of the current frame size is aligned so that the
       * variable ends on this boundary.  The offset assigned is this
       * value minus its size in bytes. The new size of the stack frame
       * is the negative of the offset.
       * ASSUMPTIONS:
       *     1.  the value frame pointer is an address whose alignment
       *         matches that of the scalar item having the most strict
       *         requrement.
       *     2.  there are not gaps between the address located by the
       *         frame pointer and the auto area (first offset is -1)
       */
      if (DINITG(sptr) || SAVEG(sptr) ||
          (!flg.recursive && (!CCSYMG(sptr) || INLNG(sptr)))) {
        /* can't put compiler-created symbols in static memory
         * until sched changes how it accesses its temporaries.
         * if it's a compiler-created symbol created by the
         * inliner, it's ok to place in static memory.
         */
        if (DINITG(sptr) || SAVEG(sptr) || STYPEG(sptr) != ST_VAR) {
          SCP(sptr, SC_STATIC);
          if (PARREFG(sptr))
            PARREFP(sptr, 0);
          if (!SAVEG(sptr) && !DINITG(sptr)) {
            if (!flg.smp && !XBIT(34, 0x200))
              LOCLIFETMP(sptr, 1);
          }
          goto static_shared;
        }
      }
      if (stype == ST_PLIST)
        size = PLLENG(sptr) * size_of(dtype);
      else
        size = size_of(dtype);
      /* For uplevel structure and ident_t in host subroutine(non outlined)
       * we set REFD field when we create it so that it does not gets here.
       * Because we don't want it to call assn_stkoff which will assign
       * negative addresses which may inadvertly cause it in create local
       * equivalence array.
       */
      if ((flg.smp || XBIT(34, 0x200)) && gbl.outlined)
        break;
      if (!SOCPTRG(sptr))
        break;
      assn_stkoff(sptr, dtype, size);
      break;
    case SC_STATIC:
      /*
        rhs structure constructure does not have DINITG or SAVED set
        To do list:
          We can create the type first so that we can reference to it and 
          then we can print out the shape later if we make BSS a structure.
          Currrently we make BSS array for easy declaration (no other reason)
          We can use the same scheme for .STATICS.
        if (!DINITG(sptr) && !SAVEG(sptr))
            break;
      */
      if ((CLASSG(sptr) && DESCARRAYG(sptr)) || SECTG(sptr)) {
        ADDRESSP(sptr, 0); /* type descriptor for poly variable */
        break;
      }
      if (ALTNAMEG(sptr)) {
        ADDRESSP(sptr, 0); /* C interface */
        break;
      }
    static_shared:
      if (stype == ST_PLIST)
        size = PLLENG(sptr) * size_of(dtype);
      else
        size = size_of(dtype);
      assn_static_off(sptr, dtype, size);
      /* All other dinit'd symbol should ready be ref'd in host routine.
       * This left acc symbols to be ref'd here or any other symbol that
       * is referenced in outlined function only.
       */
      if (gbl.outlined && DINITG(sptr) && CCSYMG(sptr)) {
        ENCLFUNCP(sptr, gbl.currsub);
      }
      break;
    case SC_CMBLK:
      break;
    case SC_EXTERN:
      if (CLASSG(sptr) && DESCARRAYG(sptr)) {
        ADDRESSP(sptr, 0); /* type descriptor for poly variable */
      }
      break;
    case SC_PRIVATE:
      if (stype == ST_PLIST)
        size = PLLENG(sptr) * size_of(dtype);
      else
        size = size_of(dtype);
      if (!((flg.quad && size >= MIN_ALIGN_SIZE) || QALNG(sptr)))
        align_unconstrained(dtype); // ???: sets dtypeutl.c#constrained
      break;
    case SC_NONE:
    default:
      break;
    }
    REFP(sptr, 1);
    break;

  case ST_PROC:
    /* for PGF90, all ST_PROCs are on the gbl.externs list already */
    if (REFG(sptr) == 0 && SCG(sptr) == SC_EXTERN) {
      REFP(sptr, 1);

      set_ag_ref(sptr);
    }
    break;
  case ST_CONST:
    SCP(sptr, SC_STATIC);
    if (SYMLKG(sptr) == 0) {
      SYMLKP(sptr, gbl.consts);
      gbl.consts = sptr;
      if (DTYPEG(sptr) == DT_ADDR && CONVAL1G(sptr))
        sym_is_refd((int)CONVAL1G(sptr));
    }
    break;

  case ST_ENTRY: /* (found on entry ili only) */
  case ST_LABEL:
    break;

  default:
    /*	interr("sym_is_refd:bad sty", sptr, 2);*/
    break;
  }

}

/**
 * For f90, the locals of a subprogram (the host) which contains internal
 * procedures must be allocated before generating code for the contained
 * procedures.  At this time, the compiler does not know what and how host
 * local variables are referenced by the contained procedures.  If we
 * don't allocate locals now, the cg may place local variables on the
 * stack, and at least two problems occur when the only reference is
 * from the internal procedure:
 * 1. a host local is initialized.
 * 2. a host local appears in a namelist group.
 */
void
hostsym_is_refd(int sptr)
{
  int dtype, stype;
  ISZ_T size;

  dtype = DTYPEG(sptr);
  switch (stype = STYPEG(sptr)) {
  case ST_PLIST:
  case ST_VAR:
  case ST_ARRAY:
  case ST_STRUCT:
  case ST_UNION:
    if (REFG(sptr))
      break;
    switch (SCG(sptr)) {
    case SC_LOCAL:
      /*
       * assign address to automatic variable: auto offsets are
       * negative relative to the frame pointer. the current size of
       * of the stack frame is saved as a positive value; the last
       * offset assigned is the negative of the current frame size.
       * The negative of the current frame size is aligned so that the
       * variable ends on this boundary.  The offset assigned is this
       * value minus its size in bytes. The new size of the stack frame
       * is the negative of the offset.
       * ASSUMPTIONS:
       *     1.  the value frame pointer is an address whose alignment
       *         matches that of the scalar item having the most strict
       *         requrement.
       *     2.  there are not gaps between the address located by the
       *         frame pointer and the auto area (first offset is -1)
       */
      if (DINITG(sptr) || SAVEG(sptr) ||
          (!flg.recursive && (!CCSYMG(sptr) || INLNG(sptr)))) {
        /* can't put compiler-created symbols in static memory
         * until sched changes how it accesses its temporaries.
         * if it's a compiler-created symbol created by the
         * inliner, it's ok to place in static memory.
         */
        SCP(sptr, SC_STATIC);
        if (PARREFG(sptr))
          PARREFP(sptr, 0);
        if (!SAVEG(sptr) && !DINITG(sptr)) {
          if (!flg.smp && !XBIT(34, 0x200))
            LOCLIFETMP(sptr, 1);
        }
        goto static_shared;
      }
      if (stype == ST_PLIST)
        size = PLLENG(sptr) * size_of(dtype);
      else {
        if (dtype == DT_ASSCHAR || dtype == DT_DEFERCHAR) {
          size = size_of(DT_PTR);
        } else
          size = size_of(dtype);
      }
      if (flg.smp && !SOCPTRG(sptr))
        break;
      assn_stkoff(sptr, dtype, size);
      break;
    case SC_STATIC:
      if (CLASSG(sptr) && DESCARRAYG(sptr)) {
        ADDRESSP(sptr, 0); /* type descriptor for poly variable */
        break;
      }
    static_shared:
      if (stype == ST_PLIST)
        size = PLLENG(sptr) * size_of(dtype);
      else
        size = size_of(dtype);
      assn_static_off(sptr, dtype, size);
      break;
    default:
      interr("hostsym_is_refd: bad sc\n", SCG(sptr), 3);
    }
    REFP(sptr, 1);
    break;

  default:
    interr("hostsym_is_refd:bad sty", sptr, 2);
  }

}

/**
   \brief Assign an address to a dummy argument which is allocated in the local
   area.

   It's assumed that the alignment and size requirements for each argument are
   those that are required for pointer-sized integer.
 */
void
arg_is_refd(int sptr)
{
  int dtype;
  INT size;

  if (!HOMEDG(sptr) || REFG(sptr))
    return;

  /* haven't homed or space has been alloc'ed */
  /* for now, get pointer-sized int allocation */
  dtype = DT_ADDR;
  size = size_of(dtype); /* is really ptr to */

  /* hack to avoid problems with zero-length strings.
   * make character*0 appear like character*1 */
  if (size == 0)
    size = 1;

  REFP(sptr, 1);
  HOMEDP(sptr, 0);

  /* sptr is the .cxxxx indirection temp; progagate information to
   * the sptr of the argument
   */
  if (REDUCG(sptr) && MIDNUMG(sptr)) {
    int arg;
    arg = MIDNUMG(sptr);
    ADDRESSP(arg, ADDRESSG(sptr));
    HOMEDP(arg, 0);
  }
}

/**
  \brief Get the alignment in bytes of a symbol representing a variable
 */
unsigned
align_of_var(SPTR sptr)
{
  DTYPE dtype = DTYPEG(sptr);
  if (!PDALN_IS_DEFAULT(sptr))
    return 1u << PDALNG(sptr);
  if (QALNG(sptr))
    return 4 * align_of(DT_INT);
  if (dtype) {
    if (flg.quad && !DESCARRAYG(sptr) && zsize_of(dtype) >= MIN_ALIGN_SIZE) {
      return DATA_ALIGN + 1;
    }
    return align_of(dtype);
  }
  if (STYPEG(sptr) == ST_PROC) /* No DTYPE */
    return align_of(DT_ADDR);
  return 0;
}

static void
assn_stkoff(int sptr, int dtype, ISZ_T size)
{
  int a;
  ISZ_T addr;

  /* hack to avoid problems with zero-length strings.
   * make character*0 appear like character*1 */
  if (size == 0)
    size = 1;
  if (XBIT(129, 0x40000000) && size > ALN_MINSZ && !DESCARRAYG(sptr)) {
    a = CACHE_ALIGN;
    size += ALN_UNIT * stk_aln_n;
    if (stk_aln_n <= ALN_THRESH)
      stk_aln_n++;
    else
      stk_aln_n = 1;
  } else if (STACK_CAN_BE_32_BYTE_ALIGNED && size >= 32) {
    a = 31;
    /* Round-up 'size' since sym's offset is 'aligned next' - size.
     */
    size = ALIGN(size, a);
  } else if ((flg.quad && size >= MIN_ALIGN_SIZE) ||
             (QALNG(sptr) && !DESCARRAYG(sptr))) {
    a = DATA_ALIGN;
    /* round-up size since sym's offset is 'aligned next' - size */
    size = ALIGN(size, a);
  } else
    a = align_unconstrained(dtype);
  addr = -gbl.locaddr;
  addr = ALIGN_AUTO(addr, a) - size;
  ADDRESSP(sptr, addr);
  gbl.locaddr = -addr;
  SYMLKP(sptr, gbl.locals);
  gbl.locals = sptr;
  if (DBGBIT(5, 32)) {
    fprintf(gbl.dbgfil, "addr: %6d size: %6d  %-32s   (%s)\n", (int)addr,
            (int)size, getprint(sptr), getprint((int)gbl.currsub));
  }
}

static void
assn_static_off(int sptr, int dtype, ISZ_T size)
{
  int a;
  ISZ_T addr;

  if (DINITG(sptr))
    addr = gbl.saddr;
  else
    addr = gbl.bss_addr;
  if (size == 0)
    size = 1;
  if (XBIT(129, 0x40000000) && size > ALN_MINSZ && DTY(dtype) != TY_CHAR) {
    a = CACHE_ALIGN;
    size += ALN_UNIT * bss_aln_n;
    if (bss_aln_n <= ALN_THRESH)
      bss_aln_n++;
    else
      bss_aln_n = 1;
  } else if ((flg.quad && size >= MIN_ALIGN_SIZE) || QALNG(sptr))
    a = DATA_ALIGN;
  else
    a = align_unconstrained(dtype);
  addr = ALIGN(addr, a);
  ADDRESSP(sptr, addr);
  if (DINITG(sptr)) {
    gbl.saddr = addr + size;
    SYMLKP(sptr, gbl.statics);
    gbl.statics = sptr;
    if (static_name_global == 1) {
      /* make sure the static_name gets output */
      static_name_global = 2;
      SYMLKP(static_base, gbl.basevars);
      gbl.basevars = static_base;
    }
    if (DBGBIT(5, 32)) {
      fprintf(gbl.dbgfil, "saddr: %6d size: %6d  %-32s   (%s)\n", (int)addr,
              (int)size, getprint(sptr), getprint((int)gbl.currsub));
    }
  } else {
    gbl.bss_addr = addr + size;
    SYMLKP(sptr, gbl.bssvars);
    gbl.bssvars = sptr;
    if (bss_name_global == 1) {
      /* make sure the bss_name gets output */
      bss_name_global = 2;
      SYMLKP(bss_base, gbl.basevars);
      gbl.basevars = bss_base;
    }
    if (DBGBIT(5, 32)) {
      fprintf(gbl.dbgfil, "baddr: %6d size: %6d  %-32s   (%s)\n", (int)addr,
              (int)size, getprint(sptr), getprint((int)gbl.currsub));
    }
  }
}

/**
   \brief Makes adjustments to the list \p loc_list
   \param loc_list   list of local symbols linked by SYMLK
   \param loc_addr   total size of the equivalenced locals

   The equivalence processor assigns positive offsets to the local variables
   which appear in equivalence statements.  Target addresses must be assigned
   using the offsets provided by the equivalence processor.
 */
void
fix_equiv_locals(int loc_list, ISZ_T loc_addr)
{
  int sym;
  ISZ_T maxa;

  if (loc_list != NOSYM) {
    maxa = alignment(DT_DBLE); /* align new size just in case */
    gbl.locaddr = ALIGN(gbl.locaddr + loc_addr, maxa);
    do {
      /* NOTE:  REF flag of sym set during equivalence processing */
      sym = loc_list;
      loc_list = SYMLKG(loc_list);

      ADDRESSP(sym, -gbl.locaddr + ADDRESSG(sym));
      SCP(sym, SC_LOCAL);
      SYMLKP(sym, gbl.locals);
      gbl.locals = sym;
    } while (loc_list != NOSYM);
  }

}

/*
 * similiar to fix_equiv_locals except that these local variables were
 * saved and/or dinit'd.  for these variables, switch the storage class to
 * SC_STATIC.
 * the equivalence processor assigns positive offsets to the local variables
 * which appear in equivalence statements.  Target addresses must be
 * assigned using the offsets provided by the equivalence processor.
 */
void
fix_equiv_statics(int loc_list,     /* list of local symbols linked by SYMLK */
                  ISZ_T loc_addr,   /* total size of the equivalenced locals */
                  LOGICAL dinitflg) /* variables were dinit'd */
{
  int sym;
  int maxa;
  ISZ_T addr;

#if DEBUG
  assert(loc_list != NOSYM, "fix_equiv_statics: bad loc_list", 0, 3);
#endif
  maxa = alignment(DT_DBLE); /* align new size just in case */
  if (dinitflg) {
    addr = gbl.saddr;
    addr = ALIGN(addr, maxa);
    do {
      /* NOTE:  REF flag of sym set during equivalence processing */
      sym = loc_list;
      loc_list = SYMLKG(loc_list);
      ADDRESSP(sym, addr + ADDRESSG(sym));
      SCP(sym, SC_STATIC);
      SYMLKP(gbl.statics, sym);
      gbl.statics = sym;
      DINITP(sym, 1); /* ensure getsname thinks it's in STATIC */
    } while (loc_list != NOSYM);
    gbl.saddr = addr += loc_addr;
    if (static_name_global == 1) {
      /* make sure the static_name gets output */
      static_name_global = 2;
      SYMLKP(static_base, gbl.basevars);
      gbl.basevars = static_base;
    }
  } else {
    addr = gbl.bss_addr;
    addr = ALIGN(addr, maxa);
    do {
      /* NOTE:  REF flag of sym set during equivalence processing */
      sym = loc_list;
      loc_list = SYMLKG(loc_list);
      ADDRESSP(sym, addr + ADDRESSG(sym));
      SYMLKP(sym, gbl.bssvars);
      gbl.bssvars = sym;
      SCP(sym, SC_STATIC);
    } while (loc_list != NOSYM);
    gbl.bss_addr = addr += loc_addr;
    if (bss_name_global == 1) {
      /* make sure the bss_name gets output */
      bss_name_global = 2;
      SYMLKP(bss_base, gbl.basevars);
      gbl.basevars = bss_base;
    }
  }

}

/*                         DEBUG Routines                           */

void
assem_emit_line(int findex, int lineno)
{
}

void
assem_emit_file_line(int findex, int lineno)
{
}

static char straddrbuf[20];
static char straddrpbuf[sizeof(bss_name) + 11 + 2];

static char *straddr(sptr) int sptr;
{
  sprintf(straddrbuf, "%ld", (long)ADDRESSG(sptr));
  return (straddrbuf);
}

static char *straddrp(sptr, bufptr) int sptr;
char *bufptr;
{
  sprintf(straddrpbuf, "%s+%ld", bufptr, (long)ADDRESSG(sptr));
  return (straddrpbuf);
}

char *
getaddrdebug(int sptr)
{
  switch (STYPEG(sptr)) {

  case ST_LABEL:
    return getsname(sptr);

  case ST_STAG:
  case ST_TYPEDEF:
  case ST_MEMBER:
    return straddr(sptr);

  case ST_VAR:
  case ST_ARRAY:
  case ST_STRUCT:
  case ST_UNION:
    switch (SCG(sptr)) {
    case SC_PRIVATE:
    case SC_NONE:
    case SC_LOCAL:
    case SC_DUMMY:
    case SC_CMBLK:
      return straddr(sptr);
    case SC_STATIC:
      if (CLASSG(sptr) && DESCARRAYG(sptr)) {
        return getsname(sptr);
      }
#ifdef BASEADDRG
      if (BASEADDRG(sptr)) {
        return straddrp(sptr, SYMNAME(BASESYMG(sptr)));
      }
#endif
      if (UPLEVELG(sptr) || (gbl.outlined && gbl.internal <= 1)) {
        if (DINITG(sptr))
          return straddrp(sptr, outer_static_name);
        return straddrp(sptr, outer_bss_name);
      }
      if (DINITG(sptr)) {
        if (static_name_global == 1) {
          /* make sure the static_name gets output */
          static_name_global = 2;
          SYMLKP(static_base, gbl.basevars);
          gbl.basevars = static_base;
        }
        if (gbl.outlined)
          return straddrp(sptr, outer_static_name);
        else
          return straddrp(sptr, static_name);
      }
      if (bss_name_global == 1) {
        /* make sure the bss_name gets output */
        bss_name_global = 2;
        SYMLKP(bss_base, gbl.basevars);
        gbl.basevars = bss_base;
      }
      return straddrp(sptr, bss_name);

    case SC_EXTERN:
      return getsname(sptr);
    case SC_BASED:
      return 0;
    }

  case ST_CMBLK:
  case ST_ENTRY:
  case ST_PROC:
  case ST_INTRIN:
  case ST_GENERIC:
  case ST_PD:
    switch (SCG(sptr)) {
    case SC_DUMMY:
      return straddr(sptr);
    case SC_NONE:
    case SC_LOCAL:
    case SC_STATIC:
    case SC_CMBLK:
    case SC_EXTERN:
      return getsname(sptr);
    case SC_PRIVATE:
    case SC_BASED:
      break;
    }
    return 0;
  default:
    return 0;
  }
}

/*                     Profiling Routines                           */

int
get_private_size()
{
  char name[32];
  if (gbl.prvt_sym_sz == 0) {
    strcpy(name, ".prvt");
    sprintf(&name[5], "%04d", gbl.func_count);
    gbl.prvt_sym_sz = getsymbol(name);
    STYPEP(gbl.prvt_sym_sz, ST_VAR);
    CCSYMP(gbl.prvt_sym_sz, 1);
    DTYPEP(gbl.prvt_sym_sz, DT_INT8);
    DINITP(gbl.prvt_sym_sz, 1);
    SCP(gbl.prvt_sym_sz, SC_STATIC);
  }
  return gbl.prvt_sym_sz;
}
int
get_stack_size()
{
  char name[10];
  if (gbl.stk_sym_sz == 0) {
    strcpy(name, ".stk");
    sprintf(&name[4], "%04d", gbl.func_count);
    gbl.stk_sym_sz = getsymbol(name);
    STYPEP(gbl.stk_sym_sz, ST_VAR);
    CCSYMP(gbl.stk_sym_sz, 1);
    DTYPEP(gbl.stk_sym_sz, DT_INT8);
    DINITP(gbl.stk_sym_sz, 1);
    SCP(gbl.stk_sym_sz, SC_STATIC);
  }
  return gbl.stk_sym_sz;
}
/**
   \brief The F90 front-end may have allocated private variables - need to
   adjust the initial size of the private area.
 */
void
set_private_size(ISZ_T sz)
{
  prvt.addr = sz + 0;
}


void
set_bss_addr(int size)
{
  gbl.bss_addr = size;
} /* set_bss_addr */

int
get_bss_addr()
{
  return gbl.bss_addr;
} /* get_bss_addr */

/* determine the alignment of the address represented by syma, an address
 * constant, within a cache-aligned container:
 * Returns:
 *   -1 - unknown
 * >= 0 - byte boundary of the address, for example, given a single
 *        precision quantity and a container which is 16-byte aligned
 *        the following values are possible:
 *        0 - aligned with the beginning of the container.
 *        4 - multiple of 4 bytes from the beginning of the container.
 *        8 - multiple of 8 bytes from the beginning of the container.
 *       12 - multiple of 12 bytes from the beginning of the container.
 */
int
runtime_alignment(int syma)
{
  int sptr, offset;

  sptr = CONVAL1G(syma);
  if (sptr) {
    sym_is_refd(sptr);
  }
  offset = CONVAL2G(syma);
#undef ALN
#define ALN(x, a) ((x)&a)
  if (!sptr) {
    return ALN(offset, DATA_ALIGN);
  }
  switch (SCG(sptr)) {
  case SC_LOCAL:
  case SC_PRIVATE:
  case SC_STATIC:
  case SC_CMBLK:
    /*
     * The stack, common blocks, bss, and data sections are
     * cache aligned.
     */
    return ALN(ADDRESSG(sptr) + offset, DATA_ALIGN);
    break;
  case SC_BASED:
    break;
  case SC_DUMMY:
  /* fall thru - QALN set by ipa */
  case SC_EXTERN:
    if (QALNG(sptr))
      return ALN(offset, DATA_ALIGN);
    break;
  case SC_NONE:
    break;
  }
  return -1;
} /* end runtime_alignment( int syma ) */

/*
 * Return the 32-byte alignment of the address represented by the
 * address constant 'acon_sptr', or -1 if it's unknown.
 */
int
runtime_32_byte_alignment(int acon_sptr)
{
  int var_sptr;

  if (!STACK_CAN_BE_32_BYTE_ALIGNED)
    return -1;

  var_sptr = CONVAL1G(acon_sptr);
  if (!var_sptr)
    return -1;

  sym_is_refd(var_sptr);

  if (SCG(var_sptr) == SC_LOCAL) {
    ENFORCE_32_BYTE_STACK_ALIGNMENT;
    return ALN(ADDRESSG(var_sptr) + CONVAL2G(acon_sptr), 31);
  } else
    return -1;

} /* end runtime_32_byte_alignment( int acon_sptr ) */

/* determine if the address represented by syma, an address constant,
 * is cache aligned.
 */
int
is_cache_aligned(int syma)
{
  if (runtime_alignment(syma))
    return 0;
  return 1;
}

void
create_static_name(char *name, int usestatic, int num)
{
  if (usestatic) {
    sprintf(name, ".GL.STAT%d", num);
  } else {
    sprintf(name, ".GL.BSS%d", num);
  }
} /* create_static_name */

/*
 * Create a new name for the base address of the statics,
 * initialized and uninitialized.
 * Put these names in static_name and bss_name.
 * Create symbols (ST_IDENT) to hold these names.
 * Go through the list of statics in gbl.statics and gbl.bssvars,
 * set the BASEADDR field and set the MIDNUM field to the appropriate symbol
 */
void
create_static_base(int num)
{
  int sptr;
  if (num <= 0) {
    static_name_initialized = 0;
    static_name_global = 0;
    static_base = 0;
    bss_name_initialized = 0;
    bss_name_global = 0;
    bss_base = 0;
    return;
  }
  if (gbl.outlined)
    create_static_name(outer_bss_name, 0, num);
  else
    create_static_name(bss_name, 0, num);
  bss_base = addnewsym(bss_name);
  STYPEP(bss_base, ST_BASE);
  bss_name_initialized = 1;
  if (gbl.bssvars <= NOSYM) {
    SYMLKP(bss_base, NOSYM);
    bss_name_global = 1;
    if (gbl.bss_addr > 0) {
      bss_name_global = 2;
      SYMLKP(bss_base, gbl.basevars);
      gbl.basevars = bss_base;
    }
  } else {
    bss_name_global = 2;
    SYMLKP(bss_base, gbl.basevars);
    gbl.basevars = bss_base;
    for (sptr = gbl.bssvars; sptr > NOSYM; sptr = SYMLKG(sptr)) {
      BASEADDRP(sptr, 1);
      BASESYMP(sptr, bss_base);
    }
  }
  if (gbl.outlined)
    create_static_name(outer_static_name, 1, num);
  else
    create_static_name(static_name, 1, num);
  static_base = addnewsym(static_name);
  STYPEP(static_base, ST_BASE);
  static_name_initialized = 1;
  if (gbl.statics <= NOSYM) {
    SYMLKP(static_base, NOSYM);
    static_name_global = 1;
    if (gbl.saddr > 0) {
      static_name_global = 2;
      SYMLKP(static_base, gbl.basevars);
      gbl.basevars = static_base;
    }
  } else {
    static_name_global = 2;
    SYMLKP(static_base, gbl.basevars);
    gbl.basevars = static_base;
    for (sptr = gbl.statics; sptr > NOSYM; sptr = SYMLKG(sptr)) {
      BASEADDRP(sptr, 1);
      BASESYMP(sptr, static_base);
    }
  }
} /* create_static_base */

/**
   \brief Get the list to attach !dbg for the symbol \p sptr
   \param sptr  the symbol (of an object)
 */
LL_ObjToDbgList **
llassem_get_objtodbg_list(SPTR sptr)
{
  switch (SCG(sptr)) {
  case SC_STATIC:
    if (CLASSG(sptr) && DESCARRAYG(sptr))
      return NULL;
#ifdef BASEADDRG
    if (BASEADDRG(sptr))
      return NULL; //SYMNAME(BASESYMG(sptr));
#endif
    if (ALTNAMEG(sptr))
      return NULL; //get_altname(sptr);
    if (UPLEVELG(sptr)) {
      if (DINITG(sptr))
        return NULL; //outer_static_name;
      return NULL; //outer_bss_name;
    }
    if (SECTG(sptr)) {
      //sprintf(name, ".SECTION%d_%d_%s", gbl.func_count, sptr, prepend);
      return NULL; //name;
    }
    if (ALTNAMEG(sptr))
      return NULL; //get_altname(sptr);
    if (DINITG(sptr)) {
      if (gbl.outlined && ENCLFUNCG(sptr) && (ENCLFUNCG(sptr) == gbl.currsub))
        return &static_dbg_list;
      /* zero sized array reference, use BSS instead of STATICS */
      if ((DTY(DTYPEG(sptr)) == TY_ARRAY) && SCG(sptr) == SC_STATIC &&
          extent_of(DTYPEG(sptr)) == 0)
        return &bss_dbg_list;
      if (gbl.outlined) {
        if ( gbl.internal > 1)
          return NULL; //contained_static_name;
        return NULL; //outer_static_name;
      }
      return &static_dbg_list;
    }
    if (gbl.outlined) {
      if (gbl.internal > 1)
        return NULL; //contained_bss_name;
      return NULL; //outer_bss_name;
    }
    return &bss_dbg_list;
  default:
    break;
  }
  return NULL;
}

/**
   \brief Get the LLVM name of the symbol \p sptr
   \param sptr  The symbol
   \return a name (as a possibly transient string)

   NB: This \e may return a pointer to a global buffer, so a subsequent call can
   silently clobber the string returned.
 */
char *
get_llvm_name(int sptr)
{
  static char name[MXIDLN]; /* 1 for null, 3 for extra '_' ,
                             * 4 for @### with mscall
                             */
  char *p, *q, ch;
  LOGICAL has_underscore = 0;
  int m;
  char *prepend = "\0";
  const SYMTYPE stype = STYPEG(sptr);

  switch (stype) {
  case ST_MEMBER:
    return SYMNAME(sptr);

  case ST_LABEL:
    sprintf(name, "%sB%d_%d", ULABPFX, gbl.func_count, sptr);
    break;
  case ST_CONST:
  case ST_PARAM:
      sprintf(name, ".C%d_%s", sptr, getfuncname(gbl.currsub));
    break;
  case ST_BASE:
    if (MIDNUMG(sptr))
      return SYMNAME(MIDNUMG(sptr));
    return SYMNAME(sptr);
  case ST_VAR:
  case ST_ARRAY:
  case ST_STRUCT:
  case ST_UNION:
  case ST_NML:
  case ST_PLIST:
    switch (SCG(sptr)) {
    case SC_DUMMY:
      if (MIDNUMG(sptr)) {
        if ((SC_DUMMY == SCG(MIDNUMG(sptr))) ||
            (!HOMEDG(sptr) && ((gbl.internal != 1) || (!PASSBYVALG(sptr)))))
          sptr = MIDNUMG(sptr);
      }
      return SYMNAME(sptr);

    case SC_EXTERN:
      if (ALTNAMEG(sptr) && CFUNCG(sptr))
        return get_altname(sptr);
      goto xlate_name;
    case SC_CMBLK:
      if (ALTNAMEG(sptr))
        return get_altname(sptr);
      /* modification needed on this name ? */
      if (CFUNCG(sptr))
        return SYMNAME(sptr);
      return getsname((int)MIDNUMG(sptr));

    case SC_LOCAL:
      if ((!REFG(sptr) && DINITG(sptr)) || !DINITG(sptr)) {

        if (CCSYMG(sptr)) {
          /* append sptr to avoid duplicate local symbol name */
          sprintf(name, "%s_%d", SYMNAME(sptr), sptr);
          return name;
        }
        /* keep name as shown in our symbol table */
        sprintf(name, "%s_%d", SYMNAME(sptr), sptr);
        return name;
      }
    case SC_STATIC:
      if (CLASSG(sptr) && DESCARRAYG(sptr))
        goto xlate_name;
#ifdef BASEADDRG
      if (BASEADDRG(sptr))
        return SYMNAME(BASESYMG(sptr));
#endif
      if (ALTNAMEG(sptr))
        return get_altname(sptr);
      if (UPLEVELG(sptr)) {
        if (DINITG(sptr))
          return outer_static_name;
        return outer_bss_name;
      }
      if (SECTG(sptr)) {
#ifdef CUDAG
        if (gbl.currsub && (CUDAG(gbl.currsub) & CUDA_CONSTRUCTOR) &&
            global_sptr) {
          /* prepend a module or routine name defined in this file */
          prepend = AG_NAME(global_sptr);
        }
#endif
        sprintf(name, ".SECTION%d_%d_%s", gbl.func_count, sptr, prepend);
        return name;
      }
      if (ALTNAMEG(sptr))
        return get_altname(sptr);
      if (DINITG(sptr)) {
        if (gbl.outlined && ENCLFUNCG(sptr) && (ENCLFUNCG(sptr) == gbl.currsub))
          return static_name;
        if (static_name_global == 1) {
          /* zero sized array reference, use BSS instead of STATICS */
          if ((DTY(DTYPEG(sptr)) == TY_ARRAY) && extent_of(DTYPEG(sptr)) == 0) {
            bss_name_global = 2;
            SYMLKP(bss_base, gbl.basevars);
            gbl.basevars = bss_base;
            ADDRESSP(sptr, gbl.bss_addr);
            if (gbl.bss_addr == 0)
              gbl.bss_addr = 4;
          } else {
            static_name_global = 2;
            SYMLKP(static_base, gbl.basevars);
            gbl.basevars = static_base;
          }
        }
        /* zero sized array reference, use BSS instead of STATICS */
        if ((DTY(DTYPEG(sptr)) == TY_ARRAY) && extent_of(DTYPEG(sptr)) == 0) {
          ADDRESSP(sptr, gbl.bss_addr);
          if (gbl.bss_addr == 0)
            gbl.bss_addr = 4;
          return bss_name;
        }
        if (gbl.outlined) {
          if ( gbl.internal > 1)
            return contained_static_name;
          return outer_static_name;
        }
        return static_name;
      }
      if (bss_name_global == 1) {
        /* make sure the bss_name gets output */
        bss_name_global = 2;
        SYMLKP(bss_base, gbl.basevars);
        gbl.basevars = bss_base;
      }
      if (gbl.outlined) {
        if (gbl.internal > 1)
          return contained_bss_name;
        return outer_bss_name;
      }
      return bss_name;

    case SC_BASED:
      if (MIDNUMG(sptr) && SCG(MIDNUMG(sptr)) == SC_DUMMY)
        return SYMNAME(MIDNUMG(sptr));
      // fall-through
    case SC_PRIVATE:
      sprintf(name, "%s_%d", SYMNAME(sptr), sptr);
      break;
    default:
      sprintf(name, ".V%d_%d", gbl.func_count, sptr);
      break;
    }
    return name;
  case ST_CMBLK:
#if defined(TARGET_OSX)
    if (FROMMODG(sptr)) { /* common block is from a module */
      int md;
      md = SCOPEG(sptr);
      if (md && NEEDMODG(md)) {
        /*  module is use-associated */
        TYPDP(md, 1);
      }
    }
#endif
    if (ALTNAMEG(sptr))
      return (get_altname(sptr));
    if (CFUNCG(sptr)) {
      /* common block C name compatibility : no underscore */
      return SYMNAME(sptr);
    }

  xlate_name:
    if (XBIT(119, 0x1000)) { /* add leading underscore */
      name[0] = '_';
      p = name + 1;
    } else {
      p = name;
    }
    q = SYMNAME(sptr);
    while ((ch = *q++)) {
      if (ch == '$')
        *p++ = flg.dollar;
      else
        *p++ = ch;
      if (ch == '_')
        has_underscore = TRUE;
    }
/*
 * append underscore to name??? -
 * - always for common block (note - common block may have CCSYM set),
 * - not compiler-created external variable,
 * - modified by -x 119 0x0100000 or -x 119 0x02000000
 */
    if ((STYPEG(sptr) == ST_CMBLK || !CCSYMG(sptr)) && !CFUNCG(sptr)) {
      if (!XBIT(119, 0x01000000)) {
        *p++ = '_';
        if (XBIT(119, 0x02000000) && has_underscore && !CCSYMG(sptr))
          *p++ = '_';
      }
    }
    *p = '\0';
#if defined(TARGET_WIN)
    if (!XBIT(121, 0x200000) && STYPEG(sptr) == ST_CMBLK && !CCSYMG(sptr) &&
        XBIT(119, 0x01000000))
      upcase_name(name);
#endif
    break;
  case ST_ENTRY:
  case ST_PROC:
    if (ALTNAMEG(sptr)) {
      return get_altname(sptr);
    }
    if (SCG(sptr) == SC_DUMMY)
      return SYMNAME(sptr);
    if ((flg.smp || XBIT(34, 0x200)) && OUTLINEDG(sptr)) {
      sprintf(name, "%s", SYMNAME(sptr));
      p = name;
    }
    else if (gbl.internal && CONTAINEDG(sptr)) {
      p = name;
      if (gbl.outersub) {
        m = INMODULEG(gbl.outersub);
        if (m) {
          q = SYMNAME(m);
          while ((ch = *q++)) {
            if (ch == '$')
              *p++ = flg.dollar;
            else
              *p++ = ch;
          }
          *p++ = '_';
        }
        q = SYMNAME(gbl.outersub);
        while ((ch = *q++)) {
          if (ch == '$')
            *p++ = flg.dollar;
          else
            *p++ = ch;
        }
        *p++ = '_';
      }
      q = SYMNAME(sptr);
      while ((ch = *q++)) {
        if (ch == '$')
          *p++ = flg.dollar;
        else
          *p++ = ch;
      }
      *p = '\0';
      return name;
    }
    if (XBIT(119, 0x1000)) { /* add leading underscore */
      name[0] = '_';
      p = name + 1;
    } else
      p = name;
    m = INMODULEG(sptr);
    if (m) {
      q = SYMNAME(m);
      while ((ch = *q++)) {
        if (ch == '$')
          *p++ = flg.dollar;
        else
          *p++ = ch;
      }
      *p++ = '_';
    }
    if (stype != ST_ENTRY || gbl.rutype != RU_PROG) {
      q = SYMNAME(sptr);
    } else if ((flg.smp || XBIT(34, 0x200) || gbl.usekmpc) && OUTLINEDG(sptr)) {
      q = SYMNAME(sptr);
    } else {
#if defined(TARGET_WIN)
      /* we have a mix of undecorated and decorated names on win32 */
      strcpy(name, "MAIN_");
      return name;
#else
      q = "MAIN";
#endif
    }
    while ((ch = *q++)) {
      if (ch == '$')
        *p++ = flg.dollar;
      else
        *p++ = ch;
      if (ch == '_')
        has_underscore = TRUE;
    }
/*
 * append underscore to name??? -
 * - always for entry,
 * - procedure if not compiler-created and not a "C" external..
 * - modified by -x 119 0x0100000 or -x 119 0x02000000
 */
    if (stype != ST_PROC || (!CCSYMG(sptr) && !CFUNCG(sptr))) {
      /* functions marked as !DEC$ ATTRIBUTES C get no underbar */
      if (!XBIT(119, 0x01000000) && !CFUNCG(sptr) && !CREFG(sptr)
#ifdef CONTAINEDG
          && !CONTAINEDG(sptr)
#endif
              ) {
        *p++ = '_';
        if (XBIT(119, 0x02000000) && has_underscore)
          *p++ = '_';
      }
    }
    *p = '\0';
    if (MSCALLG(sptr) && !CFUNCG(sptr) && !XBIT(119, 0x4000000)) {
      if (ARGSIZEG(sptr) == -1)
        sprintf(name, "%s@0", name);
      else if (ARGSIZEG(sptr) > 0) {
        sprintf(name, "%s@%d", name, ARGSIZEG(sptr));
      }
    }
    if (!XBIT(121, 0x200000) &&
        ((MSCALLG(sptr) && !STDCALLG(sptr)) ||
         (CREFG(sptr) && !CFUNCG(sptr) && !CCSYMG(sptr))))
      /* if WINNT calling conventions are used, the name must be
       * uppercase unless the subprogram has the STDCALL attribute.
       * All cref intrinsic are lowercase.
       */
      upcase_name(name);

    break;
  default:
    interr("get_llvm_name: bad stype for", sptr, 3);
    strcpy(name, "b??");
  }
  return name;
}

char *
get_string_constant(int sptr)
{
  char *name, *to, *from;
  int c, len, newlen;

  if (STYPEG(sptr) == ST_CONST) {
    len = size_of(DTYPEG(sptr));
    newlen = 3;
    from = stb.n_base + CONVAL1G(sptr);
    while (len--) {
      c = *from++ & 0xff;
      if (c == '\"' || c == '\'' || c == '\\') {
        newlen += 2;
      } else if (c >= ' ' && c <= '~') {
        newlen++;
      } else if (c == '\n') {
        newlen += 2;
      } else {
        newlen += 4;
      }
    }
    name = (char *)getitem(LLVM_LONGTERM_AREA, (newlen + 3) * sizeof(char));
    *name = '\"';
    to = name + 1;
    from = stb.n_base + CONVAL1G(sptr);
    len = size_of(DTYPEG(sptr));
    while (len--) {
      c = *from++ & 0xff;
      if (c == '\"' || c == '\'' || c == '\\') {
        *to++ = '\\';
        *to++ = c;
      } else if (c >= ' ' && c <= '~') {
        *to++ = c;
      } else if (c == '\n') {
        *to++ = '\\';
        *to++ = 'n';
      } else {
        *to++ = '\\';
        sprintf(to, "%03o", c);
        to += 3;
      }
    }
    *to++ = '\"';
  }
  return name;
}

static char *
write_ftn_type(LL_Type *ll_type, char *argptr, int byval)
{
  // NB, the original code looks to be buggy
  switch (ll_type->data_type) {
  case LL_PTR:
  case LL_ARRAY:
  case LL_STRUCT:
  case LL_FUNCTION:
  case LL_VOID:
    sprintf(argptr, "i8*");
    break;
  case LL_I1:
  case LL_I8:
  case LL_I16:
  case LL_I24:
  case LL_I32:
  case LL_I40:
  case LL_I48:
  case LL_I56:
  case LL_I64:
  case LL_I128:
  case LL_I256:
    sprintf(argptr, "i%d", ll_type_int_bits(ll_type));
    break;
  default:
    sprintf(argptr, "%s", ll_type->str);
    break;
  }
  return argptr + strlen(argptr);
}

static int
get_ag_size(int gblsym)
{
  return gblsym ? AG_SIZE(gblsym) : 0;
}

int
get_ag_argdtlist_length(int gblsym)
{
  return gblsym ? AG_ARGDTLIST_LENGTH(gblsym) : 0;
}

int
has_valid_ag_argdtlist(int gblsym)
{
  return gblsym ? AG_ARGDTLIST_IS_VALID(gblsym) : FALSE;
}

void
set_ag_argdtlist_is_valid(int gblsym)
{
  AG_ARGDTLIST_IS_VALID(gblsym) = TRUE;
}

char *
get_ag_typename(int gblsym)
{
  return AG_TYPENAME(gblsym);
}

int
add_ag_typename(int gblsym, char *typename)
{
  INT nmptr;
  nmptr = add_ag_name(typename);
  AG_TYPENMPTR(gblsym) = nmptr;
  return AG_TYPENMPTR(gblsym);
}

int
get_intrin_ag(char *ag_name, int dtype)
{
  int gblsym = find_ag(ag_name);

  if (gblsym)
    goto Found;

  /* Enter new symbol into the global symbol table */
  gblsym = make_gblsym(0, ag_name);
  AG_SYMLK(gblsym) = ag_intrin;
  ag_intrin = gblsym;
  return gblsym;

Found:

  return gblsym;
}

int
get_dummy_ag(int sptr)
{
  int gblsym, nptr, hashval;
  char *ag_name;

  ag_name = get_llvm_name(sptr);
  hashval = name_to_hash(ag_name, strlen(ag_name));
  gblsym = find_local_ag(ag_name);

  if (gblsym)
    goto Found;

  /* Enter new symbol into the global symbol table */
  gblsym = agb_local.s_avl++;
  NEED(agb_local.s_avl + 1, agb_local.s_base, AG, agb_local.s_size,
       agb_local.s_size + 32);

  nptr = add_ag_local_name(ag_name);

  BZERO(&agb_local.s_base[gblsym], AG, 1);
  AGL_NMPTR(gblsym) = nptr;
  AGL_HASHLK(gblsym) = agb_local.hashtb[hashval];
  agb_local.hashtb[hashval] = gblsym;
  AGL_SYMLK(gblsym) = ag_local;
  ag_local = gblsym;
  if (MIDNUMG(sptr))
    AGL_DTYPE(gblsym) = DTYPEG(MIDNUMG(sptr));
  else
    AGL_DTYPE(gblsym) = DTYPEG(sptr);

  return gblsym;

Found:

  return gblsym;
}

int
get_llvm_funcptr_ag(int sptr, char *ag_name)
{
  int gblsym = find_ag(ag_name);

  if (gblsym)
    goto Found;

  /* Enter new symbol into the global symbol table */
  gblsym = make_gblsym(sptr, ag_name);
  AG_SIZE(gblsym) = 0;
  AG_ISIFACE(gblsym) = 1;
  AG_DEVICE(gblsym) = 0;
  AG_SYMLK(gblsym) = ag_funcptr;
  ag_funcptr = gblsym;

Found:
  return gblsym;
}

void
deleteag_llvm_argdtlist(int gblsym)
{
  DTLIST *t = AG_ARGDTLIST(gblsym);
  DTLIST *pre;
  while (t) {
    pre = t;
    t = t->next;
    free(pre);
  }
  AG_ARGDTLIST(gblsym) = NULL;
}

char *
get_argdtlist(int gblsym)
{
  if (gblsym)
    return (char *)AG_ARGDTLIST(gblsym);
  return NULL;
}

char *
get_next_argdtlist(char *argdtlist)
{
  if (argdtlist)
    return (char *)(((DTLIST *)argdtlist)->next);
  return NULL;
}

/* arg_num: Is zero based.  arg_num zero is the initial element in the argdtlist
 * if it exists, NULL otherwise.
 */
static DTLIST *
get_argdt(int gblsym, int arg_num)
{
  int i;
  DTLIST *arg;

  for (i = 0, arg = AG_ARGDTLIST(gblsym); arg && (i < arg_num);
       ++i, arg = (DTLIST *)get_next_argdtlist((char *)arg)) {
    ; /* Iterate */
  }

  return (arg && (i == arg_num)) ? arg : NULL;
}

/* arg_num:   [1-n] where 1 is the first argument passed and the function
 *            contains n arguments.
 * arg_num 0: The function's return value.
 *
 * Called by: build_routine_parameters()
 */
void
addag_llvm_argdtlist(int gblsym, int arg_num, int arg_sptr, LL_Type *lltype)
{
  LOGICAL added;
  DTLIST *newt;
  DTLIST *t = AG_ARGDTLIST(gblsym);
  assert(arg_sptr, "Adding argument with unknown sptr", arg_sptr, 4);

  /* If we have already added this arg, update the sptr */
  added = FALSE;
  if (arg_num < AG_ARGDTLIST_LENGTH(gblsym)) {
    newt = (DTLIST *)get_argdt(gblsym, arg_num);
    assert(newt, "addag_llvm_argdtlist: Could not locate sptr", arg_sptr, 4);
  } else {
    NEW(newt, DTLIST, 1);
    memset(newt, 0, sizeof(DTLIST));
    added = TRUE;
  }

  /* Instantiate */
  newt->lltype = lltype;
  newt->byval = PASSBYVALG(arg_sptr);
  newt->sptr = arg_sptr;

  /* Link if this is a new entry */
  if (added) {
    if (t == NULL) {
      AG_ARGDTLIST(gblsym) = newt;
      t = AG_ARGDTLIST(gblsym);
      t->tail = newt;
    } else {
      t->tail->next = newt;
      t->tail = newt;
    }
    ++AG_ARGDTLIST_LENGTH(gblsym);
  }

  AG_ARGDTLIST_IS_VALID(gblsym) = TRUE;
}

LL_Type *
get_lltype_from_argdtlist(char *argdtlist)
{
  if (argdtlist)
    return ((DTLIST *)argdtlist)->lltype;
  return NULL;
}

LOGICAL
get_byval_from_argdtlist(const char *argdtlist)
{
  if (argdtlist)
    return ((DTLIST *)argdtlist)->byval;

  return FALSE; /* Fortran is pass by ref by default */
}

int
get_sptr_from_argdtlist(char *argdtlist)
{
  if (argdtlist)
    return ((DTLIST *)argdtlist)->sptr;
  return 0;
}

LOGICAL
is_llvmag_entry(int gblsym)
{
  if (gblsym == 0)
    return FALSE;
  return (AG_STYPE(gblsym) == ST_ENTRY);
}

void
set_llvmag_entry(int gblsym)
{
  if (gblsym == 0)
    return;
  AG_STYPE(gblsym) = ST_ENTRY;
}

LOGICAL
is_llvmag_iface(int gblsym)
{
  if (gblsym == 0)
    return FALSE;
  return (AG_ISIFACE(gblsym) == 1);
}

static void
write_module_as_subroutine(void)
{
  int dtype = DTYPEG(gbl.currsub);
  const char *name = get_llvm_name(gbl.currsub);

  init_output_file();
  FTN_HAS_INIT() = 1;
  print_token("define");
  print_space(1);
  write_type(make_lltype_from_dtype(dtype));
  print_space(1);
  print_token("@");
  print_token(name);
  print_token("() noinline");
  print_token(" { ");
  print_nl();
  print_line(".L.entry:");

  /*  print return statement */
  print_token("\t");
  print_token("ret");
  print_space(1);
  write_type(make_lltype_from_dtype(dtype));
  ll_proto_set_defined_body(name, TRUE);

  if (dtype == 0) {
    print_nl();
    print_token(" } ");
    print_nl();
    return;
  }

  switch (dttypes[dtype]) {
  case _TY_INT:
    print_token(" 0");
  case _TY_REAL:
    print_token(" 0.0");
  case _TY_CMPLX:
  default:
    print_token(" undef");
  }
  print_nl();
  print_token(" } ");
  print_nl();
}

int
find_funcptr_name(int sptr)
{
  int gblsym, hashval, len;
  char *np, *sp, sptrnm[MXIDLN];

  /* Key */
  sprintf(sptrnm, "%s_%d", get_llvm_name(sptr), sptr); /* Local name */
  len = strlen(sptrnm);
  hashval = name_to_hash(sptrnm, len);

  for (gblsym = fptr_local.hashtb[hashval]; gblsym;
       gblsym = FPTR_HASHLK(gblsym)) {
    np = sptrnm;
    sp = FPTR_NAME(gblsym);
    do {
      if (*np++ != *sp++)
        goto Continue;
    } while (*sp);
    if (np - sptrnm != len)
      continue;
    goto Found;
  Continue:
    if (gblsym == FPTR_HASHLK(gblsym))
      return 0;
  }
  return 0;

Found:
  return gblsym;
}

/* Return the AG number associated to the local sptr value:
 * 1) Search local-fnptr-table of function pointers
 * 2) Get the ag name from (1)
 * 3) Get the gblsym using the ag name from (2)
 * 4) Return the AG gblsym from (3)
 */
int
local_funcptr_sptr_to_gblsym(int sptr)
{
  const int key = find_funcptr_name(sptr);

  assert(key, "local_funcptr_sptr_to_gblsym: "
              "No funcptr associated with sptr:",
         sptr, 4);

  return find_ag(FPTR_IFACENM(key));
}

void
set_llvm_iface_oldname(int gblsym, char *nm)
{
  INT nmptr;
  nmptr = add_ag_name(nm);
  AG_OLDNMPTR(gblsym) = nmptr;
}

/*
 * This function will store name that will be used to search in ag global table
 * Global name is: <ag_name>_%sptr
 * <ag_name> is supposedly in format of: get_llvm_name(module/function)_$_<ifacename>
 * With the assumption that module/function would be unique.
 * Reason why we use derived type name insteaf of interface function name
 * because
 * interface is not available when we read .ilm file.
 */
void
llvm_funcptr_store(int sptr, char *ag_name)
{
  int hashval, gblsym;
  char sptrnm[MXIDLN];
  INT nmptr;

  gblsym = find_funcptr_name(sptr);
  if (gblsym > 0)
    return;

  gblsym = fptr_local.s_avl++;
  NEED(fptr_local.s_avl + 1, fptr_local.s_base, FPTRSYM, fptr_local.s_size,
       fptr_local.s_size + 5);

  BZERO(&fptr_local.s_base[gblsym], FPTRSYM, 1);

  sprintf(sptrnm, "%s_%d", get_llvm_name(sptr), sptr);
  hashval = name_to_hash(sptrnm, strlen(sptrnm));
  fptr_local.hashtb[hashval] = gblsym;
  FPTR_HASHLK(gblsym) = fptr_local.hashtb[hashval];
  FPTR_SYMLK(gblsym) = ptr_local;
  nmptr = add_ag_fptr_name(sptrnm); /* fnptr_local key */
  FPTR_NMPTR(gblsym) = nmptr;
  nmptr = add_ag_fptr_name(ag_name); /* gblsym key      */
  FPTR_IFACENMPTR(gblsym) = nmptr;
  ptr_local = gblsym;
}

/* create struct which will be filled uplevel variables addresses. */
int
make_uplevel_arg_struct()
{
  int gblsym, dtype, mem1, mem2, i;
  ISZ_T size, total_size;
  char name[MXIDLN], tname[MXIDLN + 8];

  /* Instance and type name */
  sprintf(name, "_ul_%s_%d", get_llvm_name(gbl.currsub), gbl.currsub); /* Instance */
  sprintf(tname, "struct%s", name);                               /* Type */
  dtype = mk_struct_for_llvm_init(name, 16);

  size = size_of(DT_ADDR);
  total_size = 0;
  mem1 = 0;
  mem2 = NOSYM;

  if (gbl.internal == 1 && gbl.outlined && gbl.outersub)
    gblsym = find_ag(get_ag_searchnm(gbl.outersub));
  else
    gblsym = find_ag(get_ag_searchnm(gbl.currsub));

  for (i = 0; i < AG_UPLEVEL_AVL(gblsym); i++) {
    if (AG_UPLEVEL_OLD(gblsym, i))
      mem2 = add_member_for_llvm(AG_UPLEVEL_NEW(gblsym, i), mem2, DT_ADDR,
                                 total_size);
    else {
      mem2 = add_member_for_llvm(AG_UPLEVEL_NEW(gblsym, i), mem2, DT_INT8,
                                 total_size);
    }
    AG_UPLEVEL_MEM(gblsym, i) = mem2;
    if (mem1 == 0)
      mem1 = mem2;
    total_size += size;
    DTY(dtype + 2) = AG_UPLEVEL_AVL(gblsym) * size;
  }
  if (AG_UPLEVEL_AVL(gblsym) ==
      0) { /* make up some dump member otherwise the bridge
            * will create opague structure and llvm will complain
            */
    mem1 = add_member_for_llvm(DTY(dtype + 3), mem2, DT_ADDR, total_size);
    DTY(dtype + 2) = size;
  }

  /* fill member */
  DTY(dtype + 4) = alignment(DT_ADDR);
  DTY(dtype + 1) = mem1;

  /* Create an lldef entry and add to struct_def list to be printed later */
  make_lltype_from_dtype(dtype);
  return dtype;
}

void
add_uplevel_to_host(int *ptr, int cnt)
{
  int hsize;
  int havl;
  UPLEVEL_PAIR *hptr;
  UPLEVEL_PAIR *nptr;
  int total, i, j, gblsym;

  gblsym = find_ag(get_llvm_name(gbl.outersub));

  if (!gblsym)
    return;

  hsize = AG_UPLEVEL_SZ(gblsym);
  havl = AG_UPLEVEL_AVL(gblsym);
  hptr = AG_UPLEVELPTR(gblsym);

  /* need to filter out SC_STATIC and SC_CMBLK */
  if (havl == 0) {
    NEW(hptr, UPLEVEL_PAIR, cnt);
    AG_UPLEVEL_SZ(gblsym) = cnt;
    for (i = 0; i < cnt; i++) {
      hptr[i].oldsptr = ptr[i];
    }
    AG_UPLEVEL_AVL(gblsym) = cnt;
    AG_UPLEVELPTR(gblsym) = hptr;
  } else {
    /* Reallocate ptr to be cnt+hsize so that we don't have to reallocate too
     * often */
    NEW(nptr, UPLEVEL_PAIR, cnt + havl);
    total = 0;
    for (i = 0, j = 0; i < cnt && j < hsize; total++) {
      if (hptr[j].oldsptr < *ptr) {
        nptr[total].oldsptr = hptr[j].oldsptr;
        j++;
      } else {
        nptr[total].oldsptr = *ptr;
        i++;
        ptr++;
      }
    }
    if (i < cnt) {
      do {
        nptr[total].oldsptr = *ptr;
        i++;
        total++;
        ptr++;
      } while (i < cnt);

    } else if (j < hsize) {
      do {
        nptr[total].oldsptr = hptr[j].oldsptr;
        j++;
        total++;
      } while (j < hsize);
    }
    FREE(AG_UPLEVELPTR(gblsym));
    AG_UPLEVEL_AVL(gblsym) = total;
    AG_UPLEVEL_SZ(gblsym) = cnt + hsize;
    AG_UPLEVELPTR(gblsym) = nptr;
  }
}

int
get_uplevel_address_size()
{
  int gblsym;
  gblsym = find_ag(get_llvm_name(gbl.outersub));
  if (gblsym)
    return AG_UPLEVEL_AVL(gblsym);
  return 0;
}

/* If AG_UPLEVEL_OLD is 0, then it is len of character of the previous argument
 * and
 * it is passing by value - it is 32-bit in size for 32-bit and 64-bit for
 * 64-bit target.
 */
void
_fixup_llvm_uplevel_symbol()
{
  int gblsym, outer_gblsym, i, j, sptr, dtype, cnt;
  UPLEVEL_PAIR *ptr;

  if (gbl.stbfil)
    return;
  if (gbl.internal > 1) {
    outer_gblsym = find_ag(get_llvm_name(gbl.outersub));
    gblsym = find_ag(get_llvm_name(gbl.currsub));

    AG_UPLEVEL_AVL(gblsym) = AG_UPLEVEL_AVL(outer_gblsym);
    AG_UPLEVEL_SZ(gblsym) = AG_UPLEVEL_SZ(outer_gblsym);
    NEW(ptr, UPLEVEL_PAIR, AG_UPLEVEL_SZ(gblsym));

    for (i = 0; i < AG_UPLEVEL_AVL(gblsym); i++) {
      if (AG_UPLEVEL_OLD(outer_gblsym, i)) {
        ptr[i].oldsptr = AG_UPLEVEL_OLD(outer_gblsym, i);
        ptr[i].newsptr = llvm_get_uplevel_newsptr(ptr[i].oldsptr);
        sptr = ptr[i].newsptr;
      } else {
        /* makeup something */
        if (sptr && CLENG(sptr)) {
          ptr[i].newsptr = CLENG(sptr);
        } else {
          ptr[i].newsptr = gethost_dumlen(sptr, 0);
          if (SCG(ptr[i].newsptr) == SC_DUMMY) {
            PASSBYVALP(ptr[i].newsptr, 1);
            ADDRTKNP(ptr[i].newsptr, 1);
            CLENP(sptr, ptr[i].newsptr);
          } else {
            SCP(ptr[i].newsptr, SC_LOCAL);
          }
        }
        sptr = 0;
      }
    }
    AG_UPLEVELPTR(gblsym) = ptr;
  } else if (gbl.internal) {
    gblsym = find_ag(get_ag_searchnm(gbl.currsub));
    ptr = AG_UPLEVELPTR(gblsym);
    cnt = AG_UPLEVEL_AVL(gblsym);
    for (i = 0, j = 0; i < AG_UPLEVEL_AVL(gblsym); i++, j++) {
      sptr = llvm_get_uplevel_newsptr(ptr[i].oldsptr);
      dtype = DTYPEG(sptr);
      if (DTYG(dtype) == TY_CHAR || DTYG(dtype) == TY_NCHAR ||
          (DTYG(dtype) == TY_PTR && DTY(dtype + 1) == TY_CHAR) ||
          (DTYG(dtype) == TY_PTR && DTY(dtype + 1) == TY_NCHAR)) {
        cnt++;
        if (ptr == AG_UPLEVELPTR(gblsym)) {
          (AG_UPLEVEL_SZ(gblsym))++;
          NEW((AG_UPLEVELPTR(gblsym)), UPLEVEL_PAIR, AG_UPLEVEL_SZ(gblsym));
          memcpy(AG_UPLEVELPTR(gblsym), ptr, sizeof(UPLEVEL_PAIR) * cnt);
        } else {
          NEED(cnt + 1, AG_UPLEVELPTR(gblsym), UPLEVEL_PAIR,
               AG_UPLEVEL_SZ(gblsym), (AG_UPLEVEL_SZ(gblsym) + 2));
        }
        AG_UPLEVEL_NEW(gblsym, j) = sptr;
        AG_UPLEVEL_OLD(gblsym, j) = ptr[i].oldsptr;
        j++;
        AG_UPLEVEL_OLD(gblsym, j) = 0;
        if (CLENG(sptr)) {
          AG_UPLEVEL_NEW(gblsym, j) = CLENG(sptr);
        } else {
          AG_UPLEVEL_NEW(gblsym, j) = getdumlen();
          if (SCG(sptr) == SC_DUMMY) {
            PASSBYVALP(AG_UPLEVEL_NEW(gblsym, j), 1);
            CLENP(sptr, AG_UPLEVEL_NEW(gblsym, j));
          } else {
            SCP(AG_UPLEVEL_NEW(gblsym, j), SC_LOCAL);
            CLENP(sptr, AG_UPLEVEL_NEW(gblsym, j));
          }
        }
      } else {
        AG_UPLEVEL_NEW(gblsym, j) = sptr;
        AG_UPLEVEL_OLD(gblsym, j) = ptr[i].oldsptr;
      }
    }
    if (ptr != AG_UPLEVELPTR(gblsym)) {
      AG_UPLEVEL_AVL(gblsym) = cnt;
      FREE(ptr);
      ptr = NULL;
    }
  }
}

static void
dump_uplevel_sptr(int gblsym)
{
  int i;
  for (i = 0; i < AG_UPLEVEL_AVL(gblsym); i++) {
    printf("oldsptr:%d newsptr:%d %s\n", AG_UPLEVEL_OLD(gblsym, i),
           AG_UPLEVEL_NEW(gblsym, i), get_llvm_name(AG_UPLEVEL_NEW(gblsym, i)));
  }
}

static int uplevelcnt = 0;
static int *upptr = NULL;

void
_add_llvm_uplevel_symbol(int oldsptr)
{
  int size;

  size = uplevelcnt;
  if (gbl.internal > 1) {
    if (uplevelcnt == 0) {
      NEW(upptr, int, 1);
    } else if (uplevelcnt + 1 >= size) {
      NEED(uplevelcnt + 1, upptr, int, size, size + 1);
    }
    upptr[uplevelcnt] = oldsptr;
    uplevelcnt++;
  }
}

void
add_aguplevel_oldsptr()
{
  if (gbl.internal > 1 && upptr) {
    add_uplevel_to_host(upptr, uplevelcnt);
    FREE(upptr);
    upptr = NULL;
    uplevelcnt = 0;
  }
}

void
load_uplevel_addresses(int display_temp)
{
  int i, gblsym, dtype, ilix, sym, dest_ilix, mem, basenm, oldsym, ld_ilix;
  if (gbl.internal == 1 && gbl.outlined && gbl.outersub)
    gblsym = find_ag(get_ag_searchnm(gbl.outersub));
  else
    gblsym = find_ag(get_ag_searchnm(gbl.currsub));
  if (!gblsym)
    return;
  dtype = DTYPEG(display_temp);
  if (DTY(dtype) != TY_STRUCT)
    dtype = make_uplevel_arg_struct();
  mem = DTY(dtype + 1);
  for (i = 0; i < AG_UPLEVEL_AVL(gblsym) && mem > NOSYM; i++) {
    sym = AG_UPLEVEL_NEW(gblsym, i);
    oldsym = AG_UPLEVEL_OLD(gblsym, i);
    ilix = mk_address(sym);

    if (SCG(sym) == SC_PRIVATE) {
      /* host routine should not do anything with SC_PRIVATE
       * Outlined function should only load if variable is
       * local to its outlined function.
       */
      if (!gbl.outlined || (!is_llvm_local_private(sym))) {
        mem = SYMLKG(mem);
        continue;
      }
    } else if (gbl.outlined) {
      /* Don't load shared variable from host program if we are in outlined
       * function.  Host program should already loaded the addresses.
       */
      mem = SYMLKG(mem);
      continue;
    }

    dest_ilix = ad_acon(display_temp, ADDRESSG(mem));

    if (oldsym == 0) {
      /* character len by value */
      basenm = addnme(NT_VAR, display_temp, 0, (INT)0);
      ld_ilix = ad3ili(IL_LDKR, ilix, addnme(NT_VAR, sym, 0, (INT)0), MSZ_I8);
      ilix = ad4ili(IL_STKR, ld_ilix, dest_ilix,
                    addnme(NT_MEM, mem, basenm, (INT)0), MSZ_I8);
      goto cont;
    }
    if (SCG(sym) == SC_DUMMY && !PASSBYVALG(sym)) {
      ilix = mk_address(sym);
    }

    basenm = addnme(NT_VAR, display_temp, 0, (INT)0);
    ilix = ad3ili(IL_STA, ilix, dest_ilix, addnme(NT_MEM, mem, basenm, (INT)0));
  cont:
    chk_block(ilix);
    mem = SYMLKG(mem);
  }
}

int
get_sptr_uplevel_address(int sptr)
{
  int i, gblsym;
  gblsym = find_ag(get_ag_searchnm(gbl.currsub));
  for (i = 0; i < AG_UPLEVEL_AVL(gblsym); i++) {
    if (sptr == AG_UPLEVEL_NEW(gblsym, i)) {
      return AG_UPLEVEL_MEM(gblsym, i);
    }
  }
  return 0;
}

int
ll_shallow_copy_uplevel(int hostsptr, int olsptr)
{
  /* copy information from the internal subprogram to the outlined program */

  int hostgbl, olgbl;
  hostgbl = find_ag(get_llvm_name(hostsptr));
  olgbl = find_ag(get_llvm_name(olsptr));

  AG_UPLEVELPTR(olgbl) = AG_UPLEVELPTR(hostgbl);
  AG_UPLEVEL_AVL(olgbl) = AG_UPLEVEL_AVL(hostgbl);
  return 0;
}

char *
get_ag_name(int gblsym)
{
  return AG_NAME(gblsym);
}

void
assem_dinit(void)
{
  /* intentionally empty */
}

