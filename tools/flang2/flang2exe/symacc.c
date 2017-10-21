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

/********************************************************
  FIXME: get rid of this "important notice" and proliferating copies.

             I M P O R T A N T   N O T I C E
       Do not modify this file if it resides in the
       directory src -- modify the copy which is in
       ../utils/symtab and then run copy.sh

********************************************************/
/**
   \file
   \brief Generic access module.  Used by all compilers and
   initialization utility.
 */

/* FIXME: This file is compiled with different gbldefs.h included
   depending on in which part of the build it is recompiled. */
#include "scutil.h"
#include "gbldefs.h"
#include "global.h"
#include "error.h"
#include "symtab.h"
#include <stdarg.h>

#ifndef STANDARD_MAXIDLEN
#define STANDARD_MAXIDLEN MAXIDLEN
#endif

void
sym_init_first(void)
{
  int i;

  int sizeof_SYM = sizeof(SYM) / sizeof(INT);
  //assert(sizeof_SYM == 36, "bad SYM size", sizeof_SYM, 4);

  if (stb.stg_base == NULL) {
    stb.stg_size = 1000;
    NEW(stb.stg_base, SYM, stb.stg_size);
    BZERO(stb.stg_base, SYM, stb.stg_size);
    assert(stb.stg_base, "sym_init: no room for symtab", stb.stg_size, 4);
    stb.n_size = 5024;
    NEW(stb.n_base, char, stb.n_size);
    assert(stb.n_base, "sym_init: no room for namtab", stb.n_size, 4);
    stb.n_base[0] = 0;
    stb.dt_size = 400;
    NEW(stb.dt_base, ISZ_T, stb.dt_size);
    assert(stb.dt_base, "sym_init: no room for dtypes", stb.dt_size, 4);
    stb.w_size = 32;
    NEW(stb.w_base, INT, stb.w_size);
    assert(stb.w_base, "sym_init: no room for wtab", stb.w_size, 4);
  }

  stb.stg_avail = 1;
  stb.namavl = 1;
  stb.wrdavl = 0;
  for (i = 0; i <= HASHSIZE; i++)
    stb.hashtb[i] = 0;

}

/** \brief Expand symbol storage area when NEWSYM runs out of area.

    It is assumed that stb.stg_avail is 1 more than the index of the current
    symbol being created. */
void
realloc_sym_storage()
{
  unsigned n;
  DEBUG_ASSERT(stb.stg_avail > stb.stg_size,
               "realloc_sym_storage: call only if necessary");
  if (stb.stg_avail > SPTR_MAX + 1 || stb.stg_base == NULL)
    symini_errfatal(7);
  /* Use unsigned arithmetic to avoid risk of overflow. */
  DEBUG_ASSERT(stb.stg_size > 0,
               "realloc_sym_storage: symbol storage not initialized?");
  n = 2u * stb.stg_size;
  if (n > SPTR_MAX + 1)
    n = SPTR_MAX + 1;
  NEED(stb.stg_avail, stb.stg_base, SYM, stb.stg_size, n);
  DEBUG_ASSERT(stb.stg_avail <= stb.stg_size, "realloc_sym_storage: internal error");
}

/**
   \brief Look up symbol with indicated name.

   \return If there is already such a symbol, the pointer to the
   existing symbol table entry; or 0 if a symbol doesn't exist.
   \param name is a symbol name.
   \param olength is the number of characters in the symbol name.
 */
SPTR
lookupsym(const char *name, int olength)
{
  int length;
  SPTR sptr;     /* pointer to symbol table entry */
  INT hashval;   /* index into hashtb. */

  /*
   * Loop thru the appropriate hash link list to see if symbol is
   * already in the table:
   */

  length = olength;
  if (length > MAXIDLEN) {
    length = MAXIDLEN;
  }
  HASH_ID(hashval, name, length);
  for (sptr = stb.hashtb[hashval]; sptr != 0; sptr = HASHLKG(sptr)) {
    if (strncmp(name, SYMNAME(sptr), length) != 0 ||
        *(SYMNAME(sptr) + length) != '\0')
      continue;

    /* matching entry has been found in symbol table. return it: */

    return sptr;
  }

  return 0;
} /* lookupsym */

/** \brief Issue diagnostic for identifer that is too long.

    \param name - identifier (without terminating njull)
    \param olength - length of identifier
    \param max_idlen - maximum allowed length

    Though this routine has only one lexical call site, it is factored
    out to not clutter the common path in installsym_ex.
  */
static void
report_too_long_identifier(const char *name, int olength, int max_idlen)
{
  static char *ebuf;
  static int ebuf_sz = 0;
  char len_buf[12];
  if (ebuf_sz == 0) {
    ebuf_sz = olength + 1;
    NEW(ebuf, char, ebuf_sz);
  } else {
    int ii;
    NEED(olength + 1, ebuf, char, ebuf_sz, olength + 1);
    ii = strlen(ebuf);
    if (ii < olength)
      strcpy(ebuf + (ii - 2), "..."); /* there's room for at least 1 '.'*/
  }
  memcpy(ebuf, name, olength);
  ebuf[olength] = '\0';
  sprintf(len_buf, "%d", max_idlen);
  symini_error(16, 2, gbl.lineno, ebuf, len_buf);
}

/**
   \brief Get the symbol table index for a NUL-terminated name.
 */
SPTR
lookupsymbol(const char *name)
{
  return lookupsym(name, strlen(name));
}

/**
   \brief Construct a name via printf-style formatting and then
   look it up in the symbol table via lookupsymbol().
 */
SPTR
lookupsymf(const char *fmt, ...)
{
  char buffer[MAXIDLEN + 1];
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(buffer, sizeof buffer - 1, fmt, ap);
  va_end(ap);
  buffer[sizeof buffer - 1] = '\0'; /* Windows workaround */
  return lookupsymbol(buffer);
}

/**
   \brief Enter symbol with indicated name into symbol table,
   initialize the new entry, and return pointer to it.  If there is
   already such a symbol, just return pointer to the existing symbol
   table entry.

   \param name is the symbol name.
   \param olength is the number of characters in the symbol name.
 */
SPTR
installsym_ex(const char *name, int olength, IS_MODE mode)
{
  int length;
  SPTR sptr;     /* pointer to symbol table entry */
  INT hashval;   /* index into hashtb. */
  bool toolong;
  int nmptr;
  static int max_idlen = MAXIDLEN;

  /*
   * Trim identifier if it is too long.
   */
  toolong = false;
  length = olength;
  if (flg.standard) {
    max_idlen = 31;
  }
  if (length > max_idlen) {
    length = max_idlen;
    toolong = true;
  }

  nmptr = 0;
  if (mode != IS_QUICK) {
    /*
     * Loop thru the appropriate hash link list to see if symbol is
     * already in the table.
     */
    HASH_ID(hashval, name, length);
    for (sptr = stb.hashtb[hashval]; sptr != 0; sptr = HASHLKG(sptr)) {
      const char *sname;
      int np = NMPTRG(sptr);
      if (np + length >= stb.namavl)
        continue;
      sname = stb.n_base + np;
      if (sname[0] != name[0] || sname[length] != '\0')
        continue;
      if (strncmp(name, sname, length) != 0)
        continue;
      nmptr = np;

      /* Matching entry has been found in symbol table. Return it. */

      return sptr;
    }
  }

  /* Symbol not found.  Create a new symbol table entry. */

  NEWSYM(sptr);
  if (mode != IS_QUICK) {
    LINKSYM(sptr, hashval);
  }

  if (!nmptr)
    nmptr = putsname(name, length);
  NMPTRP(sptr, nmptr);
  SYMLKP(sptr, NOSYM);
#ifdef LINENOP
  LINENOP(sptr, gbl.lineno);
#endif

  if (toolong) {
    report_too_long_identifier(name, olength, max_idlen);
  }

  return sptr;
}

/**
   \brief Put a string of characters into the symbol names storage
   area and return pointer to the string (relative to
   stb.n_base). This routine is used to enter both symbol names and
   character string constants.

   \param name are the characters to be entered.
   \param length is the number of characters to be entered.
 */
int
putsname(const char *name, int length)
{
  int nptr; /* index into character storage area */
  char *np; /* pointer into character storage area */
  int i;    /* counter */

  nptr = stb.namavl;
  stb.namavl += (length + 1);
  while (stb.namavl > stb.n_size) {
    /* To avoid quadratic behavior, we increase the storage area size
       by a factor, not a constant.  Use unsigned arithmetic here
       to avoid risk of overflow. */
    unsigned n = 2u * stb.n_size;
    if (n > MAX_NMPTR) {
      n = MAX_NMPTR;
      if (stb.namavl > n)
        symini_errfatal(7); /* names table overflow */
    }
    NEED(stb.namavl, stb.n_base, char, stb.n_size, n);
  }
  np = stb.n_base + nptr;
  for (i = 0; i < length; i++)
    *np++ = *name++;
  *np = '\0';

  return nptr;
}

/**
   \brief Create a local copy of a name known to be stored in the 'stb.n_base'
   area.

   Used when a symbol needs to be created from a name stored in the
   area; a purify umr error could occur if the area is realloc'd.  The
   char pointer to the copy is returned.
 */
char *
local_sname(char *name)
{
  static char *safe_p;
  static int safe_sz = 0;
  int length;

  length = strlen(name) + 2 + 6; /* MW: add one more character,
                                    needed in semfunc2 */
  /* Hongyon: add 6 more for
     _cr and _nm for cref,nomixed */
  if (safe_sz == 0) {
    safe_sz = length + 100;
    NEW(safe_p, char, safe_sz);
  } else {
    NEED(length, safe_p, char, safe_sz, length + 100);
  }

  strcpy(safe_p, name);

  return safe_p;
}

void
add_fp_constants(void)
{
  INT tmp[4];
  INT res[4];

  tmp[0] = 0;
  atoxf("0.0", &tmp[1], 3);
  /***** the f90 backend *****/
  stb.flt0 = getcon(tmp, DT_REAL);
  atoxf("1.0", &tmp[1], 3);
  stb.flt1 = getcon(tmp, DT_REAL);
  atoxf("2.0", &tmp[1], 3);
  stb.flt2 = getcon(tmp, DT_REAL);
  atoxf("0.5", &tmp[1], 3);
  stb.flthalf = getcon(tmp, DT_REAL);

  atoxd("0.0", &tmp[0], 3);
  stb.dbl0 = getcon(tmp, DT_DBLE);
  atoxd("1.0", &tmp[0], 3);
  stb.dbl1 = getcon(tmp, DT_DBLE);
  atoxd("2.0", &tmp[0], 3);
  stb.dbl2 = getcon(tmp, DT_DBLE);
  atoxd("0.5", &tmp[0], 3);
  stb.dblhalf = getcon(tmp, DT_DBLE);

  tmp[0] = 0;
  res[0] = 0;
  tmp[1] = CONVAL2G(stb.flt0);
  xfneg(tmp[1], &res[1]);
  stb.fltm0 = getcon(res, DT_REAL);
  tmp[0] = CONVAL1G(stb.dbl0);
  tmp[1] = CONVAL2G(stb.dbl0);
  xdneg(tmp, res);
  stb.dblm0 = getcon(res, DT_DBLE);

#ifdef LONG_DOUBLE_FLOAT128
  atoxq("0.0", &tmp[0], 4);
  stb.float128_0 = getcon(tmp, DT_FLOAT128);
  xqneg(tmp, res);
  stb.float128_m0 = getcon(res, DT_FLOAT128);
  atoxq("1.0", &tmp[0], 4);
  stb.float128_1 = getcon(tmp, DT_FLOAT128);
  atoxq("0.5", &tmp[0], 4);
  stb.float128_half = getcon(tmp, DT_FLOAT128);
  atoxq("2.0", &tmp[0], 4);
  stb.float128_2 = getcon(tmp, DT_FLOAT128);
#endif
}

LOGICAL
is_flt0(SPTR sptr)
{
  if (sptr == stb.flt0 || sptr == stb.fltm0)
    return TRUE;
  return FALSE;
}

LOGICAL
is_dbl0(SPTR sptr)
{
  if (sptr == stb.dbl0 || sptr == stb.dblm0)
    return TRUE;
  return FALSE;
}

LOGICAL
is_quad0(SPTR sptr)
{
  if (sptr == stb.quad0 || sptr == stb.quadm0)
    return TRUE;
  return FALSE;
}

#ifdef LONG_DOUBLE_FLOAT128
is_float128_0(SPTR sptr)
{
  return sptr == stb.float128_0 || sptr == stb.float128_m0;
}
#endif /* LONG_DOUBLE_FLOAT128 */

LOGICAL
is_cmplx_flt0(SPTR sptr)
{
  if (CONVAL1G(sptr) == CONVAL2G(stb.flt0) ||
      CONVAL1G(sptr) == CONVAL2G(stb.fltm0)) {
    if (CONVAL2G(sptr) == CONVAL2G(stb.flt0) ||
        CONVAL2G(sptr) == CONVAL2G(stb.fltm0)) {
      return TRUE;
    }
  }
  return FALSE;
}

LOGICAL
is_creal_flt0(SPTR sptr)
{
  if (CONVAL1G(sptr) == CONVAL2G(stb.flt0) ||
      CONVAL1G(sptr) == CONVAL2G(stb.fltm0))
    return TRUE;
  return FALSE;
}

LOGICAL
is_cimag_flt0(SPTR sptr)
{
  if (CONVAL2G(sptr) == CONVAL2G(stb.flt0) ||
      CONVAL2G(sptr) == CONVAL2G(stb.fltm0))
    return TRUE;
  return FALSE;
}

LOGICAL
is_cmplx_dbl0(SPTR sptr)
{
  if (is_dbl0(CONVAL1G(sptr)) && is_dbl0(CONVAL2G(sptr)))
    return TRUE;
  return FALSE;
}

LOGICAL
is_cmplx_quad0(SPTR sptr)
{
  if (is_quad0(CONVAL1G(sptr)) && is_quad0(CONVAL2G(sptr)))
    return TRUE;
  return FALSE;
}

STB stb;
GBL gbl;
static char buff[132];
void
symini_errfatal(int n)
{
  errfatal(n);
}

void
symini_error(int n, int s, int l, const char *c1, const char *c2)
{
  error(n, s, l, c1, c2);
}

void
symini_interr(const char *txt, int val, int sev)
{
  char buff[8];

  sprintf(buff, "%7d", val);
  symini_error(0, sev, gbl.lineno, txt, buff);
}
