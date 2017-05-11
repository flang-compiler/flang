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

/**
   \file
   \brief FTN Semantic action routines to resolve symbol references as
   to overloading class.  This module hides the walking of hash chains
   and overloading class checks.
 */

#include "gbldefs.h"
#include "error.h"
#include "global.h"
#include "symtab.h"
#include "semant.h"

/**
   \brief Look up symbol having a specific symbol type.

   If a symbol is found in the same overloading class and has the same
   symbol type, it is returned to the caller.  If a symbol is found in
   the same overloading class, the action of declref depends on the
   stype of the existing symbol and value of the argument def:
   1.  if symbol is an unfrozen intrinsic and def is 'd' (define), its
       intrinsic property is removed and a new symbol is declared,
   2.  if def is 'd', a multiple declaration error occurs, or
   3.  if def is not 'd', an 'illegal use' error occurs.

   If an error occurs or a matching symbol is not found, one is
   created and its symbol type is initialized.
 */
int
declref(int sptr, int stype, int def)
{
  register int st, sptr1, first;

  first = sptr;
  do {
    st = STYPEG(sptr);
    if (st == ST_UNKNOWN)
      goto return1; /* stype not set yet, set it */
    if (stb.ovclass[st] == stb.ovclass[stype]) {
      if (stype != st) {
        if (def == 'd') {
          /* Redeclare of intrinsic symbol is okay unless frozen */
          if (IS_INTRINSIC(st)) {
            if ((sptr1 = newsym(sptr)) != 0)
              sptr = sptr1;
            goto return1;
          }
          /* multiple declaration */
          error(44, 3, gbl.lineno, SYMNAME(first), CNULL);
        } else
          /* illegal use of symbol */
          error(84, 3, gbl.lineno, SYMNAME(first), CNULL);
        break;
      }
      goto return2; /* found, return it */
    }
    sptr = HASHLKG(sptr);
  } while (sptr && NMPTRG(sptr) == NMPTRG(first));

  sptr = insert_sym(first); /* create new one if def or illegal use */
return1:
  STYPEP(sptr, stype);
return2:
  if (flg.xref)
    xrefput(sptr, def);
  return sptr;
}

/**
   \brief Declare a new symbol.

   An error can occur if the symbol is already in the symbol table:
   - if the symbol types match treat as an error if 'errflg' is true
     otherwise its okay and return symbol to caller
   - else if symbol is an intrinsic attempt to remove symbol's
     intrinsic property otherwise it is an error
 */
int
declsym(int sptr, int stype, LOGICAL errflg)
{
  register int st, sptr1, first;

  first = sptr;
  do {
    st = STYPEG(sptr);
    if (st == ST_UNKNOWN)
      goto return1; /* Brand new symbol, return it. */
    if (st == ST_IDENT && stb.ovclass[st] == stb.ovclass[stype])
      goto return1; /* Found sym in same overloading class */
    if (stb.ovclass[st] == stb.ovclass[stype]) {
      if (stype == st) {
        /* Possible attempt to multiply define symbol */
        if (errflg) {
          error(44, 3, gbl.lineno, SYMNAME(first), CNULL);
          break;
        } else
          goto return2;
      } else {
        /* Redeclare of intrinsic symbol is okay unless frozen */
        if (IS_INTRINSIC(st)) {
          if ((sptr1 = newsym(sptr)) != 0)
            sptr = sptr1;
          goto return1;
        } else {
          error(43, 3, gbl.lineno, "symbol", SYMNAME(first));
          break;
        }
      }
    }
    sptr = HASHLKG(sptr);
  } while (sptr && NMPTRG(sptr) == NMPTRG(first));

  sptr = insert_sym(first); /* create new one if def or illegal use */
return1:
  STYPEP(sptr, stype);
return2:
  if (flg.xref)
    xrefput(sptr, 'd');
  return sptr;
}

/**
   \brief Look up a symbol having the given overloading class.

   If the symbol with the overloading class is found its sptr is
   returned.  If no symbol with the given overloading class is found,
   a new sptr is returned.  (If scoping becomes a Fortran feature,
   this routine will implement it)
 */
int
refsym(int sptr, int oclass)
{
  register int st, first;

  first = sptr;
  do {
    st = STYPEG(sptr);
    if (st == ST_UNKNOWN)
      goto return1;
    if (stb.ovclass[st] == oclass)
      goto returnit;
    sptr = HASHLKG(sptr);
  } while (sptr && NMPTRG(sptr) == NMPTRG(first));

  /* Symbol in given overloading class not found, create new one */
  sptr = insert_sym(first);
return1:
  if (flg.xref)
    xrefput(sptr, 'd');
returnit:
  return sptr;
}

/**
   \brief Look up symbol matching overloading class of given symbol
   type.

   The sptr is returned for the symbol whose overloading class matches
   the overloading class of the symbol type given.  If no symbol is
   found in the given overloading class one is created.  (If scoping
   becomes a Fortran feature, this routine will not use it)
 */
int
getocsym(int sptr, int oclass)
{
  register int st, first;

  first = sptr;
  do {
    st = STYPEG(sptr);
    if (st == ST_UNKNOWN)
      goto return1;
    if (stb.ovclass[st] == oclass)
      goto returnit; /* found it! */
    sptr = HASHLKG(sptr);
  } while (sptr && NMPTRG(sptr) == NMPTRG(first));

  /* create new symbol if undefined or illegal use */
  sptr = insert_sym(first);
return1:
  if (flg.xref)
    xrefput(sptr, 'd');
returnit:
  return sptr;
}

/**
   \brief Reset fields of intrinsic or generic symbol, sptr, to zero
   in preparation for changing its symbol type by the Semantic
   Analyzer. If the symbol type of the symbol has been 'frozen', issue
   an error message and notify the caller by returning a zero symbol
   pointer.
 */
int
newsym(int sptr)
{
  int sp2;

  if (EXPSTG(sptr)) {
    /* Symbol previously frozen as an intrinsic */
    error(43, 3, gbl.lineno, "intrinsic", SYMNAME(sptr));
    return 0;
  }
  /*
   * try to find another sym in the same overloading class; we need to
   * try this first since there could be multiple occurrences of an
   * intrinsic and therefore the sptr appears more than once in the
   * semantic stack.  E.g.,
   *    call sub (sin, sin)
   * NOTE that in order for this to work we need to perform another getsym
   * to start at the beginning of the hash links for symbols whose names
   * are the same.
   */
  sp2 = getsym(LOCAL_SYMNAME(sptr), strlen(SYMNAME(sptr)));
  sp2 = getocsym(sp2, OC_OTHER);
  if (sp2 != sptr)
    return sp2;
  /*
   * create a new symbol with the same name:
   */
  error(35, 1, gbl.lineno, SYMNAME(sptr), CNULL);
  sp2 = insert_sym(sptr);

  /* transfer dtype if it was explicitly declared for sptr:  */

  if (DCLDG(sptr)) {
    DTYPEP(sp2, DTYPEG(sptr));
    DCLDP(sp2, 1);
  }

  return sp2;
}

/**
   \brief Reference a symbol when it's known the context requires an
   identifier.  If an error occurs (e.g., symbol which is frozen as an
   intrinsic), a new symbol is created so that processing can
   continue.  If the symbol found is ST_UNKNOWN, its stype is changed
   to ST_IDENT.
 */
int
ref_ident(int sptr)
{
  int sym;

  sym = refsym(sptr, OC_OTHER);
  if (IS_INTRINSIC(STYPEG(sym))) {
    sym = newsym(sym);
    if (sym == 0)
      sym = insert_sym(sptr);
  }
  if (STYPEG(sym) == ST_UNKNOWN)
    STYPEP(sym, ST_IDENT);

  return sym;
}

