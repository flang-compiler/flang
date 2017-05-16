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
 *  \brief names table utility module
 */

#include "gbldefs.h"
#include "global.h"
#include "error.h"
#include "symtab.h"
#include "nme.h"
#include "expand.h"

static LOGICAL found_rpct(int rpct_nme1, int rpct_nme2);

#if DEBUG
#define asrt(c) \
  if (c)        \
    ;           \
  else          \
  fprintf(stderr, "asrt failed. line %d, file %s\n", __LINE__, __FILE__)
#else
#define asrt(c)
#endif

#define NMEHSZ 1217
#define MAXNME 67108864

static int nmehsh[NMEHSZ];
static int ptehsh[NMEHSZ];
static int rpcthsh[NMEHSZ];

/** \brief Query whether the given nme is a PRE temp. */
LOGICAL
is_presym(int nme)
{
  if (nme && (NME_TYPE(nme) == NT_VAR)) {
    SPTR sptr = NME_SYM(nme);

    if ((sptr > 0) && CCSYMG(sptr) && SYMNAME(sptr) &&
        (strncmp(SYMNAME(sptr), ".pre", 4) == 0))
      return TRUE;
  }

  return FALSE;
}

/** \brief Initialize names module
 */
void
nme_init(void)
{
  int i;
  static int firstcall = 1;

  STG_ALLOC(nmeb, NME, 128);
  nmeb.stg_avail = 2; /* 0, NME_UNK; 1, NME_VOL */
  STG_CLEAR_ALL(nmeb);

  NME_TYPE(NME_UNK) = NT_UNK;

  NME_TYPE(NME_VOL) = NT_UNK;
  NME_SYM(NME_VOL) = 1;

  if (firstcall)
    firstcall = 0;
  else {
    for (i = 0; i < NMEHSZ; i++) {
      nmehsh[i] = 0;
      ptehsh[i] = 0;
      rpcthsh[i] = 0;
    }
  }

  STG_ALLOC(nmeb.pte, PTE, 128);
  PTE_NEXT(PTE_UNK) = PTE_END;
  PTE_TYPE(PTE_UNK) = PT_UNK;
  PTE_VAL(PTE_UNK) = 0;

  STG_ALLOC(nmeb.rpct, RPCT, 128);

} /* nme_init */

void
nme_end(void)
{
  STG_DELETE(nmeb);

  STG_DELETE(nmeb.pte);

  STG_DELETE(nmeb.rpct);
} /* nme_end */

/*
 * for F90:
 *   type dt
 *     integer :: i, j
 *   end type
 *   type, extends(dt)::et
 *     integer :: k
 *   end type
 * type(et) :: m
 * a reference to m%i may appear in the ILMs as
 * IM_MEMBER(IM_BASE(m),IM_MEMBER(i))
 * but in the datatype table as
 *   dt-> TY_STRUCT ( i(integer), j(integer) )
 *   et-> TY_STRUCT ( dt(dt), k(integer) )
 * so the datatype table has an extra member, named dt, for the datatype dt
 * we want to insert the reference to %dt%i here
 */
static int
find_parent_member(int nmex, DTYPE dt, int sym)
{
  int mem, newnmex;
  SPTR sptr;
  mem = DTY(dt + 1);
  if (mem > NOSYM && PARENTG(mem)) {
    int dtmem = DTYPEG(mem);
    for (sptr = DTY(dtmem + 1); sptr > NOSYM; sptr = SYMLKG(sptr)) {
      if (sym == sptr)
        break;
    }
    newnmex = add_arrnme(NT_MEM, mem, nmex, 0, 0, 0);
    if (mem > NOSYM) {
      return newnmex;
    } else {
      return find_parent_member(newnmex, dtmem, sym);
    }
  }
  return 0;
} /* find_parent_member */

/**
   \brief Main add NME routine

   Enter nme into the NME area.

<pre>
  the type NT_INDARR is used to distinguish the cases:
    typedef float* fp;
    typedef float f10[10];
    fp* ppf;		// pointer to pointer to float
    fp a10pf[10];	// array of 10 pointers to float
    f10* pa10f;	// pointer to array of 10 floats
   ppf[i][j]		// appears as IM_ELEMENT -> IM_PLD -> IM_ELEMENT ->
  IM_PLD -> ppf
   a10pf[i][j]		// appears as IM_ELEMENT -> IM_PLD -> IM_ELEMENT ->
  a10pf
   pa10f[i][j]		// appears as IM_ELEMENT -> IM_ELEMENT -> IM_PLD ->
  pa10f
  NT_INDARR is used when an IM_ELEMENT -> IM_PLD appears in the ILM file.
</pre>
 */
int
add_arrnme(NT_KIND type, SPTR insym, int nm, ISZ_T cnst, int sub, LOGICAL inlarr)
{
  int val, i;
  DTYPE nmdt;
  int sym;

  if (EXPDBG(10, 256))
    fprintf(gbl.dbgfil,
            "type = %d sym = %d nm = %d cnst = %" ISZ_PF "d sub=%d inlarr=%d\n",
            type, insym, nm, cnst, sub, inlarr);
#if DEBUG
  assert(inlarr == FALSE || inlarr == TRUE, "add_arrnme: bad inlarr", inlarr,
         3);
#endif

  /* evaluate nme and fold any values  */

  if ((int)insym < 0) {
    DEBUG_ASSERT(insym == NME_NULL, "add_arrnme: bad negative insym");
  } else {
    DEBUG_ASSERT(insym <= SPTR_MAX, "add_arrnme: out of bounds insym");
  }
  sym = insym;

  switch (type) {

  case NT_SAFE:
  case NT_VAR:
  case NT_IND:
    break;

  case NT_ARR:
#ifdef NT_INDARR
  case NT_INDARR:
#endif
    if (nm == NME_VOL)
      return NME_VOL;
    { /* tpr 564:
       * for a subscripted reference, ensure the 'base' is not
       * a union or structure.
       */
      DTYPE dt = dt_nme(nm);
      if (DTY(dt) == TY_UNION)
        return nm;
    }
    break;

  case NT_MEM:
    /*
     * don't create union NMEs for fortran; fortran's conflict() does not
     * have the left-to-right walk of the nmes on which checking of unions
     * depends.
     */
    nmdt = dt_nme(nm);
    if (DTY(nmdt) == TY_STRUCT
        || DTY(nmdt) == TY_DERIVED
        ) {
      if (sym > 0) {
#if defined(INLNG)
        if (!INLNG(sym))
#else
        if (1)
#endif
        {
          for (i = DTY(nmdt + 1); i > NOSYM; i = SYMLKG(i)) {
            if (sym == i)
              goto is_member;
          }
          {
            int nnm;
            /* look if this is an extended member */
            nnm = find_parent_member(nm, nmdt, sym);
            if (nnm) {
              nm = nnm;
              goto is_member;
            }
          }
        } else {
          /* the member names are inlined separately;
           * the datatype gets duplicated.
           * allow for this */
          for (i = DTY(nmdt + 1); i > NOSYM; i = SYMLKG(i)) {
            if (sym == i || (strcmp(SYMNAME(i), SYMNAME(sym)) == 0 &&
                             ADDRESSG(i) == ADDRESSG(sym))) {
              sym = i;
              goto is_member;
            }
          }
        }
      }
    }
    if (nm == NME_VOL || sym > 1) {
      return (nm);
    }
    break; /* => real/imag (sym = 0/1) parts of complex */
  is_member:
    break;

  case NT_ADD:
    switch (NME_TYPE(nm)) {

    case NT_VAR:
    case NT_MEM:
    case NT_ARR:
      /*
       * attempt to create an array names entry for these cases. This
       * is only done when nm is for an item which is an array.
       */
      if (DTY(dt_nme(nm)) != TY_ARRAY) {

        /*
         * since nm is not for an array, it is determine if the
         * amount being added is zero.
         */
        if (sym == 0 && cnst == 0) {
          return (nm); /* go ahead and use nm */
        }
        /*
         * this occurs when adding a nonzero or variable value to
         * an & expression; i.e., &x + 4, &x + i -- it is not
         * known what is actually being referenced.  The addrtkn
         * flag is set for the variable in nm and the unknown
         * names entry is returned.
         */
        loc_of(nm);
        return (NME_UNK);
      }
      type = NT_ARR; /* create an array names entry  */
      break;

    case NT_IND:
      if (sym == 0) {
        if (NME_SYM(nm) == 0) {
          cnst += NME_CNST(nm);
          nm = NME_NM(nm);
        } else
          return (nm);
      } else {
        nm = NME_NM(nm);
        cnst = 0;
      }
      type = NT_IND;
      sub = 0;
      inlarr = 0;
      break;

    case NT_UNK:
      return nm;
    }
    break;
  case NT_UNK:
    return (NME_UNK);
  }

  /* compute the hash index for this NME  */
  val = (int)((type ^ sym ^ nm ^ sub) & 0x7fff) % NMEHSZ;

  /* search the hash links for this NME  */
  for (i = nmehsh[val]; i != 0; i = NME_HSHLNK(i))
    if (NME_TYPE(i) == type && NME_INLARR(i) == inlarr && NME_SYM(i) == sym &&
        NME_NM(i) == nm && NME_CNST(i) == cnst && NME_SUB(i) == sub &&
#ifdef NME_PTE
        NME_PTE(i) == 0 &&
#endif
        NME_RPCT_LOOP(i) == 0)
      return (i); /* F O U N D  */

  /*
   * N O T   F O U N D -- if no more storage is available, try to get more
   * storage
   */
  i = STG_NEXT(nmeb);
  if (i > MAXNME)
    error(7, 4, 0, CNULL, CNULL);
  /*
   * NEW ENTRY - add the nme to the nme area and to its hash chain
   */
  if (EXPDBG(10, 256))
    fprintf(gbl.dbgfil,
            "adding nme %d:type = %d sym = %d nm = %d cnst = %" ISZ_PF
            "d sub = %d, inlarr=%d\n",
            i, type, sym, nm, cnst, sub, inlarr);
  NME_TYPE(i) = type;
  NME_INLARR(i) = inlarr;
  NME_SYM(i) = sym;
  NME_NM(i) = nm;
  NME_CNST(i) = cnst;
  NME_HSHLNK(i) = nmehsh[val];
  NME_SUB(i) = sub;
  nmehsh[val] = i;
  return (i);
}

int
lookupnme(NT_KIND type, int insym, int nm, ISZ_T cnst)
{
  int i, val, sub = 0, sym = insym;
  LOGICAL inlarr = FALSE;
  if (insym < 0)
    sym = NME_NULL;

  val = (int)((type ^ sym ^ nm ^ sub) & 0x7fff) % NMEHSZ;
  for (i = nmehsh[val]; i != 0; i = NME_HSHLNK(i)) {
    if (NME_TYPE(i) == type && NME_INLARR(i) == inlarr && NME_SYM(i) == sym &&
        NME_NM(i) == nm && NME_CNST(i) == cnst && NME_SUB(i) == sub &&
#ifdef NME_PTE
        NME_PTE(i) == 0 &&
#endif
        NME_RPCT_LOOP(i) == 0)
      return (i); /* F O U N D  */
  }
  return 0;
} /* lookupnme */

/** \brief Add a new nme based on this nme with a new PTE list; otherwise all
           fields the same.
 */
int
add_nme_with_pte(int nm, int ptex)
{
#ifndef NME_PTE
  return nm;
#else
  int val, i;

  val =
      (int)((NME_TYPE(nm) ^ NME_SYM(nm) ^ NME_NM(nm) ^ NME_SYM(nm)) & 0x7fff) %
      NMEHSZ;
  for (i = nmehsh[val]; i > 0; i = NME_HSHLNK(i)) {
    if (NME_TYPE(i) == NME_TYPE(nm) && NME_INLARR(i) == NME_INLARR(nm) &&
        NME_SYM(i) == NME_SYM(nm) && NME_NM(i) == NME_NM(nm) &&
        NME_CNST(i) == NME_CNST(nm) && NME_SUB(i) == NME_SUB(nm) &&
        NME_RPCT_LOOP(i) == NME_RPCT_LOOP(nm) && NME_PTE(i) == ptex)
      return (i); /* F O U N D  */
  }
  i = STG_NEXT(nmeb);
  if (i > MAXNME)
    error(7, 4, 0, CNULL, CNULL);
  if (EXPDBG(10, 256))
    fprintf(gbl.dbgfil, "adding based nme %d, based on %d with pte %d\n", i, nm,
            ptex);
  BCOPY(nmeb.stg_base + i, nmeb.stg_base + nm, NME, 1);
  NME_BASE(i) = nm;
  NME_PTE(i) = ptex;
  NME_HSHLNK(i) = nmehsh[val];
  nmehsh[val] = i;
  return i;
#endif
} /* add_nme_with_pte */

/**
 * This creates and returns a new NME, 'rpct_nme', whose fields are
 * the same as those of 'orig_nme' except that it has
 * NME_RPCT_LOOP( rpct_nme ) == rpct_loop (> 0), whereas
 * NME_RPCT_LOOP( orig_nme ) == 0.  'rpct_nme' will replace all
 * occurrences of 'orig_nme' within the loop denoted by 'rpct_loop',
 * whereas 'orig_nme' will continue to be used elsewhere.  This loop
 * is guarded by runtime pointer conflict tests ('RPCT's) which prove
 * at runtime that the 'rpct_nme' references don't conflict with
 * various other references in the loop that they could potentially
 * conflict with, as indicated in the 'RPCT table', and 'conflict()'
 * will use this information to return NOCONFLICT for pairs of such
 * references.
 */
int
add_rpct_nme(int orig_nme, int rpct_loop)
{
  int hashval, rpct_nme;

  asrt(NME_RPCT_LOOP(orig_nme) == 0 && rpct_loop > 0);

  /* Compute the hash index for 'orig_nme' and 'rpct_nme' (they both
   * have the same hash index).
   */
  hashval = (int)((NME_TYPE(orig_nme) ^ NME_SYM(orig_nme) ^ NME_NM(orig_nme) ^
                   NME_SUB(orig_nme)) &
                  0x7fff) %
            NMEHSZ;

#if DEBUG
  /* Search the nme hash links for this NME.  If it already exists
   * we return it and generate an 'asrt()' error message, since this
   * shouldn't happen.
   */
  for (rpct_nme = nmehsh[hashval]; rpct_nme; rpct_nme = NME_HSHLNK(rpct_nme)) {
    if (NME_TYPE(rpct_nme) == NME_TYPE(orig_nme) &&
        NME_INLARR(rpct_nme) == NME_INLARR(orig_nme) &&
        NME_SYM(rpct_nme) == NME_SYM(orig_nme) &&
        NME_NM(rpct_nme) == NME_NM(orig_nme) &&
        NME_CNST(rpct_nme) == NME_CNST(orig_nme) &&
        NME_SUB(rpct_nme) == NME_SUB(orig_nme) &&
#ifdef NME_PTE
        NME_PTE(rpct_nme) == NME_PTE(orig_nme) &&
#endif
        NME_RPCT_LOOP(rpct_nme) == rpct_loop) {
      /* Found.
       */
      asrt(FALSE); /* we don't expect it to be found! */
      return rpct_nme;
    }
  }
#endif

  /* Not found, so create and initialise a new nme, and link it info
   * its hash chain.  If necessary get more storage.
   */
  rpct_nme = STG_NEXT(nmeb);

  if (rpct_nme > MAXNME)
    error(7, 4, 0, CNULL, CNULL);

  if (EXPDBG(10, 256))
    fprintf(gbl.dbgfil,
            "adding rpct nme %d, based on nme %d, in rpct loop %d\n", rpct_nme,
            orig_nme, rpct_loop);

  BCOPY(nmeb.stg_base + rpct_nme, nmeb.stg_base + orig_nme, NME, 1);

  NME_RPCT_LOOP(rpct_nme) = rpct_loop;
  NME_HSHLNK(rpct_nme) = nmehsh[hashval];
  nmehsh[hashval] = rpct_nme;

  return rpct_nme;

} /* end add_rpct_nme( int orig_nme, int rpct_loop ) */

/** \brief Add NME routine with no subscripts
 *
 *  enter nme into the NME area; use add_arrnme but add a subscript
 *  field of 0 and an inlarr field of FALSE.
 */
int
addnme(NT_KIND type, int insym, int nm, ISZ_T cnst)
{
  return (add_arrnme(type, insym, nm, cnst, (ISZ_T)0, FALSE));
}

int
_build_sym_nme(DTYPE dt, int curr_off, int offset, int nme)
{
  int i, j, mem, prev_mem;
  int elem_size;
  int sub;
  LOGICAL inlarr;

  return 0;
}

/** \brief Build an nme entry using a sym and an offset relative
           to base address of this symbol */
int
build_sym_nme(int sym, int offset, LOGICAL ptr_mem_op)
{
  int nme;
  int sub;
  LOGICAL inlarr;
  int dt;
  int i;

  if (!sym)
    return 0;

  if (ptr_mem_op)
    return 0;

  nme = addnme(NT_VAR, sym, 0, 0);
  dt = DTYPEG(sym);
  if (DTY(dt) == TY_PTR) {
    sub = 0;
    inlarr = FALSE;
    for (i = nme; i < nmeb.stg_avail; i++) {
      /* Check if subscript doesn't already exist */
      if (NME_TYPE(i) == NT_IND && NME_NM(i) == nme && NME_SYM(i) == 0 &&
          NME_CNST(i) == 0) {
        sub = NME_SUB(i);
          inlarr = NME_INLARR(i);
          break;
      }
    }
    nme = add_arrnme(NT_IND, 0, nme, 0, sub, inlarr);
    dt = DTY(dt + 1);
  }
  return _build_sym_nme(dt, 0, offset, nme);
}

/** \brief Get data type of a names entry
 */
DTYPE
dt_nme(int nm)
{
  int i;
  switch (NME_TYPE(nm)) {

  case NT_UNK:
    break;
  case NT_VAR:
    return (DTYPEG(NME_SYM(nm)));

  case NT_MEM:
    if (NME_SYM(nm) == 0 || NME_SYM(nm) == 1) {
      if (dt_nme((int)NME_NM(nm)) == DT_DCMPLX) {
        return (DT_DBLE);
      } else {
        return (DT_REAL);
      }
    }
    if ((i = NME_CNST(nm)) == 0)
      return (DTYPEG(NME_SYM(nm)));
    else
      return (DTYPEG(i));

  case NT_ARR:
    i = dt_nme((int)NME_NM(nm));
    if (DTY(i) == TY_ARRAY)
      return (DTY(i + 1));
    /*
     * for fortran, the TY_ARRAY dtype only occurs once; the element
     * type is returned after the array dtype is returned when we hit
     * the NT_VAR or NT_MEM case.  All we need to do is to just pass
     * up the dtype.  NOTE that this will work if try to fake arrays
     * using PLISTs.
     */
    return (i);

  case NT_IND:
    i = dt_nme((int)NME_NM(nm));
    if (DTY(i) == TY_PTR)
      return (DTY(i + 1));
    return (i);

  case NT_SAFE:
    return (dt_nme((int)NME_NM(nm)));
  }
  return (0);
}

/** \brief Location of a names entry
 *
 * LOC (explicit or implicit) has been performed.  The names entry for the
 * lvalue is scanned to determine if a symbol's ADDRTKN flag must be set
 */
void
loc_of(int nme)
{
  int type;

  while (TRUE) {
    if ((type = NME_TYPE(nme)) == NT_VAR) {
      if (!is_presym(nme))
        ADDRTKNP(NME_SYM(nme), 1);
      break;
    }
    if (type == NT_ARR || type == NT_MEM)
      nme = NME_NM(nme);
    else
      break;
  }
}

/** \brief Location of a name entry when a reference is volatile
 *
 * LOC (explicit or implicit) has been performed.  The names entry for the
 * lvalue is scanned to determine if a symbol's ADDRTKN flag must be set.  This
 * function is called when the reference is volatile; consequently, pointer
 * loads must be traversed.
 */
void
loc_of_vol(int nme)
{
  int type;

  while (TRUE) {
    if ((type = NME_TYPE(nme)) == NT_VAR) {
      ADDRTKNP(NME_SYM(nme), 1);
      break;
    }
    if (type == NT_ARR || type == NT_MEM || type == NT_IND)
      nme = NME_NM(nme);
    else
      break;
  }
}

/**
   \brief LOC (explicit or implicit) has been performed.  The names entry for
   the lvalue is scanned to determine if a symbol's PTRSTOREP flag must be set
*/
void
ptrstore_of(int nme)
{
  int type;

  while (TRUE) {
    if ((type = NME_TYPE(nme)) == NT_VAR) {
      PTRSTOREP(NME_SYM(nme), 1);
      break;
    }
    if (type == NT_ARR || type == NT_MEM)
      nme = NME_NM(nme);
    else
      break;
  }
}

/** Walk through the given nme and its chains of base nmes, query whether all
 * of them are static, i.e., a struct, array, var other than an object
 * dereferenced by a pointer.
 */
LOGICAL
basenme_is_static(int nme)
{
  while (TRUE) {
    switch (NME_TYPE(nme)) {
    case NT_IND:
      return FALSE;
    case NT_MEM:
    case NT_ARR:
    case NT_SAFE:
      nme = NME_NM(nme);
      break;
    default:
      return TRUE;
    }
  }
}

/** \brief Return the base st index of the names entry
 *
 * Returns the base symbol of a reference given its names entry -- returns 0 if
 * unknown.
 */
int
basesym_of(int nme)
{
  while (TRUE) {
    switch (NME_TYPE(nme)) {
    case NT_MEM:
    case NT_IND:
    case NT_ARR:
    case NT_SAFE:
      nme = NME_NM(nme);
      break;
    case NT_VAR:
      return ((int)NME_SYM(nme));
    default:
      goto not_found;
    }
  }
not_found:
  return 0;
}

/** \brief Return the base nme of a reference given its names entry;
           return 0 if unknown.
 */
int
basenme_of(int nme)
{
  while (TRUE) {
    switch (NME_TYPE(nme)) {
    case NT_MEM:
    case NT_IND:
    case NT_ARR:
    case NT_SAFE:
      nme = NME_NM(nme);
      break;
    default:
      goto found;
    }
  }
found:
  return nme;
}

/** \brief Return the base nme of a reference given its names entry,
           return 0 if unknown.

    WARNING - zbasenme_of() cannot traverse NT_IND

    SOMEDAY, replace with basenme_of() when flow's def/use can distinguish
    between a 'base' which is static ( a struct, array, var) or an object
    located by a pointer.
 */
int
zbasenme_of(int nme)
{
  while (TRUE) {
    switch (NME_TYPE(nme)) {
    case NT_MEM:
    case NT_ARR:
    case NT_SAFE:
      nme = NME_NM(nme);
      break;
    default:
      goto found;
    }
  }
found:
  return nme;
}

/** \brief Add pointer target entry
 */
int
addpte(int type, SPTR sptr, int val, int next)
{
  int hshval, p;
  hshval = (int)((type ^ sptr ^ val ^ next) & 0x7fff) % NMEHSZ;
  for (p = ptehsh[hshval]; p > 0; p = PTE_HSHLNK(p)) {
    if (PTE_TYPE(p) == type && PTE_SPTR(p) == sptr && PTE_VAL(p) == val &&
        PTE_NEXT(p) == next) {
      return p;
    }
  }
  p = STG_NEXT(nmeb.pte);
  PTE_TYPE(p) = type;
  PTE_SPTR(p) = sptr;
  PTE_VAL(p) = val;
  PTE_NEXT(p) = next;
  PTE_HSHLNK(p) = ptehsh[hshval];
  ptehsh[hshval] = p;
  return p;
} /* addpte */

/**
 * \brief Add a "runtime pointer conflict test" record
 *
 * This creates and returns a new RPCT (runtime pointer conflict test)
 * record containing the pair of NMEs (rpct_nme1, rpct_nme2).  These
 * NMEs are 'RPCT NMEs' that were created by 'add_rpct_nme()'.  The
 * existence of this RPCT record indicates that a pointer conflict
 * test has been generated which proves at runtime that this pair of
 * references do not conflict, so 'conflict( rpct_nme1, rpct_nme2 )'
 * will return NOCONFLICT.
 */
void
add_rpct(int rpct_nme1, int rpct_nme2)
{
  int hashval, rpct;

  asrt(rpct_nme1 != rpct_nme2 && NME_RPCT_LOOP(rpct_nme1) &&
       NME_RPCT_LOOP(rpct_nme2) == NME_RPCT_LOOP(rpct_nme1));

  /* If necessary swap 'rpct_nme1' & 'rpct_nme2' so that
   * (rpct_nme1 < rpct_nme2), since that makes it quicker to search
   * the RPCT table for a match.
   */
  if (rpct_nme1 > rpct_nme2) {
    int tmp;

    tmp = rpct_nme1;
    rpct_nme1 = rpct_nme2;
    rpct_nme2 = tmp;
  }

  /* Compute the hash index for this RPCT record.
   */
  hashval = (int)((rpct_nme1 ^ rpct_nme2) & 0x7fff) % NMEHSZ;

#if DEBUG
  /* Search the RPCT hash links for this RPCT.  If it already exists
   * we generate an 'asrt()' error message, since this shouldn't happen.
   */
  for (rpct = rpcthsh[hashval]; rpct; rpct = RPCT_HSHLNK(rpct)) {
    if (RPCT_NME1(rpct) == rpct_nme1 && RPCT_NME2(rpct) == rpct_nme2) {
      /* Found.
       */
      asrt(FALSE); /* we don't expect it to be found! */
      return;
    }
  }
#endif

  /* Create and initialise a new RPCT record and link it into its hash chain.
   */
  rpct = STG_NEXT(nmeb.rpct);
  RPCT_NME1(rpct) = rpct_nme1;
  RPCT_NME2(rpct) = rpct_nme2;
  RPCT_HSHLNK(rpct) = rpcthsh[hashval];
  rpcthsh[hashval] = rpct;

} /* end add_rpct( int rpct_nme1, int rpct_nme2 ) */

/**
 * This function returns TRUE if there is an RPCT (runtime pointer
 * conflict test) record containing the pair of NMEs (rpct_nme1,
 * rpct_nme2), and FALSE otherwise.  In the former case
 * 'conflict( rpct_nme1, rpct_nme2 )' will return NOCONFLICT, as
 * explained in the comment for function 'add_rpct()'.
 */
static LOGICAL
found_rpct(int rpct_nme1, int rpct_nme2)
{
  int hashval, rpct;

  asrt(rpct_nme1 != rpct_nme2 && NME_RPCT_LOOP(rpct_nme1) &&
       NME_RPCT_LOOP(rpct_nme2) == NME_RPCT_LOOP(rpct_nme1));

  /* If necessary swap 'rpct_nme1' & 'rpct_nme2' so that
   * (rpct_nme1 < rpct_nme2).
   */
  if (rpct_nme1 > rpct_nme2) {
    int tmp;

    tmp = rpct_nme1;
    rpct_nme1 = rpct_nme2;
    rpct_nme2 = tmp;
  }

  /* Compute the hash index for this RPCT record.
   */
  hashval = (int)((rpct_nme1 ^ rpct_nme2) & 0x7fff) % NMEHSZ;

  /* Search the RPCT hash links for this RPCT.
   */
  for (rpct = rpcthsh[hashval]; rpct; rpct = RPCT_HSHLNK(rpct)) {
    if (RPCT_NME1(rpct) == rpct_nme1 && RPCT_NME2(rpct) == rpct_nme2) {
      return TRUE; /* found */
    }
  }

  return FALSE; /* not found */

} /* end found_rpct( int rpct_nme1, int rpct_nme2 ) */

/** \brief Return the base symbol of a reference given its names entry;
           return 0 if unknown or compiler-created.
*/
int
usersym_of(int nme)
{
  int sym;

  sym = basesym_of(nme);
  if (sym == 0 || CCSYMG(sym))
    return 0;
  return sym;
}

static void
prsym(int sym, FILE *ff)
{
  char *p;
  p = getprint((int)sym);
  fprintf(ff, "%s", p);
  if (strncmp("..inline", p, 8) == 0)
    fprintf(ff, "%d", sym);
}

/* FIXME: this functionality is duplicated and extended in mwd.c.
   Merge these functions with their duplicates in mwd.c */
/**
 * Prints the symbol reference represented by a names entry and
 * returns the base symbol of a reference given its names entry --
 * this is for scalar and structure references only
 */
int
__print_nme(FILE *ff, int nme)
{
  int i;

  if (ff == NULL)
    ff = stderr;
  switch (NME_TYPE(nme)) {
  case NT_VAR:
    i = NME_SYM(nme);
    prsym(i, ff);
    break;
  case NT_MEM:
    i = print_nme((int)NME_NM(nme));
    if (NME_SYM(nme) == 0) {
      fprintf(ff, ".real");
      break;
    }
    if (NME_SYM(nme) == 1) {
      fprintf(ff, ".imag");
      break;
    }
    fprintf(ff, ".%s", getprint((int)NME_SYM(nme)));
    break;
  default:
    interr("print_nme:ill.sym", nme, 3);
    i = 0;
    break;
  }

  return (i);
}

/** \brief Print the symbol reference associated with a names entry.
 *
 * prints the symbol reference represented by a names entry and
 * returns the base symbol of a reference given its names entry --
 * this is for scalar and structure references only
 */
int
print_nme(int nme)
{
  int i;
  FILE *ff;

  ff = gbl.dbgfil;
  return __print_nme(ff, nme);
}

#if DEBUG

void
__dmpnme(FILE *f, int i, int flag)
{
  FILE *ff;
  int j;

  ff = f;
  if (f == NULL)
    ff = stderr;
  if (!flag)
    fprintf(ff, "%5u   ", i);
  else
    fprintf(ff, "%5u   rfptr %d sub %d hshlk %d f6 %d inlarr %d\n\t", i,
            NME_RFPTR(i), NME_SUB(i), NME_HSHLNK(i), NME_DEF(i), NME_INLARR(i));
  switch (NME_TYPE(i)) {
  case NT_VAR:
    j = NME_SYM(i);
    fprintf(ff, "variable            sym:%5u  \"", j);
    prsym(j, ff);
    fprintf(ff, "\"\n");
    break;
  case NT_ARR:
    if (NME_SYM(i) == NME_NULL)
      fprintf(ff, "variable array       nm:%5u", NME_NM(i));
    else
      fprintf(ff, "constant array       nm:%5u    cnst:%5" ISZ_PF "d",
              NME_NM(i), NME_CNST(i));
    fprintf(ff, "    sub: %5u %s", NME_SUB(i), NME_INLARR(i) ? "<inl>" : " ");
    if (NME_OVS(i)) {
      fprintf(ff, "<ovs>");
    }
    fprintf(ff, "\n");
    break;
  case NT_MEM:
    j = NME_SYM(i);
    if (j == 0) {
      fprintf(ff, "member              sym:%5u      nm:%5u  \"real\"\n", j,
              NME_NM(i));
      break;
    }
    if (j == 1) {
      fprintf(ff, "member              sym:%5u      nm:%5u  \"imag\"\n", j,
              NME_NM(i));
      break;
    }
    j = NME_SYM(i);
    fprintf(ff, "member              sym:%5u      nm:%5u  \"%s\"\n", j,
            NME_NM(i), getprint((int)j));
    break;
  case NT_IND:
    if (NME_SYM(i) == NME_NULL)
      fprintf(ff, "variable indirection nm:%5u\n", NME_NM(i));
    else
      fprintf(ff, "constant indirection nm:%5u    cnst:%5" ISZ_PF "d\n",
              NME_NM(i), NME_CNST(i));
    break;
  case NT_SAFE:
    fprintf(ff, "safe nme             nm:%5u\n", NME_NM(i));
    break;
  case NT_UNK:
    if (NME_SYM(i))
      fprintf(ff, "unknown/volatile\n");
    else
      fprintf(ff, "unknown\n");
    break;
  default:
    interr("__dmpnme: illegal nme", NME_TYPE(i), 3);
    fprintf(ff, "\n");
  }
}

/** \brief Dump names table (all fields)
 */
void
dmpnmeall(int flag)
{
  int i, j;
  int tmp;

  fprintf(gbl.dbgfil, "\n\n***** NME Area Dump *****\n\n");
  for (i = 0; i < nmeb.stg_avail; i++) {
    __dmpnme(gbl.dbgfil, i, flag);
  }

  if ((flg.dbg[10] & 8) != 0) {
    fprintf(gbl.dbgfil, "\n\n***** NME Hash Table *****\n");
    for (i = 0; i < NMEHSZ; i++)
      if ((j = nmehsh[i]) != 0) {
        tmp = 0;
        fprintf(gbl.dbgfil, "%3d.", i);
        for (; j != 0; j = NME_HSHLNK(j)) {
          fprintf(gbl.dbgfil, " %5u^", j);
          if ((++tmp) == 6) {
            tmp = 0;
            fprintf(gbl.dbgfil, "\n    ");
          }
        }
        if (tmp != 0)
          fprintf(gbl.dbgfil, "    \n");
      }
  }
}

/** \brief Dump names table
 */
void
dmpnme(void)
{
  dmpnmeall(0);
}
#endif

void
__dumpname(FILE *f, int opn)
{
  static int level = 0;
  FILE *ff;

#if DEBUG
  ff = f;
  if (f == NULL)
    ff = stderr;

  if (opn < 0 || opn >= nmeb.stg_size) {
    interr("__dumpname:bad names ptr", opn, 3);
    fprintf(ff, " %5u <BAD>", opn);
    return;
  }

  if (level == 0)
    fprintf(ff, " %5u~ <", opn);

  level++;

  switch (NME_TYPE(opn)) {
  case NT_VAR:
    prsym(NME_SYM(opn), ff);
    break;
  case NT_MEM:
    __dumpname(ff, (int)NME_NM(opn));
    if (NME_SYM(opn) == 0) {
      fprintf(ff, "->real");
      break;
    }
    if (NME_SYM(opn) == 1) {
      fprintf(ff, "->imag");
      break;
    }
    fprintf(ff, "->%s", getprint((int)NME_SYM(opn)));
    break;
  case NT_IND:
    fprintf(ff, "*(");
    __dumpname(ff, (int)NME_NM(opn));
    if (NME_SYM(opn) == 0)
      if (NME_CNST(opn))
        fprintf(ff, "%+" ISZ_PF "d)", NME_CNST(opn));
      else
        fprintf(ff, ")");
    else
      fprintf(ff, "+i)");
    break;
  case NT_ARR:
    __dumpname(ff, (int)NME_NM(opn));
    fprintf(ff, "[");
    if (NME_SYM(opn) == 0)
      fprintf(ff, "%" ISZ_PF "d]", NME_CNST(opn));
    else
      fprintf(ff, "i]");
    break;
  case NT_SAFE:
    fprintf(ff, "safe(");
    __dumpname(ff, (int)NME_NM(opn));
    fprintf(ff, ")");
    break;
  case NT_UNK:
    if (NME_SYM(opn))
      fprintf(ff, "?vol");
    else
      fprintf(ff, "?");
    break;
  }

  --level;

  if (level == 0)
    fprintf(ff, ">");
#endif
}

/** Pretty print a nme
 */
void
dumpname(int opn)
{
  __dumpname(gbl.dbgfil, opn);
}

#if DEBUG
void
__dumpnme(FILE *f, int opn)
{
  FILE *ff;

  ff = f;
  if (f == NULL)
    ff = stderr;

  if (opn < 0 || opn >= nmeb.stg_size) {
    interr("__dumpnme:bad names ptr", opn, 3);
    fprintf(ff, " %5u <BAD>", opn);
    return;
  }

  __dmpnme(ff, opn, 0);
  switch (NME_TYPE(opn)) {
  case NT_VAR:
    break;
  case NT_MEM:
    __dumpnme(ff, (int)NME_NM(opn));
    break;
  case NT_IND:
    __dumpnme(ff, (int)NME_NM(opn));
    break;
  case NT_ARR:
    __dumpnme(ff, (int)NME_NM(opn));
    break;
  case NT_SAFE:
    __dumpnme(ff, (int)NME_NM(opn));
    break;
  case NT_UNK:
    break;
  }
}

#define TOP 10
void
PrintTopNMEHash(void)
{
  int h, s, t, nmex;
  int topten[TOP], toptensize[TOP];
  if (nmeb.stg_base == NULL)
    return;
  for (s = 0; s < TOP; ++s) {
    topten[s] = 0;
    toptensize[s] = 0;
  }
  for (h = 0; h < NMEHSZ; ++h) {
    s = 0;
    for (nmex = nmehsh[h]; nmex > 0; nmex = NME_HSHLNK(nmex))
      ++s;
    if (s) {
      for (t = 0; t < TOP; ++t) {
        if (s == toptensize[t]) {
          ++topten[t];
          break;
        } else if (s > toptensize[t]) {
          /* move the others */
          int tt;
          for (tt = TOP - 1; tt > t; --tt) {
            toptensize[tt] = toptensize[tt - 1];
            topten[tt] = topten[tt - 1];
          }
          toptensize[t] = s;
          topten[t] = 1;
          break;
        }
      }
    }
  }
  fprintf(gbl.dbgfil, "Function %d = %s\nTop %d NME Hash Table Entries\n %d "
                      "NME entries, Hash Size %d, Average Length %d\n",
          gbl.func_count, GBL_CURRFUNC ? SYMNAME(GBL_CURRFUNC) : "", TOP,
          nmeb.stg_avail - 1, NMEHSZ, (nmeb.stg_avail - 1 + NMEHSZ) / NMEHSZ);
  for (s = 0; s < TOP; ++s) {
    fprintf(gbl.dbgfil, " [%2d] %d * %d\n", s + 1, toptensize[s], topten[s]);
  }
} /* PrintTopNMEHash */

void
PrintTopHash(void)
{
  int h, s, t, sptr;
  int topten[TOP], toptensize[TOP];
  for (s = 0; s < TOP; ++s) {
    topten[s] = 0;
    toptensize[s] = 0;
  }
  for (h = 0; h < HASHSIZE + 1; ++h) {
    s = 0;
    for (sptr = stb.hashtb[h]; sptr > NOSYM; sptr = HASHLKG(sptr))
      ++s;
    if (s) {
      for (t = 0; t < TOP; ++t) {
        if (s == toptensize[t]) {
          ++topten[t];
          break;
        } else if (s > toptensize[t]) {
          /* move the others */
          int tt;
          for (tt = TOP - 1; tt > t; --tt) {
            toptensize[tt] = toptensize[tt - 1];
            topten[tt] = topten[tt - 1];
          }
          toptensize[t] = s;
          topten[t] = 1;
          break;
        }
      }
    }
  }
  fprintf(gbl.dbgfil, "Function %d = %s\nTop %d Symbol Hash Table Entries\n %d "
                      "symbols, Hash Size %d, Average length %d:\n",
          gbl.func_count, GBL_CURRFUNC ? SYMNAME(GBL_CURRFUNC) : "", TOP,
          stb.symavl - 1, HASHSIZE + 1, (stb.symavl - 1 + HASHSIZE) / HASHSIZE);
  for (s = 0; s < TOP; ++s) {
    fprintf(gbl.dbgfil, " [%2d] %d * %d\n", s + 1, toptensize[s], topten[s]);
  }
  PrintTopNMEHash();
} /* PrintTopHash */
#endif
