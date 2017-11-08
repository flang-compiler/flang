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
    \brief Compiler miscellaneous utility programs.
 */

#include "gbldefs.h"
#include "global.h"
#include "error.h"

#include <stdbool.h>
#include "flang/ArgParser/xflag.h"

/**
   \brief Allocate space for and make new filename using mkperm.
 */
char *
mkfname(char *oldname, char *oldsuf, char *newsuf)
{
  char *p;

  /*  get enough space for oldname, newsuf, '.', '\0', and 1 extra: */
  p = getitem(8, strlen(oldname) + strlen(newsuf) + 3);
  strcpy(p, oldname);
  return (mkperm(p, oldsuf, newsuf));
}

LOGICAL
is_xflag_bit(int indx)
{
  return is_xflag_bitvector(indx);
}

/** \brief Called only from main() */
void
set_xflag(int indx, INT val)
{
  set_xflag_value(flg.x, indx, val);
  /* XXX Unexpected side effect: "set x flag" should not be upping opt level */
  if (indx == 9 && flg.opt < 2) /* max cnt for unroller */
    flg.opt = 2;
}

/** \brief Called only from main() */
void
set_yflag(int indx, INT val)
{
  unset_xflag_value(flg.x, indx, val);
}

void
fprintf_str_esc_backslash(FILE *f, char *str)
{
  int ch;
  fputc('"', f);
  while ((ch = *str++)) {
    fputc(ch, f);
    if (ch == '\\')
      fputc('\\', f);
  }
  fputc('"', f);
}

/*
 * error message
 */
static void
invalid_size(char* funcname, int dtsize, int size, char* stgname)
{
  interrf(ERR_Fatal,
          "%s: STG %s has invalid datatype size (%d) or structure size(%d)",
          funcname, stgname, dtsize, size);
} /* invalid_size */

/*
 * memory management
 *  allocate STG data structure, set the appropriate fields
 *  element zero is reserved, so stg_avail is initialized to 1
 */
static void
stg_alloc_base(STG *stg, int dtsize, int size, char *name)
{
  if (dtsize > 0 && size > 0) {
    memset(stg, 0, sizeof(STG));
    stg->stg_size = size;
    stg->stg_dtsize = dtsize;
    stg->stg_avail = 1;
    stg->stg_cleared = 0;
    stg->stg_name = name;
    stg->stg_base = (void *)sccalloc(stg->stg_dtsize * stg->stg_size);
  } else {
    invalid_size("stg_alloc", dtsize, size, name);
  }
} /* stg_alloc_base */

/*
 * clear 'n' elements of the data structure starting at 'r'
 * reset stg_cleared if we're initializing or extending the cleared region
 */
void
stg_clear_force(STG *stg, int r, int n, LOGICAL force)
{
  if (r >= 0 && n > 0) {
    STG *thisstg;
    if (r == stg->stg_cleared) {
      stg->stg_cleared += n;
    } else if (r == 0 && n > stg->stg_cleared) {
      stg->stg_cleared = n;
    }
    for (thisstg = stg; thisstg; thisstg = (STG *)thisstg->stg_sidecar) {
      thisstg->stg_cleared = stg->stg_cleared;
      if (force || !STG_CHECKFLAG((*thisstg), STG_FLAG_NOCLEAR))
        memset((char *)(thisstg->stg_base) + (r * thisstg->stg_dtsize), 0,
         n * thisstg->stg_dtsize);
    }
  }
} /* stg_clear_force */

void
stg_clear(STG *stg, int r, int n)
{
  if (r >= 0 && n > 0) {
    stg_clear_force(stg, r, n, FALSE);
  }
} /* stg_clear */

/*
 * clear the data structure up to stg_avail
 */
void
stg_clear_all(STG *stg)
{
  stg_clear(stg, 0, stg->stg_avail);
} /* stg_clear_all */

/*
 * allocate STG data structure, clear element zero
 */
void
stg_alloc(STG *stg, int dtsize, int size, char *name)
{
  stg_alloc_base(stg, dtsize, size, name);
  stg_clear_force(stg, 0, 1, TRUE);
} /* stg_alloc */

/*
 * deallocate STG data structure
 */
void
stg_delete(STG *stg)
{
  if (stg->stg_base)
    sccfree((char *)stg->stg_base);
  memset(stg, 0, sizeof(STG));
} /* stg_delete */

/*
 * reallocate STG structure if we need the extra size (if stg_avail > stg_size)
 *  reallocate any sidecars as well
 *  the new size will be 2*(stg_avail-1), which must be >= 2*stg_size
 */
void
stg_need(STG *stg)
{
  STG *thisstg;
  /* if the compiler has recycled some previously allocated space,
   * we need to reset the stg_cleared region */
  if (stg->stg_cleared > stg->stg_avail)
    stg->stg_cleared = stg->stg_avail;
  if (stg->stg_avail > stg->stg_size) {
    int newsize, oldsize;
    oldsize = stg->stg_size;
    newsize = (stg->stg_avail - 1) * 2;
    /* reallocate stg and all its sidecars */
    for (thisstg = stg; thisstg; thisstg = (STG *)thisstg->stg_sidecar) {
      thisstg->stg_size = newsize;
      thisstg->stg_base = (void *)sccrelal(
          (char *)thisstg->stg_base, newsize * thisstg->stg_dtsize);
    }
    /* we have to clear all newly allocated elements, in case there
     * are sidecars with the NOCLEAR flag set, so they get initially cleared */
    stg_clear_force(stg, oldsize, newsize - oldsize, TRUE);
  }
  if (stg->stg_avail > stg->stg_cleared) {
    /* clear any new elements */
    stg_clear_force(stg, stg->stg_cleared, stg->stg_avail - stg->stg_cleared, TRUE);
  }
} /* stg_need */

/*
 * Allocate a sidecar, attach to list of sidecars
 */
void
stg_alloc_sidecar(STG *basestg, STG *stg, int dtsize, char *name)
{
  if (stg->stg_sidecar) {
    interrf(ERR_Fatal, "%s: %s has a sidecar, may not add as sidecar to %s",
      "stg_alloc_sidecar", stg->stg_name, basestg->stg_name);
  }
  stg_alloc_base(stg, dtsize, basestg->stg_size, name);
  stg->stg_avail = basestg->stg_avail;
  /* clear sidecar for any already-allocated elements */
  stg_clear_force(stg, 0, stg->stg_size, TRUE);
  /* link this sidecar to the list of sidecars for the basestg */
  stg->stg_sidecar = basestg->stg_sidecar;
  basestg->stg_sidecar = (void *)stg;
} /* stg_alloc_sidecar */

/*
 * error message
 */
static void
sidecar_not_found(char *funcname, STG *basestg, STG *stg)
{
  /* sidecar not found, this is an error */
  interrf(ERR_Fatal, "%s: Sidecar %s to %s not found", funcname,
          basestg->stg_name, stg->stg_name);
} /* sidecar_not_found */

/*
 * Deallocate a sidecar, detach from list of sidecars
 */
void
stg_delete_sidecar(STG *basestg, STG *stg)
{
  if (basestg->stg_sidecar == (void *)stg) {
    basestg->stg_sidecar = stg->stg_sidecar;
  } else {
    STG *sidecar;
    for (sidecar = (STG *)basestg->stg_sidecar; sidecar;
         sidecar = sidecar->stg_sidecar) {
      if (sidecar->stg_sidecar == (void *)stg) {
        sidecar->stg_sidecar = stg->stg_sidecar;
        break;
      }
    }
    if (!sidecar) {
      sidecar_not_found("stg_delete_sidecar", basestg, stg);
    }
  }
  stg_delete(stg);
} /* stg_delete_sidecar */

/*
 * reserve next n elements at stg_avail; increment stg_avail;
 * grow, if necessary;
 * clear newly allocated elements; return the first such element.
 */
int
stg_next(STG *stg, int n)
{
  STG *thisstg;
  int r = stg->stg_avail;
  if (n == 0)
    return 0;
  if (n < 0) {
    interrf(ERR_Fatal, "stg_next(%s,%d) called with n < 0", stg->stg_name, n);
    return 0;
  }
  /* if the compiler has recycled some previously allocated space,
   * we need to reset the stg_cleared region */
  if (stg->stg_cleared > r)
    stg->stg_cleared = r;
  stg->stg_avail += n;
  for (thisstg = stg->stg_sidecar; thisstg; thisstg = (STG *)thisstg->stg_sidecar) {
    thisstg->stg_avail = stg->stg_avail;
    thisstg->stg_cleared = stg->stg_cleared;
  }
  if (stg->stg_avail > stg->stg_size) {
    stg_need(stg);
  } else {
    stg_clear(stg, stg->stg_cleared, stg->stg_avail - stg->stg_cleared);
  }
  return r;
} /* stg_next */

/*
 * error message
 */
static void
too_small_for_freelist(char *funcname, STG *stg)
{
  interrf(ERR_Fatal, "%s: structure %s too small for a freelist link, size=%d",
    funcname, stg->stg_name, stg->stg_dtsize);
} /* too_small_for_freelist */

static char*
freefield(STG* stg, int r)
{
  char *base;
  /* get stg_base */
  base = (char *)stg->stg_base;
  /* add the offset of the r'th element (r*dtsize) */
  base += r * stg->stg_dtsize;
  /* add freelink offset */
  base += stg->stg_freelink_offset;
  return base;
} /* freefield */

/*
 * get next element from the free list, if it's not null.
 * reset the free list from the free list link.
 * otherwise, just get the next available element from stg_avail
 * the link to the next free element is stored at 'word 0' of the structure
 */
int
stg_next_freelist(STG *stg)
{
  int r = stg->stg_free;
  if (!r) {
    r = stg_next(stg, 1);
    stg_clear(stg, r, 1);
  } else {
    char *base;
    if (stg->stg_dtsize < sizeof(int))
      too_small_for_freelist("stg_next_freelist", stg);
    /* get freelink for entry 'r' */
    base = freefield(stg, r);
    /* move stg_free to the next free element */
    stg->stg_free = *(int *)base;
    /* clear the new element */
    stg_clear(stg, r, 1);
  }
  return r;
} /* stg_next_freelist */

/*
 * add element to the free list
 * store the link to the next free element at 'word 0'
 */
void
stg_add_freelist(STG *stg, int r)
{
  char *base;
  if (stg->stg_dtsize < sizeof(int))
    too_small_for_freelist("stg_next_freelist", stg);
  /* clear the recycled element */
  stg_clear(stg, r, 1);
  /* get stg_base */
  base = freefield(stg, r);
  /* link to the free list */
  *(int *)base = stg->stg_free;
  stg->stg_free = r;
} /* stg_add_freelist */

/*
 * set the free list link field offset
 */
void
stg_set_freelink(STG* stg, int offset)
{
  stg->stg_freelink_offset = offset;
} /* stg_set_freelink */

