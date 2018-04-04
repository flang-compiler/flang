/*
 * Copyright (c) 1993-2018, NVIDIA CORPORATION.  All rights reserved.
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
 * \brief data type utility functions.
 */

#include "gbldefs.h"
#include "error.h"
#include "global.h"
#include "symtab.h"
#include "machar.h"
#include "machardf.h"

static int size_sym = 0;
/* The no_data_components() function and its supporting predicate functions 
 * are mirrored from the front end */
struct visit_list {
  DTYPE dtype;
  LOGICAL is_active;
  struct visit_list *next;
};

static struct visit_list *
visit_list_scan(struct visit_list *list, DTYPE dtype)
{
  for (; list; list = list->next) {
    if (list->dtype == dtype)
      break;
  }
  return list;
}

static void
visit_list_push(struct visit_list **list, DTYPE dtype)
{
  struct visit_list *newlist;
  NEW(newlist, struct visit_list, 1);
  newlist->dtype = dtype;
  newlist->is_active = TRUE;
  newlist->next = *list;
  *list = newlist;
}

static void
visit_list_free(struct visit_list **list)
{
  struct visit_list *p;
  while ((p = *list)) {
    *list = p->next;
    FREE(p);
  }
}

static LOGICAL is_recursive(int sptr, struct visit_list **visited);
typedef LOGICAL (*stm_predicate_t)(int member_sptr,
                                   struct visit_list **visited);
static TY_KIND
get_ty_kind(DTYPE dtype)
{
  assert(dtype > 0 && dtype < stb.dt_avail, "bad dtype", dtype, ERR_Severe);
  return DTY(dtype);
}

LOGICAL
is_array_dtype(DTYPE dtype)
{
  return dtype > DT_NONE && get_ty_kind(dtype) == TY_ARRAY;
}

DTYPE
array_element_dtype(DTYPE dtype)
{
  return is_array_dtype(dtype) ? DTY((int)dtype + 1) : DT_NONE;
}

static LOGICAL
is_container_dtype(DTYPE dtype)
{
  if (dtype > 0) {
    if (is_array_dtype(dtype))
      dtype = array_element_dtype(dtype);
    switch (DTYG(dtype)) {
      case TY_STRUCT:
      case TY_UNION:
        return TRUE;
    }
  }
  return FALSE;
}

static LOGICAL
search_type_members(DTYPE dtype, stm_predicate_t predicate,
                    struct visit_list **visited)
{
  LOGICAL result = FALSE;

  if (is_array_dtype(dtype))
    dtype = array_element_dtype(dtype);
  if (is_container_dtype(dtype)) {
    int member_sptr = DTY(dtype + 1);
    struct visit_list *active = visit_list_scan(*visited, dtype);

    if (active) {
      return predicate == is_recursive && active->is_active;
    }

    visit_list_push(visited, dtype);
    active = *visited;

    /* Traverse the members of the derived type. */
    while (member_sptr > NOSYM && !(result = predicate(member_sptr, visited))) {
      member_sptr = SYMLKG(member_sptr);
    }

    /* The scan of this data type is complete. Leave it on the visited
     * list to forestall another failed pass later.
     */
    active->is_active = FALSE;
  }
  return result;
}


/** Check for special case of empty typedef which has a size of 0
 * but one member of type DT_NONE to indicate that the type is
 * empty and not incomplete, a forward reference, etc.
 */
LOGICAL
is_empty_typedef(DTYPE dtype)
{
  int mem;
  if (DTY(dtype) != TY_UNION && DTY(dtype) != TY_STRUCT) {
    return 0;
  }
  mem = DTY(dtype + 1);
  if (mem <= NOSYM)
    return 1;
  return 0;
}

static LOGICAL
is_recursive(int sptr, struct visit_list **visited)
{
  return sptr > NOSYM &&
         search_type_members(DTYPEG(sptr), is_recursive, visited);
}

static LOGICAL
is_recursive_dtype(int sptr, struct visit_list **visited)
{
  return sptr > NOSYM &&
         search_type_members(sptr, is_recursive_dtype, visited);
}

/** For the derived type in dtype: Returns true if dtype is empty or
 * if it does not contain any data components (i.e., a derived type with
 * type bound procedures returns false). Otherwise, returns false.
 */
static LOGICAL
no_data_components_recursive(DTYPE dtype, stm_predicate_t predicate, struct visit_list **visited)
{
  int mem;
  struct visit_list *active = visit_list_scan(*visited, dtype);

  if (DTY(dtype) == TY_ARRAY)
    dtype = DTY(dtype + 1);

  if (is_empty_typedef(dtype)) {
    return 1;
  }
  if (DTY(dtype) != TY_UNION && DTY(dtype) != TY_STRUCT) {
    return 0;
  }

  if (active) {
    return predicate == is_recursive_dtype && active->is_active;
  }

  visit_list_push(visited, dtype);
  active = *visited;

  for (mem = DTY(dtype + 1); mem > NOSYM; mem = SYMLKG(mem)) {
    /* if member has derived type datatype, then need to recursively check
       it's possible that it is empty type, such as abstract type.
     */
    if (DTY(DTYPEG(mem)) == TY_STRUCT) {
      if (!no_data_components_recursive(DTYPEG(mem), is_recursive_dtype, visited)) {
        active->is_active = FALSE;
        return 0;
      }
    } else if (!CLASSG(mem) || !TBPLNKG(mem)) {
      active->is_active = FALSE;
      return 0;
    }
  }
  return 1;
}

LOGICAL
no_data_components(DTYPE dtype)
{
  struct visit_list *visited = NULL;
  LOGICAL result = no_data_components_recursive(dtype, is_recursive_dtype, &visited);
  visit_list_free(&visited);
  return result;
}

static int nosize_ok = 0;

static ISZ_T
_size_of(DTYPE dtype)
{
  INT d;
  ADSC *ad;
  ISZ_T val, nelems, sz;

  assert((int)dtype > 0 && dtype < stb.dt_avail, "size_of:bad dtype", dtype, 3);

  switch (DTY(dtype)) {
  case TY_WORD:
  case TY_DWORD:
  case TY_LOG:
  case TY_INT:
  case TY_UINT:
  case TY_FLOAT:
  case TY_PTR:
  case TY_SLOG:
  case TY_SINT:
  case TY_USINT:
  case TY_BINT:
  case TY_UBINT:
  case TY_BLOG:
  case TY_DBLE:
  case TY_QUAD:
  case TY_CMPLX:
  case TY_DCMPLX:
  case TY_INT8:
  case TY_UINT8:
  case TY_LOG8:
  case TY_128:
  case TY_256:
  case TY_512:
  case TY_INT128:
  case TY_UINT128:
  case TY_LOG128:
  case TY_FLOAT128:
  case TY_CMPLX128:
    return dtypeinfo[DTY(dtype)].size;

  case TY_HOLL:
  case TY_CHAR:
    if (dtype == DT_ASSCHAR) {
      if (nosize_ok)
        return -1;
      interr("size_of: attempt to size assumed size character", 0, 3);
    }
    if (dtype == DT_DEFERCHAR) {
      if (nosize_ok)
        return -1;
      interr("size_of: attempt to size deferred size character", 0, 3);
    }
    return DTY(dtype + 1);

  case TY_NCHAR:
    if (dtype == DT_ASSCHAR) {
      if (nosize_ok)
        return -1;
      interr("size_of: attempt to size assumed size character", 0, 3);
    }
    if (dtype == DT_DEFERCHAR) {
      if (nosize_ok)
        return -1;
      interr("size_of: attempt to size deferred size character", 0, 3);
    }
    return 2 * DTY(dtype + 1);

  case TY_ARRAY:
    if ((d = DTY(dtype + 2)) <= 0) {
      if (nosize_ok)
        return -1;
      interr("size_of: no ad", (int)d, 3);
      return size_of((int)DTY(dtype + 1));
    }
    ad = AD_DPTR(dtype);
    d = AD_NUMELM(ad);
    if (d == 0 || STYPEG(d) != ST_CONST) {
/* illegal use of adjustable or assumed-size array:
   should have been caught in semant.  */
/* errsev(50); */
      if (XBIT(68, 0x1))
        d = AD_NUMELM(ad) = stb.k1;
      else
        d = AD_NUMELM(ad) = stb.i1;
    }
    nelems = ad_val_of(d);
    sz = size_of((int)DTY(dtype + 1));
    val = nelems * sz;
    if (size_sym && (val < nelems || val < sz) && nelems && sz) {
      return -1;
    }
    return val;

  case TY_STRUCT:
  case TY_UNION:
    if (DTY(dtype + 2) < 0)
    {
      if (nosize_ok)
        return -1;
      errsev(151);
      return 4;
    } else
      return DTY(dtype + 2);
  case TY_VECT:
    d = DTY(dtype + 2);
    if (d == 3)
      d = 4;
    return d * size_of((int)DTY(dtype + 1));

  default:
    interr("size_of: bad dtype ", (int)DTY(dtype), 3);
    return 1;
  }
}

ISZ_T
size_of(DTYPE dtype)
{
  return _size_of(dtype);
}

ISZ_T
zsize_of(DTYPE dtype)
{
  ISZ_T d;
  nosize_ok = 1;
  d = _size_of(dtype);
  nosize_ok = 0;
  return d;
}

ISZ_T
size_of_sym(SPTR sym)
{
  ISZ_T sz;

  size_sym = sym;
  sz = size_of(DTYPEG(sym));
  size_sym = 0;
  if (sz < 0) {
    error(219, 3, gbl.lineno, SYMNAME(sym), NULL);
    sz = 1;
  }
  return sz;
}

/** \brief Return the length, in stb.dt_base words, of each type of datatype
 * entry
 */
int
dlen(int dty)
{
  switch (dty) {
  case TY_ANY:
  case TY_BINT:
  case TY_UBINT:
  case TY_BLOG:
  case TY_CMPLX:
  case TY_DBLE:
  case TY_DCMPLX:
  case TY_DWORD:
  case TY_HOLL:
  case TY_INT:
  case TY_INT8:
  case TY_LOG:
  case TY_LOG8:
  case TY_NONE:
  case TY_NUMERIC:
  case TY_QUAD:
  case TY_REAL:
  case TY_SINT:
  case TY_SLOG:
  case TY_UINT:
  case TY_UINT8:
  case TY_USINT:
  case TY_WORD:
  case TY_128:
  case TY_256:
  case TY_512:
  case TY_INT128:
  case TY_UINT128:
  case TY_LOG128:
  case TY_FLOAT128:
  case TY_CMPLX128:
    return 1;
  case TY_CHAR:
  case TY_NCHAR:
  case TY_PTR:
    return 2;
  case TY_ARRAY:
  case TY_PFUNC:
    return 3;
  case TY_VECT:
    return 3;
  case TY_STRUCT:
  case TY_UNION:
    return 5;
  case TY_PARAM:
    return 4;
  case TY_PROC:
    return 6;
  default:
    return 1;
  }
} /* dlen */

static LOGICAL constrained = TRUE; /* assume aligning within an aggregate */

int
alignment(DTYPE dtype)
{
  TY_KIND ty;

  switch (ty = DTY(dtype)) {
  case TY_DWORD:
  case TY_DBLE:
  case TY_DCMPLX:
    if (constrained && !flg.dalign)
      return dtypeinfo[TY_INT].align;
    return dtypeinfo[ty].align;
  case TY_QUAD:
  case TY_WORD:
  case TY_HOLL:
  case TY_BINT:
  case TY_UBINT:
  case TY_SINT:
  case TY_USINT:
  case TY_INT:
  case TY_UINT:
  case TY_REAL:
  case TY_CMPLX:
  case TY_BLOG:
  case TY_SLOG:
  case TY_LOG:
  case TY_CHAR:
  case TY_NCHAR:
  case TY_PTR:
  case TY_128:
  case TY_256:
  case TY_512:
  case TY_INT128:
  case TY_UINT128:
  case TY_LOG128:
  case TY_FLOAT128:
  case TY_CMPLX128:
    return dtypeinfo[ty].align;
  case TY_INT8:
  case TY_UINT8:
  case TY_LOG8:
    if (constrained && (!flg.dalign || XBIT(119, 0x100000)))
      return dtypeinfo[TY_INT].align;
    return dtypeinfo[ty].align;

  case TY_ARRAY:
    return alignment((int)DTY(dtype + 1));
  case TY_VECT:
    return alignment(DTY(dtype + 1));

  case TY_STRUCT:
  case TY_UNION:
    return DTY(dtype + 4);

  default:
    interr("alignment: bad dtype ", ty, 3);
    return 0;
  }
}

/** Align on the most strict boundary for a data type -- used whenever we want
 * to align unconstrained (top-level) objects such as simple local, static,
 * and external variables. The alignment of struct of union variables
 * is just the most strict alignment determined by alignment() of its
 * members.  The alignment of arrays is just the aligmnent of required for
 * its element type.
 */
int
align_unconstrained(DTYPE dtype)
{
  int a;

  constrained = FALSE;
  a = alignment(dtype);
  constrained = TRUE;
  return a;
}

int
alignment_sym(SPTR sym)
{
  if (QALNG(sym))
    return dtypeinfo[TY_DBLE].align;
  return alignment(DTYPEG(sym));
}

/** \brief Support the alignof operator
 */
int
align_of(int dtype)
{
  return alignment(dtype) + 1;
}

#define CHARTABSIZE 40
static int chartab[CHARTABSIZE];

/** Data structure to hold TY_CHAR entries: linked list off of
 * array chartab; entries that are equal module CHARTABSIZE are
 * linked.  Relative pointers (integers) are used.
 */
struct chartab {
  int next;
  int dtype;
};

static int chartabavail, chartabsize;
static struct chartab *chartabbase = 0;

void
init_chartab(void)
{
  int i, ctb;

  for (i = 0; i < CHARTABSIZE; ++i)
    chartab[i] = 0;
  if (chartabbase == 0) {
    /* allocate new */
    chartabsize = CHARTABSIZE;
    NEW(chartabbase, struct chartab, chartabsize);
  }
  ctb = 1;
  chartabbase[0].next = 0;
  chartabbase[0].dtype = 0;
  /* Enter character*1 predefined data type */
  chartab[1] = ctb;
  chartabbase[ctb].next = 0;
  chartabbase[ctb].dtype = DT_CHAR;
  ++ctb;
  /* Enter ncharacter*1 predefined data type */
  chartabbase[ctb - 1].next = ctb;
  chartabbase[ctb].next = 0;
  chartabbase[ctb].dtype = DT_NCHAR;
  ++ctb;
  chartabavail = ctb;
}

void
Save_Chartab(FILE *fil)
{
  int nw;
  nw = fwrite((void *)&chartab, sizeof(int), CHARTABSIZE, fil);
  if (nw != CHARTABSIZE) {
    error(155, 4, 0, "Error writing temp file:", "chartabhead");
    exit(1);
  }
  nw = fwrite((void *)&chartabavail, sizeof(int), 1, fil);
  if (nw != 1) {
    error(155, 4, 0, "Error writing temp file:", "chartabavl");
    exit(1);
  }
  nw = fwrite((void *)chartabbase, sizeof(struct chartab), chartabavail, fil);
  if (nw != chartabavail) {
    error(155, 4, 0, "Error writing temp file:", "chartabavl");
    exit(1);
  }
} /* Save_Chartab */

void
Restore_Chartab(FILE *fil)
{
  int nw;
  nw = fread((void *)&chartab, sizeof(int), CHARTABSIZE, fil);
  if (nw != CHARTABSIZE) {
    error(155, 4, 0, "Error reading temp file:", "chartabhead");
    exit(1);
  }
  nw = fread((void *)&chartabavail, sizeof(int), 1, fil);
  if (nw != 1) {
    error(155, 4, 0, "Error reading temp file:", "chartabavl");
    exit(1);
  }
  NEED(chartabavail, chartabbase, struct chartab, chartabsize,
       chartabavail + 1000);
  nw = fread((void *)chartabbase, sizeof(struct chartab), chartabavail, fil);
  if (nw != chartabavail) {
    error(155, 4, 0, "Error reading temp file:", "chartabavl");
    exit(1);
  }
} /* Restore_Chartab */

DTYPE
get_type(int n, TY_KIND v1, int v2)
{
  int dtype;
  int i, j;

  dtype = stb.dt_avail;

/* if we want TY_CHAR find one if it exists */
  if (v1 == TY_CHAR || v1 == TY_NCHAR)
  {
    if (v2 < 0)
      v2 = 0;
    i = v2 % CHARTABSIZE;
    if (chartab[i]) {
      /* check list for this length */
      for (j = chartab[i]; j != 0; j = chartabbase[j].next) {
        int k = chartabbase[j].dtype;
        if (DTY(k + 1) == v2 && /* same length */
            DTY(k) == v1 /*TY_CHAR vs TY_NCHAR*/) {
          dtype = chartabbase[j].dtype;
          goto found;
        }
      }
    }
    /* not found */
    NEED(chartabavail + n, chartabbase, struct chartab, chartabsize,
         chartabsize + CHARTABSIZE);
    chartabbase[chartabavail].dtype = dtype;
    chartabbase[chartabavail].next = chartab[i];
    chartab[i] = chartabavail++;
  }

  NEED(stb.dt_avail + n, stb.dt_base, ISZ_T, stb.dt_size, stb.dt_size + 1000);
  stb.dt_base[stb.dt_avail] = v1;
  stb.dt_base[stb.dt_avail + 1] = v2;
  stb.dt_avail += n;
found:
  return dtype;
}

#define _VP 3
#define _FP ((4 * sizeof(int) + sizeof(char *)) / sizeof(int))

/** \brief Create a dtype record for an array of rank numdim including its
 * array descriptor.
 *
 * The layout of an array descriptor is:
 * <pre>
 *    int    numdim;  --+
 *    int    scheck;    |
 *    int    zbase;	+-- 5 ints (fixed part)
 *    int    sdsc;	|
 *    ILM_T  *ilmp;   --+   watch out if  pointers are 8 bytes.
 *    struct {
 *        int mlpyr;  --+
 *        int lwbd;	+-- 3 ints (variable part)
 *        int upbd;   --+
 *    } b[numdim];
 *    int    numlem;  --+-- 1 int after variable part.
 * </pre>
 *
 * Any change in the size of the structure requires a change to one or both
 * of the macros _FP and _VP.  Also the size assertion in symtab.c needs
 * to be changed.
 */
static void
get_aux_arrdsc(DTYPE dtype, int numdim)
{
  ADSC *ad;
  int numints;

  struct getpointeralign { /* see if 8 byte pointers require 8 byte alignmnt*/
    char *ilmp;
    char c;
  };
#define ALIGNSIZE \
  ((sizeof(struct getpointeralign) - sizeof(char *) - 1) / sizeof(int))
  /* ALIGNSIZE is pad bytes / sizeof(int)     expect 0 or 1 */

  DTY(dtype + 2) = aux.arrdsc_avl;
  numints = (_FP + 1) + (_VP * numdim);
  numints = ALIGN(numints, ALIGNSIZE);
  aux.arrdsc_avl += numints;

  NEED(aux.arrdsc_avl, aux.arrdsc_base, int, aux.arrdsc_size,
       aux.arrdsc_avl + 240);
  ad = AD_DPTR(dtype);
  BZERO(ad, int, numints);
  AD_NUMDIM(ad) = numdim;
}

DTYPE
get_array_dtype(int numdim, DTYPE eltype)
{
  DTYPE dtype;

  dtype = get_type(3, TY_ARRAY, eltype);
  get_aux_arrdsc(dtype, numdim);

  return dtype;
}

DTYPE
get_vector_dtype(DTYPE dtype, int n)
{
  DTYPE vecdt;

  vecdt = aux.vtypes[DTY(dtype)][n - 1];
  if (vecdt == 0) {
    vecdt = get_type(3, TY_VECT, dtype);
    DTY(vecdt + 2) = n;
    aux.vtypes[DTY(dtype)][n - 1] = vecdt;
  }
  return vecdt;
}

/** \brief Return true if the data types for two functions are compatible.
 *
 *  Two functions are compatible if a single local variable can be
 *  used to hold their return values and therefore implying that the
 *  same return mechanism can be used for the functions.
 *
 */
LOGICAL
cmpat_func(int d1, int d2)
{
  int fv1, fv2;

  if (d1 == d2)
    return TRUE;
  fv1 = dtypeinfo[DTY(d1)].fval;
  assert(fv1 >= 0, "cmpat_func1:bad dtype", d1, 3);
  fv2 = dtypeinfo[DTY(d2)].fval;
  assert(fv2 >= 0, "cmpat_func2:bad dtype", d2, 3);
  if (fv1 == fv2)
    return TRUE;
  return FALSE;
}

/** \brief Put into the character array pointed to by ptr, the print
 * representation of dtype.
 */
void
getdtype(DTYPE dtype, char *ptr)
{
  int i;
  ADSC *ad;
  int numdim;
  char *p;
  char temp[100];

  p = ptr;
  *p = 0;
  for (; dtype != 0 && p - ptr <= 70; dtype = DTY(dtype + 1)) {
    if (dtype <= 0 || dtype >= stb.dt_avail || DTY(dtype) <= 0 ||
        DTY(dtype) > TY_MAX) {
      interr("getdtype: bad dtype", dtype, 3);
      strcpy(p, "????");
      break;
    }
    /*strcpy(p, dtypes[DTY(dtype)]);*/
    strcpy(p, stb.tynames[DTY(dtype)]);
    p += strlen(p);

    switch (DTY(dtype)) {
    case TY_STRUCT:
    case TY_UNION:
      if ((i = DTY(dtype + 3))) {
#if DEBUG
        assert(i > NOSYM, "getdtype: bad tag", dtype, 3);
#endif
        sprintf(p, "/%s/", SYMNAME(i));
        p += strlen(p);
      }
      return;

    case TY_ARRAY:
      *p++ = ' ';
      *p++ = '(';
      if (DTY(dtype + 2) != 0) {
        ad = AD_DPTR(dtype);
        numdim = AD_NUMDIM(ad);
        if (numdim < 1 || numdim > 7) {
          interr("getdtype:bad numdim", 0, 1);
          numdim = 0;
        }
        for (i = 0; i < numdim; i++) {
          sprintf(p, "%s:", getprint(AD_LWBD(ad, i)));
          p += strlen(p);
          sprintf(p, "%s", getprint(AD_UPBD(ad, i)));
          p += strlen(p);
          if (i != numdim - 1)
            *p++ = ',';
        }
      }
      strcpy(p, ") of ");
      p += 5;
      break;

    case TY_PTR:
      break;

    case TY_CHAR:
    case TY_NCHAR:
      if (dtype != DT_ASSCHAR && dtype != DT_ASSNCHAR)
        sprintf(p, "*%d", (int)DTY(dtype + 1));
      else
        sprintf(p, "*(*)");
      return;
    case TY_VECT:
      snprintf(temp, sizeof(temp), "%ld ", DTY(dtype + 2));
      strcat(p, temp);
      break;

    default:
      return;
    }
  }

}

/** Compute total number of elements in this array - if a dimension is
 * not known, estimate. We assume that we already know that dtype
 * is an array reference (either an array or a pointer to an array)
 */
ISZ_T
extent_of(DTYPE dtype)
{
#define DEFAULT_DIM_SIZE 127

  int i;
  ADSC *ad;
  int numdim;
  ISZ_T dim_size;
  ISZ_T size = 1;

  for (; dtype != 0; dtype = DTY(dtype + 1)) {
    if (dtype <= 0 || dtype >= stb.dt_avail || DTY(dtype) <= 0 ||
        DTY(dtype) > TY_MAX) {
      interr("getdtype: bad dtype", dtype, 3);
      break;
    }

    switch (DTY(dtype)) {

    case TY_ARRAY:
      if (DTY(dtype + 2) != 0) {
        ad = AD_DPTR(dtype);
        numdim = AD_NUMDIM(ad);
        if (numdim < 1 || numdim > 7) {
          interr("extent_of: bad numdim", 0, 1);
          numdim = 0;
        }
        for (i = 0; i < numdim; i++) {
          if (STYPEG(AD_LWBD(ad, i)) != ST_CONST || AD_UPBD(ad, i) == 0 ||
              STYPEG(AD_UPBD(ad, i)) != ST_CONST)
            dim_size = DEFAULT_DIM_SIZE;
          else
            dim_size =
                ad_val_of(AD_UPBD(ad, i)) - ad_val_of(AD_LWBD(ad, i)) + 1;
          size *= dim_size;
        }
      }
      break;

    default:
      return size;
    }
  }

  return size;
}

ISZ_T
ad_val_of(int sym)
{
  if (XBIT(68, 0x1))
    return get_isz_cval(sym);
  return CONVAL2G(sym);
}

/** \brief Create a constant sym entry which reflects the type of an array
 * bound/extent.
 */
int
get_bnd_con(ISZ_T v)
{
  INT num[2];

  if (XBIT(68, 0x1)) {
    ISZ_2_INT64(v, num);
    return getcon(num, DT_INT8);
  }
  num[0] = 0;
  num[1] = v;
  return getcon(num, DT_INT);
}

/** \brief Given a constant symbol, return its numerical value.
 */
ISZ_T
get_bnd_cval(int con)
{
  INT int64[2];
  ISZ_T isz;

  if (con == 0)
    return 0;
#if DEBUG
  assert(STYPEG(con) == ST_CONST, "get_bnd_cval-not ST_CONST", con, 0);
  assert(DT_ISINT(DTYPEG(con)), "get_bnd_cval-not int const", con, 0);
#endif
  int64[0] = CONVAL1G(con);
  int64[1] = CONVAL2G(con);
  INT64_2_ISZ(int64, isz);
  return isz;
}

int
_dmp_dent(int dtypeind, FILE *outfile)
{
  char buf[256];
  int retval;
  ADSC *ad;
  int numdim;
  int i;
  int paramct, dpdsc;

  if (outfile == 0)
    outfile = stderr;

  if (dtypeind < 1 || dtypeind >= stb.dt_avail) {
    fprintf(outfile, "dtype index (%d) out of range in dmp_dent\n", dtypeind);
    return 0;
  }
  buf[0] = '\0';
  fprintf(outfile, " %5d    ", dtypeind);
  switch (DTY(dtypeind)) {
  case TY_WORD:
  case TY_DWORD:
  case TY_HOLL:
  case TY_BINT:
  case TY_UBINT:
  case TY_SINT:
  case TY_USINT:
  case TY_INT:
  case TY_UINT:
  case TY_REAL:
  case TY_DBLE:
  case TY_QUAD:
  case TY_CMPLX:
  case TY_DCMPLX:
  case TY_BLOG:
  case TY_SLOG:
  case TY_LOG:
  case TY_NUMERIC:
  case TY_ANY:
  case TY_INT8:
  case TY_UINT8:
  case TY_LOG8:
  case TY_128:
  case TY_256:
  case TY_512:
  case TY_INT128:
  case TY_UINT128:
  case TY_LOG128:
  case TY_FLOAT128:
  case TY_CMPLX128:
    retval = 1;
    break;
  case TY_CHAR:
  case TY_NCHAR:
    retval = 2;
    break;
  case TY_PTR:
    fprintf(outfile, "ptr     dtype=%5d\n", (int)DTY(dtypeind + 1));
    retval = 2;
    break;
  case TY_ARRAY:
    retval = 3;
    fprintf(outfile, "array   dtype=%5d   desc   =%9" ISZ_PF "d\n",
            (int)DTY(dtypeind + 1), DTY(dtypeind + 2));
    if (DTY(dtypeind + 2) == 0) {
      fprintf(outfile, "(No array desc)\n");
      break;
    }
    ad = AD_DPTR(dtypeind);
    numdim = AD_NUMDIM(ad);
    if (numdim < 1 || numdim > 7) {
      interr("dmp_dent:bad numdim", 0, 1);
      numdim = 0;
    }
    fprintf(outfile,
            "numdim: %d   scheck: %d   zbase: %d   numelm: %d   sdsc: %d\n",
            numdim, AD_SCHECK(ad), AD_ZBASE(ad), AD_NUMELM(ad), AD_SDSC(ad));
    for (i = 0; i < numdim; i++)
      fprintf(outfile, "%1d:     mlpyr: %d   lwbd: %d   upbd: %d\n", i + 1,
              AD_MLPYR(ad, i), AD_LWBD(ad, i), AD_UPBD(ad, i));
    break;
  case TY_STRUCT:
  case TY_UNION:
    fprintf(outfile, "%s  sptr =%5d   size  =%5" ISZ_PF "d",
            stb.tynames[DTY(dtypeind)], (int)DTY(dtypeind + 1),
            DTY(dtypeind + 2));
    fprintf(outfile, "   tag=%5d   align=%3d", (int)DTY(dtypeind + 3),
            (int)DTY(dtypeind + 4));
    fprintf(outfile, "   ict=%p\n", get_getitem_p(DTY(dtypeind + 5)));
    retval = 6;
    break;
  case TY_PROC:
    paramct = DTY(dtypeind + 3);
    dpdsc = DTY(dtypeind + 4);
    fprintf(outfile, "proc    dtype=%5ld  interface=%5ld  paramct=%3d  "
                     "dpdsc=%5d  fval=%5ld\n",
            DTY(dtypeind + 1), DTY(dtypeind + 2), paramct, dpdsc,
            DTY(dtypeind + 5));
    for (i = 0; i < paramct; i++) {
      fprintf(outfile, "     arg %d: %d\n", i + 1, aux.dpdsc_base[dpdsc + i]);
    }
    retval = 6;
    break;
  case TY_VECT:
    fprintf(outfile, "vect   dtype=%3d   n =%2ld\n        ",
            (int)DTY(dtypeind + 1), DTY(dtypeind + 2));
    retval = 3;
    break;
  case TY_PFUNC:
    fprintf(outfile, "proto funct   dtype=%3d   params=%3d\n        ",
            (int)DTY(dtypeind + 1), (int)DTY(dtypeind + 2));
    retval = 3;
    break;
  case TY_PARAM:
    fprintf(outfile, "param  dtype = %3d  sptr =%3d   next=%3d\n",
            (int)DTY(dtypeind + 1), (int)DTY(dtypeind + 2),
            (int)DTY(dtypeind + 3));
    retval = 4;
    dtypeind = 0;
    break;
  default:
    interr("dmp_dent: unknown dtype", (int)DTY(dtypeind), 3);
    /* function param thing ?? */
    fprintf(outfile, "????  %5d\n", (int)DTY(dtypeind));
    retval = 1;
    dtypeind = 0;
    break;
  }
  if (dtypeind) {
    getdtype(dtypeind, buf);
    fprintf(outfile, "%s\n", buf);
  }
  return retval;
}

int
dmp_dent(int dtypeind)
{
  return _dmp_dent(dtypeind, gbl.dbgfil);
}

void
dmp_dtype(void)
{
  int i;

  fprintf(gbl.dbgfil, "\n------------------------\nDTYPE DUMP:\n");
  fprintf(gbl.dbgfil, "\ndt_base: %p   dt_size: %d   dt_avail: %d\n\n",
          (void *)stb.dt_base, stb.dt_size, stb.dt_avail);
  i = 1;
  fprintf(gbl.dbgfil, "index   dtype\n");
  while (i < stb.dt_avail) {
    i += dmp_dent(i);
  }
  fprintf(gbl.dbgfil, "\n------------------------\n");
}

/** \brief Compute the size of a data type
 *
 *  This machine dependent routine computes the size of a data type
 *  in terms of two quantities:
 *  \param size  - number of elements in the data type (returned thru size).
 *  \return scale - number of bytes in each element, expressed as a power
 *              of two (the return value of scale_of).
 *
 *  This routine will be used to take advantage of the machines that
 *  have the ability to add a scaled expression (multiplied by a power
 *  of two) to an address.  This is particularly useful for incrementing
 *  a pointer variable and array subscripting.
 *
 *  Note that for those machines that do not have this feature, scale_of
 *  returns a scale of 0 and size_of for size.
 */
int
Scale_Of(int dtype, ISZ_T *size)
{
  INT d;
  int tmp;
  int scale;
  ISZ_T tmpsiz;

  assert(dtype > 0 && dtype < stb.dt_avail, "Scale_Of:bad dtype", dtype, 3);

  switch ((d = DTY(dtype))) {
  case TY_WORD:
  case TY_DWORD:
  case TY_LOG:
  case TY_INT:
  case TY_UINT:
  case TY_FLOAT:
  case TY_PTR:
  case TY_SLOG:
  case TY_SINT:
  case TY_USINT:
  case TY_BINT:
  case TY_UBINT:
  case TY_BLOG:
  case TY_DBLE:
  case TY_CMPLX:
  case TY_DCMPLX:
  case TY_INT8:
  case TY_UINT8:
  case TY_LOG8:
  case TY_128:
  case TY_256:
  case TY_512:
  case TY_INT128:
  case TY_UINT128:
  case TY_LOG128:
  case TY_FLOAT128:
  case TY_CMPLX128:
    scale = dtypeinfo[d].scale;
    *size = (unsigned)dtypeinfo[d].size >> scale;
    return scale;

  case TY_HOLL:
  case TY_CHAR:
    if (dtype == DT_ASSCHAR)
      interr("Scale_Of: attempt to size assumed size character", 0, 3);
    *size = DTY(dtype + 1);
    return 0;

  case TY_NCHAR:
    if (dtype == DT_ASSNCHAR)
      interr("Scale_Of: attempt to size assumed size ncharacter", 0, 3);
    *size = 2 * DTY(dtype + 1);
    return 0;

  case TY_ARRAY:
    if ((d = DTY(dtype + 2)) <= 0) {
      interr("Scale_Of: no ad", (int)d, 3);
      d = DTY(dtype + 2) = 1;
    }
    tmp = Scale_Of((int)DTY(dtype + 1), &tmpsiz);
    *size = d * tmpsiz;
    return tmp;

  case TY_STRUCT:
  case TY_UNION:
    if (DTY(dtype + 2) < 0)
    {
      interr("Scale_Of: 0 size struct", 0, 3);
      *size = 4;
      return 0;
    } else {
      *size = DTY(dtype + 2);
      return 0;
    }

  case TY_VECT:
    d = DTY(dtype + 2);
    if (d == 3)
      d = 4;
    tmp = Scale_Of((int)DTY(dtype + 1), &tmpsiz);
    *size = d * tmpsiz;
    return tmp;

  default:
    interr("Scale_Of: bad dtype ", (int)DTY(dtype), 3);
    *size = 1;
    return 0;
  }
}

int
scale_of(int dtype, INT *size)
{
  int scale;
  ISZ_T tmpsiz;

  scale = Scale_Of(dtype, &tmpsiz);
  *size = tmpsiz;
  return scale;
}

/** \brief Get FVAL field of a data type
 *
 *  \return 0 if reg, 1 if mem.
 */
int
fval_of(int dtype)
{
  int fv;

  assert(dtype > 0 && dtype < stb.dt_avail, "fval_of:bad dtype", dtype, 3);

  fv = dtypeinfo[DTY(dtype)].fval & 0x3;
  assert(fv <= 1, "fval_of: bad dtype, dt is", dtype, 3);
  return fv;
}

#define SS2 0x8e
#define SS3 0x8f

/** Get number of kanji characters
 *
 * \param length Length in bytes of character string
 */
int
kanji_len(unsigned char *p, int len)
{
  int count = 0;
  int val;

  while (len > 0) {
    val = *p;
    count++;
    if ((val & 0x80) == 0 || len <= 1) /* ASCII */
      len--, p++;
    else if (val == SS2) /* JIS 8-bit character */
      len -= 2, p += 2;
    else if (val == SS3 && len >= 3) /* Graphic Character */
      len -= 3, p += 3;
    else /* Kanji */
      len -= 2, p += 2;
  }

  return count;
}

/** \brief Extract necessary bytes from character string in order to return
 * integer (16-bit) representation of one kanji char.
 *
 * \param p the character string
 * \param len number of bytes in string p
 * \return number of EUC bytes used up
 */
int
kanji_char(unsigned char *p, int len, int *bytes)
{
  int val = *p;

  if ((val & 0x80) == 0 || len <= 1) /* ASCII */
    *bytes = 1;
  else if (val == SS2) /* JIS 8-bit character */
    *bytes = 2, val = *(p + 1);
  else if (val == SS3 && len >= 3) /* Graphic Character */
    *bytes = 3, val = ((*(p + 1) << 8) | (*(p + 2) & 0x7F));
  else /* Kanji */
    *bytes = 2, val = ((val << 8) | *(p + 1));

  return val;
}

/** \brief Get number of bytes needed for kanji characters in string prefix
 *
 * \param p ptr to EUC string
 * \param newlen number of kanji chars required from string prefix
 * \param len total number of bytes in string
 * \return number of bytes required for newlen chars
 */
int
kanji_prefix(unsigned char *p, int newlen, int len)
{
  unsigned char *begin;
  int bytes;

  begin = p;
  while (newlen-- > 0) {
    (void)kanji_char(p, len, &bytes);
    p += bytes;
    len -= bytes;
  }

  return (p - begin);
}

