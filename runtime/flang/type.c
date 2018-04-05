/*
 * Copyright (c) 2010-2018, NVIDIA CORPORATION.  All rights reserved.
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

/* clang-format off */

/** \file
 * \brief F2003 polymorphic/OOP runtime support
 */

#include "type.h"
#include "f90alloc.h"
#include "stdioInterf.h"

static struct type_desc *I8(__f03_ty_to_id)[];

void ENTF90(SET_INTRIN_TYPE, set_intrin_type)(F90_Desc *dd,
                                              __INT_T intrin_type);

#define ARG1_PTR 0x1
#define ARG1_ALLOC 0x2
#define ARG2_PTR 0x4
#define ARG2_ALLOC 0x8
#define ARG2_INTRIN 0x10

__LOG_T
ENTF90(SAME_TYPE_AS, same_type_as)
(void *ab, OBJECT_DESC *ad, void *bb, OBJECT_DESC *bd, __INT_T flag, ...)
{
  OBJECT_DESC *t1 = ad, *t2 = bd;
  TYPE_DESC *atd, *btd;

  if (!ad || !bd)
    return 0;

  if (flag) {
    va_list va;
    int is_unl_poly = 0;
    va_start(va, flag);
    if (flag & (ARG1_PTR | ARG1_ALLOC)) {
      OBJECT_DESC *vatd = va_arg(va, OBJECT_DESC *);
      if (!((flag & ARG1_PTR) &&
            ENTFTN(ASSOCIATED, associated)(ab, (F90_Desc *)ad, 0, 0)) &&
          !I8(__fort_allocated)(ab)) {
        t1 = vatd;
        is_unl_poly |= t1->tag ==  __POLY && t1->baseTag == __POLY;
      }
    }
    if (flag & (ARG2_PTR | ARG2_ALLOC)) {
      OBJECT_DESC *vatd = va_arg(va, OBJECT_DESC *);
      if (!((flag & ARG2_PTR) &&
            ENTFTN(ASSOCIATED, associated)(bb, (F90_Desc *)bd, 0, 0)) &&
          !I8(__fort_allocated)(bb)) {
        t2 = vatd;
        is_unl_poly |= t2->tag ==  __POLY && t2->baseTag == __POLY;
      }
    }
    va_end(va);
    if (is_unl_poly)
      return 0;
  }

  atd = t1->type ? t1->type : (TYPE_DESC *)t1;
  btd = t2->type ? t2->type : (TYPE_DESC *)t2;
  return atd == btd ? GET_DIST_TRUE_LOG : 0;
}

__LOG_T
ENTF90(EXTENDS_TYPE_OF, extends_type_of)
(void *ab, OBJECT_DESC *ad, void *bb, OBJECT_DESC *bd, __INT_T flag, ...)
{
  OBJECT_DESC *t1 = ad, *t2 = bd;
  TYPE_DESC *atd, *btd;

  if (!ad || !bd)
    return 0;

  if (flag) {
    va_list va;
    int is_unl_poly_arg1 = 0, is_unl_poly_arg2 = 0;

    va_start(va, flag);
    if (flag & (ARG1_PTR | ARG1_ALLOC)) {
      OBJECT_DESC *vatd = va_arg(va, OBJECT_DESC *);
      if (!((flag & ARG1_PTR) &&
            ENTFTN(ASSOCIATED, associated)(ab, (F90_Desc *)ad, 0, 0)) &&
          !I8(__fort_allocated)(ab)) {
        t1 = vatd;
        is_unl_poly_arg1 = t1->tag ==  __POLY && t1->baseTag == __POLY;
      }
    }
    if (flag & (ARG2_PTR | ARG2_ALLOC)) {
      OBJECT_DESC *vatd = va_arg(va, OBJECT_DESC *);
      if (!((flag & ARG2_PTR) &&
            ENTFTN(ASSOCIATED, associated)(bb, (F90_Desc *)bd, 0, 0)) &&
          !I8(__fort_allocated)(bb)) {
        t2 = vatd;
        is_unl_poly_arg2 = t2->tag ==  __POLY && t2->baseTag == __POLY;
      }
    }
    va_end(va);

    if (is_unl_poly_arg2) {
      /* if second argument is unlimited polymorphic and it's
       * disassociated pointer or unallocated allocatable, then return TRUE.
       */
      return GET_DIST_TRUE_LOG;
    }
    if (is_unl_poly_arg1) {
      /* if first argument is unlimited polymorphic and it's either a
       * disassociated pointer or unallocated allocatable, then return FALSE.
       */
      return 0;
    }
  }

  atd = (t1->type /*&& t1->tag > 0 && t1->tag < __NTYPES*/) ? t1->type
                                                            : (TYPE_DESC *)t1;
  btd = (t2->type /*&& t2->tag > 0 && t2->tag < __NTYPES*/) ? t2->type
                                                            : (TYPE_DESC *)t2;
  if (atd == btd)
    return GET_DIST_TRUE_LOG;

  if (atd->obj.level > btd->obj.level) {
    __INT_T offset = (btd->obj.level + 1) * sizeof(__POINT_T);
    TYPE_DESC *parent = *((TYPE_DESC **)(((char *)atd) - offset));
    if (btd == parent)
      return GET_DIST_TRUE_LOG;
  }

  return 0;
}

/* Identical to same_type_as() above apart from name and result type. */
__LOG8_T
ENTF90(KSAME_TYPE_AS, ksame_type_as)
(void *ab, OBJECT_DESC *ad, void *bb, OBJECT_DESC *bd, __INT_T flag, ...)
{
  OBJECT_DESC *t1 = ad, *t2 = bd;
  TYPE_DESC *atd, *btd;

  if (!ad || !bd)
    return 0;

  if (flag) {
    va_list va;
    int is_unl_poly = 0;
    va_start(va, flag);
    if (flag & (ARG1_PTR | ARG1_ALLOC)) {
      OBJECT_DESC *vatd = va_arg(va, OBJECT_DESC *);
      if (!((flag & ARG1_PTR) &&
            ENTFTN(ASSOCIATED, associated)(ab, (F90_Desc *)ad, 0, 0)) &&
          !I8(__fort_allocated)(ab)) {
        t1 = vatd;
        is_unl_poly |= t1->tag ==  __POLY && t1->baseTag == __POLY;
      }
    }
    if (flag & (ARG2_PTR | ARG2_ALLOC)) {
      OBJECT_DESC *vatd = va_arg(va, OBJECT_DESC *);
      if (!((flag & ARG2_PTR) &&
            ENTFTN(ASSOCIATED, associated)(bb, (F90_Desc *)bd, 0, 0)) &&
          !I8(__fort_allocated)(bb)) {
        t2 = vatd;
        is_unl_poly |= t2->tag ==  __POLY && t2->baseTag == __POLY;
      }
    }
    va_end(va);
    if (is_unl_poly)
      return 0;
  }

  atd = t1->type ? t1->type : (TYPE_DESC *)t1;
  btd = t2->type ? t2->type : (TYPE_DESC *)t2;
  return atd == btd ? GET_DIST_TRUE_LOG : 0;
}

/* Identical to extends_type_of() above apart from name and result type. */
__LOG8_T
ENTF90(KEXTENDS_TYPE_OF, kextends_type_of)
(void *ab, OBJECT_DESC *ad, void *bb, OBJECT_DESC *bd, __INT_T flag, ...)
{
  OBJECT_DESC *t1 = ad, *t2 = bd;
  TYPE_DESC *atd, *btd;

  if (!ad || !bd)
    return 0;

  if (flag) {
    va_list va;
    int is_unl_poly_arg1 = 0, is_unl_poly_arg2 = 0;

    va_start(va, flag);
    if (flag & (ARG1_PTR | ARG1_ALLOC)) {
      OBJECT_DESC *vatd = va_arg(va, OBJECT_DESC *);
      if (!((flag & ARG1_PTR) &&
            ENTFTN(ASSOCIATED, associated)(ab, (F90_Desc *)ad, 0, 0)) &&
          !I8(__fort_allocated)(ab)) {
        t1 = vatd;
        is_unl_poly_arg1 = t1->tag ==  __POLY && t1->baseTag == __POLY;
      }
    }
    if (flag & (ARG2_PTR | ARG2_ALLOC)) {
      OBJECT_DESC *vatd = va_arg(va, OBJECT_DESC *);
      if (!((flag & ARG2_PTR) &&
            ENTFTN(ASSOCIATED, associated)(bb, (F90_Desc *)bd, 0, 0)) &&
          !I8(__fort_allocated)(bb)) {
        t2 = vatd;
        is_unl_poly_arg2 = t2->tag ==  __POLY && t2->baseTag == __POLY;
      }
    }
    va_end(va);

    if (is_unl_poly_arg2) {
      /* if second argument is unlimited polymorphic and it's
       * disassociated pointer or unallocated allocatable, then return TRUE.
       */
      return GET_DIST_TRUE_LOG;
    }
    if (is_unl_poly_arg1) {
      /* if first argument is unlimited polymorphic and it's either a
       * disassociated pointer or unallocated allocatable, then return FALSE.
       */
      return 0;
    }
  }

  atd = (t1->type /*&& t1->tag > 0 && t1->tag < __NTYPES*/) ? t1->type
                                                            : (TYPE_DESC *)t1;
  btd = (t2->type /*&& t2->tag > 0 && t2->tag < __NTYPES*/) ? t2->type
                                                            : (TYPE_DESC *)t2;
  if (atd == btd)
    return GET_DIST_TRUE_LOG;

  if (atd->obj.level > btd->obj.level) {
    __INT_T offset = (btd->obj.level + 1) * sizeof(__POINT_T);
    TYPE_DESC *parent = *((TYPE_DESC **)(((char *)atd) - offset));
    if (btd == parent)
      return GET_DIST_TRUE_LOG;
  }

  return 0;
}

void ENTF90(SET_TYPE, set_type)(F90_Desc *dd, OBJECT_DESC *td)
{
  OBJECT_DESC *td2 = (OBJECT_DESC *)dd;
  TYPE_DESC *type = td->type;

  if (type) {
    td2->type = type;
    if (type == I8(__f03_ty_to_id)[__STR]) {
      td2->size = td->size;
    }
  } else {
    td2->type = (TYPE_DESC *)td;
  }
}

void ENTF90(TEST_AND_SET_TYPE, test_and_set_type)(F90_Desc *dd, OBJECT_DESC *td)
{
  OBJECT_DESC *td2 = (OBJECT_DESC *)dd;
  TYPE_DESC *type = td->type;

  if (type) {
    td2->type = type;
    if (type == I8(__f03_ty_to_id)[__STR]) {
      td2->size = td->size;
    }
  } else if (td->tag > 0 && td->tag < __NTYPES) {
    td2->type = (TYPE_DESC *)td;
  }
}

__INT_T
ENTF90(GET_OBJECT_SIZE, get_object_size)(F90_Desc *d)
{
  TYPE_DESC *td;
  OBJECT_DESC *od = (OBJECT_DESC *)d;

  if (!od || !od->type)
    return 0;

  td = od->type;
  return td->obj.size;
}

__INT8_T
ENTF90(KGET_OBJECT_SIZE, kget_object_size)(F90_Desc *d)
{
  TYPE_DESC *td;
  OBJECT_DESC *od = (OBJECT_DESC *)d;

  if (!od || !od->type)
    return (__INT8_T)0;

  td = od->type;
  return (__INT8_T)td->obj.size;
}

static void
process_final_procedures(char *area, F90_Desc *sd)
{
  /* See also Cuda Fortran version in rte/cudafor/hammer/src/dev_allo.c
   * call dev_process_final_procedures()
   */
  OBJECT_DESC *src = (OBJECT_DESC *)sd;
  TYPE_DESC *src_td, *tmp_td;
  __INT_T rank;
  FINAL_TABLE(finals);
  __LOG_T g1;
  int is_elemental;

  is_elemental = 0;
  if (src) {
    tmp_td = src->type;
    if (tmp_td && (tmp_td->obj.tag > 0 && tmp_td->obj.tag < __NTYPES)) {
      src_td = tmp_td;
    } else {
      src_td = 0;
    }
  } else {
    return;
  }

  if (!src_td)
    return;

  if (src_td->finals) {

    finals = src_td->finals;
    rank = (sd->tag == __DESC) ? F90_RANK_G(sd) : 0;

    if (rank && finals[rank]) {
      /* array case */
      (finals[rank])(area, (char *)sd);
    } else if (!rank && finals[0]) {
      /* scalar case */
      (finals[0])(area, (char *)src_td);
    } else if (finals[MAXDIMS + 1]) {
      /* elemental case */
      if (!rank) {
        (finals[MAXDIMS + 1])(area, (char *)sd);
      } else {
        is_elemental = 1;
      }
    }
  } else {
    rank = 0;
    finals = 0;
  }

  if (src_td->layout) {
    LAYOUT_DESC *ld = src_td->layout;
    F90_Desc *fd;
    char *ptr1[1] = {0};
    char *ptr2[1] = {0};
    char *cb, *db;
    __LOG_T g1;
    for (; ld->tag != 0; ld++) {
      if ((ld->tag != 'T' && ld->tag != 'D' && ld->tag != 'P' &&
           ld->tag != 'F') ||
          ld->offset < 0) {
        continue;
      }
      fd = (ld->desc_offset >= 0) ? (F90_Desc *)(area + ld->desc_offset) : 0;
      if (!fd && ld->tag == 'F') {
        cb = area + ld->offset;
        if (cb && !fd && ld->declType) {
          process_final_procedures(cb, (F90_Desc *)ld->declType);
        }
      } else if (fd && (fd->tag == __DESC || fd->tag == __POLY)) {
        if (rank == 0) {
          __fort_bcopy((char *)ptr2, area + ld->offset, sizeof(char *));
          cb = ptr2[0];
          g1 = (fd) ? ENTFTN(ASSOCIATED, associated)(cb, fd, 0, 0) : 0;
          if ((ld->length == 0) || (!g1 && !I8(__fort_allocated)(cb))) {
            continue;
          }
          process_final_procedures(cb, fd);
        }
      }
    }
  }
  if (is_elemental && rank > 0 && finals && finals[0]) {
    int i;
    int src_sz = sd->lsize * (size_t)src_td->obj.size;
    for (i = 0; i < src_sz; i += src_td->obj.size) {
      g1 = ENTFTN(ASSOCIATED, associated)(area + i, sd, 0, 0);
      if (!g1 && !I8(__fort_allocated)(area + i)) {
        continue;
      }
      finals[MAXDIMS + 1](area + i, (char *)src_td);
    }
  }

  if (((F90_Desc *)src_td)->tag == __POLY && src_td->obj.level > 0) {
    /* process parent finals */
    __INT_T offset = (src_td->obj.level) * sizeof(__POINT_T);
    TYPE_DESC *parent = *((TYPE_DESC **)(((char *)src_td) - offset));

    if (rank > 0) {
      int i;
      int src_sz = sd->lsize * (size_t)src_td->obj.size;
      for (i = 0; i < src_sz; i += src_td->obj.size) {
        process_final_procedures(area + i, (F90_Desc *)parent);
      }
    } else {
      process_final_procedures(area, (F90_Desc *)parent);
    }
  }
}

void ENTF90(FINALIZE, finalize)(char *area, F90_Desc *sd)
{
  /* See also Cuda Fortran version in rte/cudafor/hammer/src/dev_allo.c
   * call DEV_FINALIZE().
   */

  process_final_procedures(area, sd);
}

void ENTF90(DEALLOC_POLY_MBR03,
            dealloc_poly_mbr03)(F90_Desc *sd, __STAT_T *stat, char *area,
                                __INT_T *firsttime, DCHAR(errmsg) DCLEN(errmsg))
{

  OBJECT_DESC *src = (OBJECT_DESC *)sd;
  TYPE_DESC *src_td;

  if (!I8(__fort_allocated)(area))
    return;

  if (src) {
    src_td = (src->type) ? src->type : 0;
  } else {
    src_td = 0;
  }

  process_final_procedures(area, sd);

  if (src_td && src_td->layout) {

    LAYOUT_DESC *ld = src_td->layout;
    F90_Desc *fd;
    char *ptr1[1] = {0};
    char *ptr2[1] = {0};
    char *cb, *db;
    __LOG_T g1;

    for (; ld->tag != 0; ld++) {
      if ((ld->tag != 'T' && ld->tag != 'D' && ld->tag != 'P' &&
           ld->tag != 'F') ||
          ld->offset < 0) {
        continue;
      }
      fd = (ld->desc_offset >= 0) ? (F90_Desc *)(area + ld->desc_offset) : 0;
      if (ld->tag == 'F') {
        continue;

      } else {
        __fort_bcopy((char *)ptr2, area + ld->offset, sizeof(char *));
        cb = ptr2[0];
      }
      if (ld->tag == 'F') {
        if (ld->declType)
          process_final_procedures(cb, (F90_Desc *)ld->declType);
        continue;
      }
      g1 = (fd) ? ENTFTN(ASSOCIATED, associated)(cb, fd, 0, 0) : 0;
      if (!g1 && !I8(__fort_allocated)(cb)) {
        continue;
      }
    }
  }
  ENTF90(DEALLOC_MBR03, dealloc_mbr03)
  (stat, area, firsttime, CADR(errmsg), CLEN(errmsg));
}

void ENTF90(DEALLOC_POLY03, dealloc_poly03)(F90_Desc *sd, __STAT_T *stat,
                                            char *area, __INT_T *firsttime,
                                            DCHAR(errmsg) DCLEN(errmsg))
{
  OBJECT_DESC *src = (OBJECT_DESC *)sd;
  TYPE_DESC *src_td;

  if (!I8(__fort_allocated)(area))
    return;

  if (src) {
    src_td = (src->type) ? src->type : 0;
  } else {
    src_td = 0;
  }

  process_final_procedures(area, sd);

  if (src_td && src_td->layout) {

    LAYOUT_DESC *ld = src_td->layout;
    F90_Desc *fd;
    char *ptr1[1] = {0};
    char *ptr2[1] = {0};
    char *cb;
    __LOG_T g1;

    for (; ld->tag != 0; ld++) {
      if ((ld->tag != 'T' && ld->tag != 'D' && ld->tag != 'P' &&
           ld->tag != 'F') ||
          ld->offset < 0) {
        continue;
      }
      fd = (ld->desc_offset >= 0) ? (F90_Desc *)(area + ld->desc_offset) : 0;
      if (ld->tag == 'F') {
          continue;
      } else {
        __fort_bcopy((char *)ptr2, area + ld->offset, sizeof(char *));
        cb = ptr2[0];
      }
      g1 = (fd) ? ENTFTN(ASSOCIATED, associated)(cb, fd, 0, 0) : 0;
      if (!g1 && !I8(__fort_allocated)(cb)) {
        continue;
      }
      if (ld->tag == 'F') {
        if (ld->declType)
          process_final_procedures(cb, (F90_Desc *)ld->declType);
        continue;
      }
      if (fd) {
        if (ld->tag == 'T' && src_td->obj.tag == __POLY) {
            ENTF90(DEALLOC_POLY_MBR03, dealloc_poly_mbr03)
            (fd, stat, cb, firsttime, CADR(errmsg), CLEN(errmsg));
        }
      }
    }
  }
  ENTF90(DEALLOC03, dealloc03)
  (stat, area, firsttime, CADR(errmsg), CLEN(errmsg));
}

static void sourced_alloc_and_assign_array(int extent, char *ab, char *bb, TYPE_DESC *td);

/* Used with F2003 sourced allocation. Allocate and assign
 * components of a derived type. This function assumes
 * that it was called by POLY_ASN() below.
 */
static void
sourced_alloc_and_assign(char *ab, char *bb, TYPE_DESC *td)
{
  LAYOUT_DESC *ld;
  char *cb, *db;
  __INT_T one = 1;
  __INT_T zero = 0;
  __INT_T len, i;
  __INT_T kind = __NONE;
  char *errmsg = "sourced_alloc_and_assign: malloc error";

  if (td == 0 || td->layout == 0) {
    return;
  }

  for (ld = td->layout; ld->tag != 0; ld++) {
    if ((ld->tag != 'F' && ld->tag != 'T') || ld->offset < 0) {
      continue;
    }
    if (ld->tag == 'F') {
      if (ld->declType != NULL) {
        cb = (bb + ld->offset);
        db = (ab + ld->offset);
        sourced_alloc_and_assign(db, cb, ld->declType);
      }
      continue;
    }
    cb = *(char **)(bb + ld->offset);
     
    if (ld->desc_offset > 0) {
      F90_Desc *fd = (F90_Desc *)(ab + ld->desc_offset);
      if (!ENTFTN(ASSOCIATED, associated)(cb, fd, 0, 0) &&
          !I8(__fort_allocated)(cb)) {
        continue;
      }
      if (fd->tag == __DESC && fd->rank > 0) {
        len = fd->lsize * fd->len;
      } else {
        len = ENTF90(GET_OBJECT_SIZE, get_object_size)(fd);
      }
      
      ENTF90(PTR_SRC_ALLOC03, ptr_src_alloc03)
        (fd, &one, &kind, &len, (__STAT_T *)(ENTCOMN(0, 0)), &db,
          (__POINT_T *)(ENTCOMN(0, 0)), &zero, errmsg, strlen(errmsg));
      *(char **)(ab + ld->offset) = db;
      __fort_bcopy(db, cb, len);
      if (ld->tag == 'T') {
        OBJECT_DESC *od = (OBJECT_DESC *)fd;
        if (fd->tag == __DESC && fd->rank > 0) {
          sourced_alloc_and_assign_array(fd->lsize, db, cb, od->type);
        } else {
          sourced_alloc_and_assign(db, cb, od->type);
        }
      }
    } else if ((len = ld->length) > 0) {
      ENTF90(PTR_ALLOC03, ptr_alloc03)
        (&one, &kind, &len, (__STAT_T *)(ENTCOMN(0, 0)), &db,
          (__POINT_T *)(ENTCOMN(0, 0)), &zero, errmsg, strlen(errmsg));
      *(char **)(ab + ld->offset) = db;
      if (I8(__fort_allocated)(cb)) {
        __fort_bcopy(db, cb, len);
        if (ld->tag == 'T') {
          sourced_alloc_and_assign(db, cb, ld->declType);
        }
      }
    }
  }
}

/* Perform sourced_alloc_and_assign on each element of an array. */
static void
sourced_alloc_and_assign_array(int extent, char *ab, char *bb, TYPE_DESC *td)
{
  if (td != 0) {
    const int elem_size = td->obj.size;
    const int end_offset = extent * elem_size;
    int elem_offset;
    for (elem_offset = 0; elem_offset < end_offset; elem_offset += elem_size) {
      sourced_alloc_and_assign(ab + elem_offset, bb + elem_offset, td);
    }
  }
}

void ENTF90(POLY_ASN, poly_asn)(char *ab, F90_Desc *ad, char *bb, F90_Desc *bd,
                                __INT_T flag)
{
  /* Copy the contents of object bb to object ab
   * Assumes destination descriptor, ad, is a full descriptor.
   * If flag == 0, then source descriptor, bd, is a scalar "fake" descriptor
   * If flag == 1, assume full descriptor for bd
   * If flag == 2, assume full descriptor for bd and copy bd into ad.
   */

  OBJECT_DESC *src = (OBJECT_DESC *)bd;
  OBJECT_DESC *dest = (OBJECT_DESC *)ad;
  TYPE_DESC *src_td, *dest_td;
  int src_sz, dest_sz, sz;
  int dest_is_array, src_is_array, i;

  if (dest) {
    dest_td = dest->type ? dest->type : (TYPE_DESC *)ad;
  } else {
    dest_td = 0;
  }

  if (src && (flag || src->tag == __DESC || src->tag == __POLY)) {
    src_td = src->type ? src->type : (TYPE_DESC *)bd;
  } else {
    src_td = 0;
  }
  dest_is_array = src_is_array = 0;
  if (src_td) {
    if (bd && bd->tag == __DESC && bd->rank > 0) {
      src_sz = bd->lsize * (size_t)src_td->obj.size;
      src_is_array = 1;
    } else if (bd && (flag || bd->tag == __POLY || bd->tag == __DESC)) {
      src_sz = (size_t)src_td->obj.size;
    } else {
      src_sz = 0;
    }
  } else if (bd && !flag && ISSCALAR(bd) && bd->tag != __POLY &&
             bd->tag < __NTYPES) {
#if defined(WINNT)
    src_sz = __get_fort_size_of(bd->tag);
#else
    src_sz = __fort_size_of[bd->tag];
#endif
  } else {
    src_sz = 0;
  }
  if (dest_td) {
    if (ad && ad->tag == __DESC && ad->rank > 0) {
      dest_sz = ad->lsize * (size_t)dest_td->obj.size;
      dest_is_array = 1;
    } else if (ad && ad->tag == __DESC && dest_td &&
               dest_td->obj.tag == __POLY && ad->len > 0 && !ad->lsize &&
               !ad->gsize && ad->kind > 0 && ad->kind < __NTYPES) {
      dest_sz = (size_t)dest_td->obj.size * ad->len;
    } else if (!src_sz || (ad && ad->tag == __DESC && dest_td &&
                           dest_td->obj.tag == __POLY)) {
      dest_sz = (size_t)dest_td->obj.size;
    } else {
      dest_sz = 0;
    }
  } else {
    dest_sz = 0;
  }

  if (src_sz && src_td && src_td->obj.tag == __POLY &&
      (!ad || ad->tag != __DESC || !dest_td || dest_td->obj.tag != __POLY))
    sz = src_sz;
  else if (!src_sz ||
           (ad && ad->tag == __DESC && dest_td && dest_td->obj.tag == __POLY &&
            (!src_td || src_td->obj.tag != __POLY)))
    sz = dest_sz;
  else
    sz = (src_sz > dest_sz) ? src_sz : dest_sz;

  if (src_td && src_td->obj.size && dest_is_array && !src_is_array) {
    for (i = 0; i < dest_sz; i += src_td->obj.size) {
      __fort_bcopy(ab + i, bb, src_sz);
    }
  } else if (src_sz && !flag && ISSCALAR(bd) && bd->tag != __POLY &&
             bd->tag < __NTYPES) {
    for (i = 0; i < sz; i += src_sz) {
      __fort_bcopy(ab + i, bb, src_sz);
    }
  } else {
    __fort_bcopy(ab, bb, sz);
  }

  if (flag && ad && bd && ad != bd && F90_TAG_G(bd) == __DESC &&
      (F90_TAG_G(ad) == __DESC || flag == 2)) {
    __fort_bcopy((char *)ad, (char *)bd, SIZE_OF_RANK_n_ARRAY_DESC(F90_RANK_G(bd)));
    SET_F90_DIST_DESC_PTR(ad, F90_RANK_G(ad));
    /* check for align-target to self */
    if (DIST_ALIGN_TARGET_G(ad) == ad) {
      DIST_ALIGN_TARGET_P(ad, ad);
    }
  }

  if (flag) {
    if (src_td && (src_td->obj.tag > 0 && src_td->obj.tag < __NTYPES) &&
        !src_is_array) {
      sourced_alloc_and_assign(ab, bb, src_td);
    } else if (dest_is_array) {
      sourced_alloc_and_assign_array(ad->lsize, ab, bb, dest_td);
    }
  }
}

void I8(__fort_dump_type)(TYPE_DESC *d)
{
  fprintf(__io_stderr(), "Polymorphic variable type '");
  switch (d->obj.baseTag) {
  case __NONE:
    fprintf(__io_stderr(), "__NONE'\n");
    return;
  case __SHORT:
    fprintf(__io_stderr(), "__SHORT'\n");
    break;
  case __USHORT:
    fprintf(__io_stderr(), "__USHORT'\n");
    break;
  case __CINT:
    fprintf(__io_stderr(), "__CINT'\n");
    break;
  case __UINT:
    fprintf(__io_stderr(), "__UINT'\n");
    break;
  case __LONG:
    fprintf(__io_stderr(), "__LONG'\n");
    break;
  case __ULONG:
    fprintf(__io_stderr(), "__FLOAT'\n");
    break;
  case __DOUBLE:
    fprintf(__io_stderr(), "__DOUBLE'\n");
    break;
  case __CPLX8:
    fprintf(__io_stderr(), "__CPLX8'\n");
    break;
  case __CPLX16:
    fprintf(__io_stderr(), "__CPLX16'\n");
    break;
  case __CHAR:
    fprintf(__io_stderr(), "__CHAR'\n");
    break;
  case __UCHAR:
    fprintf(__io_stderr(), "__UCHAR'\n");
    break;
  case __LONGDOUBLE:
    fprintf(__io_stderr(), "__LONGDOUBLE'\n");
    break;
  case __STR:
    fprintf(__io_stderr(), "__STR'\n");
    break;
  case __LONGLONG:
    fprintf(__io_stderr(), "__LONGLONG'\n");
    break;
  case __ULONGLONG:
    fprintf(__io_stderr(), "__ULONGLONG'\n");
    break;
  case __LOG1:
    fprintf(__io_stderr(), "__LOG1'\n");
    break;
  case __LOG2:
    fprintf(__io_stderr(), "__LOG2'\n");
    break;
  case __LOG4:
    fprintf(__io_stderr(), "__LOG4'\n");
  case __LOG8:
    fprintf(__io_stderr(), "__LOG8'\n");
    break;
  case __WORD4:
    fprintf(__io_stderr(), "__WORD4'\n");
    break;
  case __WORD8:
    fprintf(__io_stderr(), "__WORD8'\n");
    break;
  case __NCHAR:
    fprintf(__io_stderr(), "__NCHAR'\n");
    break;
  case __INT2:
    fprintf(__io_stderr(), "__INT2'\n");
    break;
  case __INT4:
    fprintf(__io_stderr(), "__INT4'\n");
    break;
  case __INT8:
    fprintf(__io_stderr(), "__INT8'\n");
    break;
  case __REAL4:
    fprintf(__io_stderr(), "__REAL4'\n");
    break;
  case __REAL8:
    fprintf(__io_stderr(), "__REAL8'\n");
    break;
  case __REAL16:
    fprintf(__io_stderr(), "__REAL16'\n");
    break;
  case __CPLX32:
    fprintf(__io_stderr(), "__CPLX32'\n");
    break;
  case __WORD16:
    fprintf(__io_stderr(), "__WORD16'\n");
    break;
  case __INT1:
    fprintf(__io_stderr(), "__INT1'\n");
    break;
  case __DERIVED:
    fprintf(__io_stderr(), "__DERIVED'\n");
    break;
  case __PROC:
    fprintf(__io_stderr(), "__PROC'\n");
    break;
  case __DESC:
    fprintf(__io_stderr(), "__DESC'\n");
    break;
  case __SKED:
    fprintf(__io_stderr(), "__SKED'\n");
    break;
  case __M128:
    fprintf(__io_stderr(), "__M128'\n");
    break;
  case __M256:
    fprintf(__io_stderr(), "__M256'\n");
    break;
  case __INT16:
    fprintf(__io_stderr(), "__INT16'\n");
    break;
  case __LOG16:
    fprintf(__io_stderr(), "__LOG16'\n");
    break;
  case __QREAL16:
    fprintf(__io_stderr(), "__QREAL16'\n");
    break;
  case __QCPLX32:
    fprintf(__io_stderr(), "__QCPLX32'\n");
    break;
  case __POLY:
    fprintf(__io_stderr(), "__POLY'\n");
    break;
  case __PROCPTR:
    fprintf(__io_stderr(), "__PROCPTR'\n");
    break;
  default:
    fprintf(__io_stderr(), "unknown (%d)'\n", d->obj.baseTag);
    return;
  }

  fprintf(__io_stderr(), "Size: %d\n", d->obj.size);
  fprintf(__io_stderr(), "Type Descriptor:\n\t'%s'\n", d->name);
  if (d->obj.level > 0) {
    TYPE_DESC *parent;
    __INT_T offset, level;
    fprintf(__io_stderr(), "(Child Type)\n");
    fprintf(__io_stderr(), "Parent Descriptor%s\n",
            (d->obj.level == 1) ? ":" : "s:");
    for (level = d->obj.level - 1; level >= 0; --level) {
      offset = (level + 1) * sizeof(__POINT_T);
      TYPE_DESC *parent = *((TYPE_DESC **)(((char *)d) - offset));
      fprintf(__io_stderr(), "\t'%s'\n", parent->name);
    }

    if (d->func_table) {
      fprintf(__io_stderr(), "function table: %p\n", *(d->func_table));
    }

  } else
    fprintf(__io_stderr(), "(Base Type)\n");

  if (d->layout != 0) {
    LAYOUT_DESC *ld;
    fprintf(__io_stderr(), "Layout descriptors:\n");
    for (ld = d->layout; ld->tag != 0; ld++) {
      if ((/*ld->tag != 'P' &&*/ ld->tag != 'T') || ld->offset < 0) {
        continue;
      }
      fprintf(__io_stderr(),
        "  tag=%c offset=%d desc_offset=%d length=%d declType=%p\n",
        ld->tag, ld->offset, ld->desc_offset, ld->length, ld->declType);
    }
  }
}

static struct type_desc I8(__f03_short_td) = {
    {__POLY, __SHORT, 0, sizeof(__SHORT_T), 0, 0, 0, 0, 0, &I8(__f03_short_td)},
    0,
    0,
    0,
    0,
    "__f03_short_td"};

static struct type_desc I8(__f03_ushort_td) = {{__POLY, __USHORT, 0,
                                                sizeof(__USHORT_T), 0, 0, 0, 0,
                                                0, &I8(__f03_ushort_td)},
                                               0,
                                               0,
                                               0,
                                               0,
                                               "__f03_ushort_td"};

static struct type_desc I8(__f03_cint_td) = {
    {__POLY, __CINT, 0, sizeof(__CINT_T), 0, 0, 0, 0, 0, &I8(__f03_cint_td)},
    0,
    0,
    0,
    0,
    "__f03_cint_td"};

static struct type_desc I8(__f03_uint_td) = {
    {__POLY, __UINT, 0, sizeof(__UINT_T), 0, 0, 0, 0, 0, &I8(__f03_uint_td)},
    0,
    0,
    0,
    0,
    "__f03_uint_td"};

static struct type_desc I8(__f03_long_td) = {
    {__POLY, __LONG, 0, sizeof(__LONG_T), 0, 0, 0, 0, 0, &I8(__f03_long_td)},
    0,
    0,
    0,
    0,
    "__f03_long_td"};

static struct type_desc I8(__f03_ulong_td) = {
    {__POLY, __ULONG, 0, sizeof(__ULONG_T), 0, 0, 0, 0, 0, &I8(__f03_ulong_td)},
    0,
    0,
    0,
    0,
    "__f03_ulong_td"};

static struct type_desc I8(__f03_float_td) = {
    {__POLY, __FLOAT, 0, sizeof(__FLOAT_T), 0, 0, 0, 0, 0, &I8(__f03_float_td)},
    0,
    0,
    0,
    0,
    "__f03_float_td"};

static struct type_desc I8(__f03_double_td) = {{__POLY, __DOUBLE, 0,
                                                sizeof(__DOUBLE_T), 0, 0, 0, 0,
                                                0, &I8(__f03_double_td)},
                                               0,
                                               0,
                                               0,
                                               0,
                                               "__f03_double_td"};

static struct type_desc I8(__f03_cplx8_td) = {
    {__POLY, __CPLX8, 0, sizeof(__CPLX8_T), 0, 0, 0, 0, 0, &I8(__f03_cplx8_td)},
    0,
    0,
    0,
    0,
    "__f03_cplx8_td"};

static struct type_desc I8(__f03_cplx16_td) = {{__POLY, __CPLX16, 0,
                                                sizeof(__CPLX16_T), 0, 0, 0, 0,
                                                0, &I8(__f03_cplx16_td)},
                                               0,
                                               0,
                                               0,
                                               0,
                                               "__f03_cplx16_td"};

static struct type_desc I8(__f03_char_td) = {
    {__POLY, __CHAR, 0, sizeof(__CHAR_T), 0, 0, 0, 0, 0, &I8(__f03_char_td)},
    0,
    0,
    0,
    0,
    "__f03_char_td"};

static struct type_desc I8(__f03_uchar_td) = {
    {__POLY, __UCHAR, 0, sizeof(__UCHAR_T), 0, 0, 0, 0, 0, &I8(__f03_uchar_td)},
    0,
    0,
    0,
    0,
    "__f03_uchar_td"};

static struct type_desc I8(__f03_longdouble_td) = {
    {__POLY, __LONGDOUBLE, 0, sizeof(__LONGDOUBLE_T), 0, 0, 0, 0, 0,
     &I8(__f03_longdouble_td)},
    0,
    0,
    0,
    0,
    "__f03_longdouble_td"};

static struct type_desc I8(__f03_str_td) = {
    {__POLY, __STR, 0, sizeof(__STR_T), 0, 0, 0, 0, 0, &I8(__f03_str_td)},
    0,
    0,
    0,
    0,
    "__f03_str_td"};

static struct type_desc I8(__f03_longlong_td) = {{__POLY, __LONGLONG, 0,
                                                  sizeof(__LONGLONG_T), 0, 0, 0,
                                                  0, 0, &I8(__f03_longlong_td)},
                                                 0,
                                                 0,
                                                 0,
                                                 0,
                                                 "__f03_longlong_td"};

static struct type_desc I8(__f03_ulonglong_td) = {
    {__POLY, __ULONGLONG, 0, sizeof(__ULONGLONG_T), 0, 0, 0, 0, 0,
     &I8(__f03_ulonglong_td)},
    0,
    0,
    0,
    0,
    "__f03_ulonglong_td"};

static struct type_desc I8(__f03_log1_td) = {
    {__POLY, __LOG1, 0, sizeof(__LOG1_T), 0, 0, 0, 0, 0, &I8(__f03_log1_td)},
    0,
    0,
    0,
    0,
    "__f03_log1_td"};

static struct type_desc I8(__f03_log2_td) = {
    {__POLY, __LOG2, 0, sizeof(__LOG2_T), 0, 0, 0, 0, 0, &I8(__f03_log2_td)},
    0,
    0,
    0,
    0,
    "__f03_log2_td"};

static struct type_desc I8(__f03_log4_td) = {
    {__POLY, __LOG4, 0, sizeof(__LOG4_T), 0, 0, 0, 0, 0, &I8(__f03_log4_td)},
    0,
    0,
    0,
    0,
    "__f03_log4_td"};

static struct type_desc I8(__f03_log8_td) = {
    {__POLY, __LOG8, 0, sizeof(__LOG8_T), 0, 0, 0, 0, 0, &I8(__f03_log8_td)},
    0,
    0,
    0,
    0,
    "__f03_log8_td"};

static struct type_desc I8(__f03_word4_td) = {
    {__POLY, __WORD4, 0, sizeof(__WORD4_T), 0, 0, 0, 0, 0, &I8(__f03_word4_td)},
    0,
    0,
    0,
    0,
    "__f03_word4_td"};

static struct type_desc I8(__f03_word8_td) = {
    {__POLY, __WORD8, 0, sizeof(__WORD8_T), 0, 0, 0, 0, 0, &I8(__f03_word8_td)},
    0,
    0,
    0,
    0,
    "__f03_word8_td"};

static struct type_desc I8(__f03_nchar_td) = {
    {__POLY, __NCHAR, 0, sizeof(__NCHAR_T), 0, 0, 0, 0, 0, &I8(__f03_nchar_td)},
    0,
    0,
    0,
    0,
    "__f03_nchar_td"};

static struct type_desc I8(__f03_int2_td) = {
    {__POLY, __INT2, 0, sizeof(__INT2_T), 0, 0, 0, 0, 0, &I8(__f03_int2_td)},
    0,
    0,
    0,
    0,
    "__f03_int2_td"};

static struct type_desc I8(__f03_int4_td) = {
    {__POLY, __INT4, 0, sizeof(__INT4_T), 0, 0, 0, 0, 0, &I8(__f03_int4_td)},
    0,
    0,
    0,
    0,
    "__f03_int4_td"};

static struct type_desc I8(__f03_int8_td) = {
    {__POLY, __INT8, 0, sizeof(__INT8_T), 0, 0, 0, 0, 0, &I8(__f03_int8_td)},
    0,
    0,
    0,
    0,
    "__f03_int8_td"};

static struct type_desc I8(__f03_real4_td) = {
    {__POLY, __REAL4, 0, sizeof(__REAL4_T), 0, 0, 0, 0, 0, &I8(__f03_real4_td)},
    0,
    0,
    0,
    0,
    "__f03_real4_td"};

static struct type_desc I8(__f03_real8_td) = {
    {__POLY, __REAL8, 0, sizeof(__REAL8_T), 0, 0, 0, 0, 0, &I8(__f03_real8_td)},
    0,
    0,
    0,
    0,
    "__f03_real8_td"};

static struct type_desc I8(__f03_real16_td) = {{__POLY, __REAL16, 0,
                                                sizeof(__REAL16_T), 0, 0, 0, 0,
                                                0, &I8(__f03_real16_td)},
                                               0,
                                               0,
                                               0,
                                               0,
                                               "__f03_real16_td"};

static struct type_desc I8(__f03_cplx32_td) = {{__POLY, __CPLX32, 0,
                                                sizeof(__CPLX32_T), 0, 0, 0, 0,
                                                0, &I8(__f03_cplx32_td)},
                                               0,
                                               0,
                                               0,
                                               0,
                                               "__f03_cplx32_td"};

static struct type_desc I8(__f03_word16_td) = {{__POLY, __WORD16, 0,
                                                sizeof(__WORD16_T), 0, 0, 0, 0,
                                                0, &I8(__f03_word16_td)},
                                               0,
                                               0,
                                               0,
                                               0,
                                               "__f03_word16_td"};

static struct type_desc I8(__f03_int1_td) = {
    {__POLY, __INT1, 0, sizeof(__INT1_T), 0, 0, 0, 0, 0, &I8(__f03_int1_td)},
    0,
    0,
    0,
    0,
    "__f03_int1_td"};

/* The order of the type descriptors below must correspond with their type
 * code in _DIST_TYPE enum defined in pghpft.h
 */
static struct type_desc *I8(__f03_ty_to_id)[__NTYPES] = {
    0,
    &I8(__f03_short_td),
    &I8(__f03_ushort_td),
    &I8(__f03_cint_td),
    &I8(__f03_uint_td),
    &I8(__f03_long_td),
    &I8(__f03_ulong_td),
    &I8(__f03_float_td),
    &I8(__f03_double_td),
    &I8(__f03_cplx8_td),
    &I8(__f03_cplx16_td),
    &I8(__f03_char_td),
    &I8(__f03_uchar_td),
    &I8(__f03_longdouble_td),
    &I8(__f03_str_td),
    &I8(__f03_longlong_td),
    &I8(__f03_ulonglong_td),
    &I8(__f03_log1_td),
    &I8(__f03_log2_td),
    &I8(__f03_log4_td),
    &I8(__f03_log8_td),
    &I8(__f03_word4_td),
    &I8(__f03_word8_td),
    &I8(__f03_nchar_td),
    &I8(__f03_int2_td),
    &I8(__f03_int4_td),
    &I8(__f03_int8_td),
    &I8(__f03_real4_td),
    &I8(__f03_real8_td),
    &I8(__f03_real16_td),
    &I8(__f03_cplx32_td),
    &I8(__f03_word16_td),
    &I8(__f03_int1_td),
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0};

void ENTF90(SET_INTRIN_TYPE, set_intrin_type)(F90_Desc *dd, __INT_T intrin_type)
{
  OBJECT_DESC *od = (OBJECT_DESC *)dd;

#if DEBUG
  if (!od)
    return;

  od->type = (intrin_type >= 0 && intrin_type < __NTYPES)
                 ? I8(__f03_ty_to_id)[intrin_type]
                 : 0;

  if (od->type == 0) {
    __fort_abort("set_intrin_type: Illegal intrinsic type");
  }
#else
    od->type = I8(__f03_ty_to_id)[intrin_type];
#endif
}

__LOG_T
ENTF90(SAME_INTRIN_TYPE_AS, same_intrin_type_as)
(void *ab, OBJECT_DESC *ad, void *bb, __INT_T intrin_type, __INT_T flag, ...)

{
  TYPE_DESC *atd;
  TYPE_DESC *btd;
  OBJECT_DESC *t1, *t2;
  __LOG_T g1, g2;
  va_list va;

  if (!ad)
    return 0;

  if (flag) {
    va_start(va, flag);
    if (flag & ARG1_PTR) {
      /* first arg is a pointer */

      g1 = ENTFTN(ASSOCIATED, associated)(ab, (F90_Desc *)ad, 0, 0);
      if (!g1 && !I8(__fort_allocated)(ab)) {
        t1 = va_arg(va, OBJECT_DESC *); /* get declared type */
      } else {
        t1 = ad; /* use dynamic (runtime) type */
      }
    } else if (flag & ARG1_ALLOC) {
      /* first arg is an allocatable */
      g1 = I8(__fort_allocated)(ab);
      if (!g1) {
        t1 = va_arg(va, OBJECT_DESC *); /* get declared type */
      } else {
        t1 = ad; /* use dynamic (runtime) type */
      }
    } else {
      t1 = ad; /* use dynamic (runtime) type */
    }
  } else {
    t1 = ad;
  }

  btd = I8(__f03_ty_to_id)[intrin_type];
  t2 = &(btd->obj);

  return ENTF90(SAME_TYPE_AS, same_type_as)(ab, t1, bb, t2, 0);
}

__LOG8_T
ENTF90(KSAME_INTRIN_TYPE_AS, ksame_intrin_type_as)
(void *ab, OBJECT_DESC *ad, void *bb, __INT_T intrin_type, __INT_T flag, ...)

{
  TYPE_DESC *atd;
  TYPE_DESC *btd;
  OBJECT_DESC *t1, *t2;
  __LOG_T g1, g2;
  va_list va;

  if (!ad)
    return 0;

  if (flag) {
    va_start(va, flag);
    if (flag & ARG1_PTR) {
      /* first arg is a pointer */

      g1 = ENTFTN(ASSOCIATED, associated)(ab, (F90_Desc *)ad, 0, 0);
      if (!g1 && !I8(__fort_allocated)(ab)) {
        t1 = va_arg(va, OBJECT_DESC *); /* get declared type */
      } else {
        t1 = ad; /* use dynamic (runtime) type */
      }
    } else if (flag & ARG1_ALLOC) {
      /* first arg is an allocatable */
      g1 = I8(__fort_allocated)(ab);
      if (!g1) {
        t1 = va_arg(va, OBJECT_DESC *); /* get declared type */
      } else {
        t1 = ad; /* use dynamic (runtime) type */
      }
    } else {
      t1 = ad; /* use dynamic (runtime) type */
    }

  } else {
    t1 = ad;
  }

  btd = I8(__f03_ty_to_id)[intrin_type];
  t2 = &(btd->obj);

  return ENTF90(KSAME_TYPE_AS, ksame_type_as)(ab, t1, bb, t2, 0);
}

void ENTF90(POLY_ASN_SRC_INTRIN,
            poly_asn_src_intrin)(char *ab, F90_Desc *ad, char *bb,
                                 __INT_T intrin_type, __INT_T flag)
{

  F90_Desc *bd;
  TYPE_DESC *td;

  td = I8(__f03_ty_to_id)[intrin_type];
#if DEBUG
  if (td == 0) {
    __fort_abort("poly_asn_src_intrin: Illegal intrinsic type");
  }
#endif
  bd = (F90_Desc *)&(td->obj);

  ENTF90(POLY_ASN, poly_asn)(ab, ad, bb, bd, flag);
}

void ENTF90(POLY_ASN_DEST_INTRIN,
            poly_asn_dest_intrin)(char *ab, __INT_T intrin_type, char *bb,
                                  F90_Desc *bd, __INT_T flag)
{

  F90_Desc *ad;
  TYPE_DESC *td;

  td = I8(__f03_ty_to_id)[intrin_type];
#if DEBUG
  if (td == 0) {
    __fort_abort("poly_asn_dest_intrin: Illegal intrinsic type");
  }
#endif
  ad = (F90_Desc *)&(td->obj);

  ENTF90(POLY_ASN, poly_asn)(ab, ad, bb, bd, flag);
}

void ENTF90(INIT_UNL_POLY_DESC, init_unl_poly_desc)(F90_Desc *dd, F90_Desc *sd,
                                                    __INT_T kind)
{
  OBJECT_DESC *od = (OBJECT_DESC *)sd;
    if (sd && F90_TAG_G(sd) == __DESC) {
      __fort_bcopy((char *)dd, (char *)sd, SIZE_OF_RANK_n_ARRAY_DESC(F90_RANK_G(sd)));
      SET_F90_DIST_DESC_PTR(dd, F90_RANK_G(dd));
      /* check for align-target to self */
      if (DIST_ALIGN_TARGET_G(dd) == dd) {
        DIST_ALIGN_TARGET_P(dd, dd);
      }
      dd->kind = kind;
    } else {
      dd->len = (sd && sd->tag == __DESC) ? sd->len : 0;
      dd->tag = __DESC;
      dd->rank = 0;
      dd->lsize = 0;
      dd->gsize = 0;
      dd->kind = kind;
    }
}

void ENTF90(INIT_FROM_DESC, init_from_desc)(void *object,
                                            const F90_Desc *desc,
                                            int rank)
{
  if (object && desc) {

    OBJECT_DESC *obj_desc = (OBJECT_DESC *) desc;
    size_t items = 1;
    size_t index[MAXDIMS];
    TYPE_DESC *type_desc = obj_desc->type;
    int j;
    size_t element_bytes = 0;
    void *prototype = NULL;

    if (desc->tag == __DESC) {
      if (desc->rank < rank)
        rank = desc->rank;
      if (rank > 0) {
        items = desc->lsize;
        for (j = 0; j < rank; ++j) {
          index[j] = 0;
        }
      }
    }

    if (type_desc)
      obj_desc = &type_desc->obj;
    else
      type_desc = (TYPE_DESC *) obj_desc;
    element_bytes = obj_desc->size;
    prototype = obj_desc->prototype;

    while (items-- > 0) {
      int do_increment = 1;
      char *element = object;
      size_t offset = 0;
      for (j = 0; j < rank; ++j) {
        offset += index[j] * desc->dim[j].lstride;
        if (do_increment) {
          if (++index[j] >= desc->dim[j].extent)
            index[j] = 0;
          else
            do_increment = 0;
        }
      }
      element = (char *) object + element_bytes * offset;
      if (prototype)
        memcpy(element, prototype, element_bytes);
      else
        memset(element, 0, element_bytes);
    }
  }
}

void 
ENTF90(ASN_CLOSURE, asn_closure) 
      (PROC_DESC * pdesc, void * closure)
{
   pdesc->tag = __PROCPTR;
   pdesc->closure = closure;
} 


void
ENTF90(COPY_PROC_DESC, copy_proc_desc)
      (PROC_DESC *dd, PROC_DESC *sd)
{
   dd->tag = __PROCPTR;
   dd->closure = sd->closure;
}

