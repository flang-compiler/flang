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

/* clang-format off */

/* red_maxloc.c -- intrinsic reduction function */

#include "stdioInterf.h"
#include "fioMacros.h"
#include "red.h"

MLOCFNLKN(>, maxloc_int1, __INT1_T, 1)
MLOCFNLKN(>, maxloc_int2, __INT2_T, 1)
MLOCFNLKN(>, maxloc_int4, __INT4_T, 1)
MLOCFNLKN(>, maxloc_int8, __INT8_T, 1)
MLOCFNLKN(>, maxloc_real4, __REAL4_T, 1)
MLOCFNLKN(>, maxloc_real8, __REAL8_T, 1)
MLOCFNLKN(>, maxloc_real16, __REAL16_T, 1)
MLOCSTRFNLKN(>, maxloc_str, __STR_T, 1)

MLOCFNLKN(>, maxloc_int1, __INT1_T, 2)
MLOCFNLKN(>, maxloc_int2, __INT2_T, 2)
MLOCFNLKN(>, maxloc_int4, __INT4_T, 2)
MLOCFNLKN(>, maxloc_int8, __INT8_T, 2)
MLOCFNLKN(>, maxloc_real4, __REAL4_T, 2)
MLOCFNLKN(>, maxloc_real8, __REAL8_T, 2)
MLOCFNLKN(>, maxloc_real16, __REAL16_T, 2)
MLOCSTRFNLKN(>, maxloc_str, __STR_T, 2)

MLOCFNLKN(>, maxloc_int1, __INT1_T, 4)
MLOCFNLKN(>, maxloc_int2, __INT2_T, 4)
MLOCFNLKN(>, maxloc_int4, __INT4_T, 4)
MLOCFNLKN(>, maxloc_int8, __INT8_T, 4)
MLOCFNLKN(>, maxloc_real4, __REAL4_T, 4)
MLOCFNLKN(>, maxloc_real8, __REAL8_T, 4)
MLOCFNLKN(>, maxloc_real16, __REAL16_T, 4)
MLOCSTRFNLKN(>, maxloc_str, __STR_T, 4)

MLOCFNLKN(>, maxloc_int1, __INT1_T, 8)
MLOCFNLKN(>, maxloc_int2, __INT2_T, 8)
MLOCFNLKN(>, maxloc_int4, __INT4_T, 8)
MLOCFNLKN(>, maxloc_int8, __INT8_T, 8)
MLOCFNLKN(>, maxloc_real4, __REAL4_T, 8)
MLOCFNLKN(>, maxloc_real8, __REAL8_T, 8)
MLOCFNLKN(>, maxloc_real16, __REAL16_T, 8)
MLOCSTRFNLKN(>, maxloc_str, __STR_T, 8)

MLOCFNG(>, maxloc_int1, __INT1_T)
MLOCFNG(>, maxloc_int2, __INT2_T)
MLOCFNG(>, maxloc_int4, __INT4_T)
MLOCFNG(>, maxloc_int8, __INT8_T)
MLOCFNG(>, maxloc_real4, __REAL4_T)
MLOCFNG(>, maxloc_real8, __REAL8_T)
MLOCFNG(>, maxloc_real16, __REAL16_T)
MLOCSTRFNG(>, maxloc_str, __STR_T)

static void (*l_maxloc[4][__NTYPES])() = TYPELIST3LK(l_maxloc_);
static void (*g_maxloc[__NTYPES])() = TYPELIST3(g_maxloc_);

KMLOCFNLKN(>, kmaxloc_int1, __INT1_T, 1)
KMLOCFNLKN(>, kmaxloc_int2, __INT2_T, 1)
KMLOCFNLKN(>, kmaxloc_int4, __INT4_T, 1)
KMLOCFNLKN(>, kmaxloc_int8, __INT8_T, 1)
KMLOCFNLKN(>, kmaxloc_real4, __REAL4_T, 1)
KMLOCFNLKN(>, kmaxloc_real8, __REAL8_T, 1)
KMLOCFNLKN(>, kmaxloc_real16, __REAL16_T, 1)
KMLOCSTRFNLKN(>, kmaxloc_str, __STR_T, 1)

KMLOCFNLKN(>, kmaxloc_int1, __INT1_T, 2)
KMLOCFNLKN(>, kmaxloc_int2, __INT2_T, 2)
KMLOCFNLKN(>, kmaxloc_int4, __INT4_T, 2)
KMLOCFNLKN(>, kmaxloc_int8, __INT8_T, 2)
KMLOCFNLKN(>, kmaxloc_real4, __REAL4_T, 2)
KMLOCFNLKN(>, kmaxloc_real8, __REAL8_T, 2)
KMLOCFNLKN(>, kmaxloc_real16, __REAL16_T, 2)
KMLOCSTRFNLKN(>, kmaxloc_str, __STR_T, 2)

KMLOCFNLKN(>, kmaxloc_int1, __INT1_T, 4)
KMLOCFNLKN(>, kmaxloc_int2, __INT2_T, 4)
KMLOCFNLKN(>, kmaxloc_int4, __INT4_T, 4)
KMLOCFNLKN(>, kmaxloc_int8, __INT8_T, 4)
KMLOCFNLKN(>, kmaxloc_real4, __REAL4_T, 4)
KMLOCFNLKN(>, kmaxloc_real8, __REAL8_T, 4)
KMLOCFNLKN(>, kmaxloc_real16, __REAL16_T, 4)
KMLOCSTRFNLKN(>, kmaxloc_str, __STR_T, 4)

KMLOCFNLKN(>, kmaxloc_int1, __INT1_T, 8)
KMLOCFNLKN(>, kmaxloc_int2, __INT2_T, 8)
KMLOCFNLKN(>, kmaxloc_int4, __INT4_T, 8)
KMLOCFNLKN(>, kmaxloc_int8, __INT8_T, 8)
KMLOCFNLKN(>, kmaxloc_real4, __REAL4_T, 8)
KMLOCFNLKN(>, kmaxloc_real8, __REAL8_T, 8)
KMLOCFNLKN(>, kmaxloc_real16, __REAL16_T, 8)
KMLOCSTRFNLKN(>, kmaxloc_str, __STR_T, 8)

KMLOCFNG(>, kmaxloc_int1, __INT1_T)
KMLOCFNG(>, kmaxloc_int2, __INT2_T)
KMLOCFNG(>, kmaxloc_int4, __INT4_T)
KMLOCFNG(>, kmaxloc_int8, __INT8_T)
KMLOCFNG(>, kmaxloc_real4, __REAL4_T)
KMLOCFNG(>, kmaxloc_real8, __REAL8_T)
KMLOCFNG(>, kmaxloc_real16, __REAL16_T)
KMLOCSTRFNG(>, kmaxloc_str, __STR_T)

static void (*l_kmaxloc[4][__NTYPES])() = TYPELIST3LK(l_kmaxloc_);
static void (*g_kmaxloc[__NTYPES])() = TYPELIST3(g_kmaxloc_);

/* dim absent */

void ENTFTN(MAXLOCS, maxlocs)(__INT_T *rb, char *ab, char *mb, F90_Desc *rs,
                              F90_Desc *as, F90_Desc *ms)
{
  red_parm z;
  double vb[4];
  char *strvb;
  int len;

  INIT_RED_PARM(z);
  __fort_red_what = "MAXLOC";

  z.kind = F90_KIND_G(as);
  z.len = F90_LEN_G(as);
  z.mask_present = (F90_TAG_G(ms) == __DESC && F90_RANK_G(ms) > 0);
  if (!z.mask_present) {
    z.lk_shift = GET_DIST_SHIFTS(__LOG);
  } else {
    z.lk_shift = GET_DIST_SHIFTS(F90_KIND_G(ms));
  }
  z.l_fn = l_maxloc[z.lk_shift][z.kind];
  z.g_fn = g_maxloc[z.kind];
  z.zb = GET_DIST_MINS(z.kind);

  if (z.kind == __STR) {
    strvb = (char *)__fort_gmalloc(z.len);
    memset(strvb, *((char *)z.zb), z.len);
    I8(__fort_red_scalarlk)(&z, strvb, ab, mb, rs, as, ms, rb, __MAXLOC);
    __fort_gfree(strvb);
  } else {
    I8(__fort_red_scalarlk)(&z, (char *)vb, ab, mb, rs, as, ms, rb, __MAXLOC);
  }
}

/* dim present */

void ENTFTN(MAXLOC, maxloc)(char *rb, char *ab, char *mb, char *db,
                            F90_Desc *rs, F90_Desc *as, F90_Desc *ms,
                            F90_Desc *ds)
{
  red_parm z;

  INIT_RED_PARM(z);
  __fort_red_what = "MAXLOC";

  z.kind = F90_KIND_G(as);
  z.len = F90_LEN_G(as);
  z.mask_present = (F90_TAG_G(ms) == __DESC && F90_RANK_G(ms) > 0);
  if (!z.mask_present) {
    z.lk_shift = GET_DIST_SHIFTS(__LOG);
  } else {
    z.lk_shift = GET_DIST_SHIFTS(F90_KIND_G(ms));
  }
  z.l_fn = l_maxloc[z.lk_shift][z.kind];
  z.g_fn = g_maxloc[z.kind];
  z.zb = GET_DIST_MINS(z.kind);
  if (z.kind == __STR)
    memset(rb, *((char *)z.zb), z.len);
  if (ISSCALAR(ms)) {
    DECL_HDR_VARS(ms2);

    mb = (char *)I8(__fort_create_conforming_mask_array)(__fort_red_what, ab, mb,
                                                        as, ms, ms2);
    I8(__fort_red_array)(&z, rb, ab, mb, db, rs, as, ms2, ds, __MAXLOC);
    __fort_gfree(mb);
  } else {
    I8(__fort_red_arraylk)(&z, rb, ab, mb, db, rs, as, ms, ds, __MAXLOC);
  }
}

/* dim absent */

void ENTFTN(KMAXLOCS, kmaxlocs)(__INT8_T *rb, char *ab, char *mb, F90_Desc *rs,
                                F90_Desc *as, F90_Desc *ms)
{
  red_parm z;
  double vb[4];
  char *strvb;

  INIT_RED_PARM(z);
  __fort_red_what = "MAXLOC";

  z.kind = F90_KIND_G(as);
  z.len = F90_LEN_G(as);
  z.mask_present = (F90_TAG_G(ms) == __DESC && F90_RANK_G(ms) > 0);
  if (!z.mask_present) {
    z.lk_shift = GET_DIST_SHIFTS(__LOG);
  } else {
    z.lk_shift = GET_DIST_SHIFTS(F90_KIND_G(ms));
  }
  z.l_fn = l_kmaxloc[z.lk_shift][z.kind];
  z.g_fn = g_kmaxloc[z.kind];
  z.zb = GET_DIST_MINS(z.kind);

  if (z.kind == __STR) {
    strvb = (char *)__fort_gmalloc(z.len);
    memset(strvb, *((char *)z.zb), z.len);
    I8(__fort_kred_scalarlk)(&z, strvb, ab, mb, rs, as, ms, rb, __MAXLOC);
    __fort_gfree(strvb);
  } else {
    I8(__fort_kred_scalarlk)(&z, (char *)vb, ab, mb, rs, as, ms, rb, __MAXLOC);
  }
}

/* dim present */

void ENTFTN(KMAXLOC, kmaxloc)(char *rb, char *ab, char *mb, char *db,
                              F90_Desc *rs, F90_Desc *as, F90_Desc *ms,
                              F90_Desc *ds)
{
  red_parm z;

  INIT_RED_PARM(z);
  __fort_red_what = "MAXLOC";

  z.kind = F90_KIND_G(as);
  z.len = F90_LEN_G(as);
  z.mask_present = (F90_TAG_G(ms) == __DESC && F90_RANK_G(ms) > 0);
  if (!z.mask_present) {
    z.lk_shift = GET_DIST_SHIFTS(__LOG);
  } else {
    z.lk_shift = GET_DIST_SHIFTS(F90_KIND_G(ms));
  }
  z.l_fn = l_kmaxloc[z.lk_shift][z.kind];
  z.g_fn = g_kmaxloc[z.kind];
  z.zb = GET_DIST_MINS(z.kind);
  if (z.kind == __STR)
    memset(rb, *((char *)z.zb), z.len);
  if (ISSCALAR(ms)) {
    DECL_HDR_VARS(ms2);

    mb = (char *)I8(__fort_create_conforming_mask_array)(__fort_red_what, ab, mb,
                                                        as, ms, ms2);
    I8(__fort_red_array)(&z, rb, ab, mb, db, rs, as, ms2, ds, __MAXLOC);
    __fort_gfree(mb);
  } else {
    I8(__fort_kred_arraylk)(&z, rb, ab, mb, db, rs, as, ms, ds, __MAXLOC);
  }
}
