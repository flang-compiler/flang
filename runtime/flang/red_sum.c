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

/* red_sum.c -- intrinsic reduction function */

#include "stdioInterf.h"
#include "fioMacros.h"
#include "red.h"

#define CSUMFN(NAME, RTYP, ATYP)                                               \
  static void l_##NAME(RTYP *r, __INT_T n, RTYP *v, __INT_T vs, __LOG_T *m,    \
                       __INT_T ms, __INT_T *loc, __INT_T li, __INT_T ls)       \
  {                                                                            \
    __INT_T i, j;                                                              \
    ATYP xr = r->r, xi = r->i;                                                 \
    __LOG_T mask_log;                                                          \
    if (ms == 0)                                                               \
      for (i = 0; n > 0; n--, i += vs) {                                       \
        xr += v[i].r;                                                          \
        xi += v[i].i;                                                          \
      }                                                                        \
    else {                                                                     \
      mask_log = GET_DIST_MASK_LOG;                                           \
      for (i = j = 0; n > 0; n--, i += vs, j += ms)                            \
        if (m[j] & mask_log) {                                                 \
          xr += v[i].r;                                                        \
          xi += v[i].i;                                                        \
        }                                                                      \
    }                                                                          \
    r->r = xr;                                                                 \
    r->i = xi;                                                                 \
  }                                                                            \
  static void g_##NAME(__INT_T n, RTYP *lr, RTYP *rr, void *lv, void *rv)      \
  {                                                                            \
    __INT_T i;                                                                 \
    for (i = 0; i < n; i++) {                                                  \
      lr[i].r += rr[i].r;                                                      \
      lr[i].i += rr[i].i;                                                      \
    }                                                                          \
  }

#define CSUMFNLKN(NAME, RTYP, ATYP, N)                                         \
  static void l_##NAME##l##N(RTYP *r, __INT_T n, RTYP *v, __INT_T vs,          \
                             __LOG##N##_T *m, __INT_T ms, __INT_T *loc,        \
                             __INT_T li, __INT_T ls)                           \
  {                                                                            \
    __INT_T i, j;                                                              \
    ATYP xr = r->r, xi = r->i;                                                 \
    __LOG##N##_T mask_log;                                                     \
    if (ms == 0)                                                               \
      for (i = 0; n > 0; n--, i += vs) {                                       \
        xr += v[i].r;                                                          \
        xi += v[i].i;                                                          \
      }                                                                        \
    else {                                                                     \
      mask_log = GET_DIST_MASK_LOG##N;                                        \
      for (i = j = 0; n > 0; n--, i += vs, j += ms)                            \
        if (m[j] & mask_log) {                                                 \
          xr += v[i].r;                                                        \
          xi += v[i].i;                                                        \
        }                                                                      \
    }                                                                          \
    r->r = xr;                                                                 \
    r->i = xi;                                                                 \
  }

ARITHFN(+, sum_int1, __INT1_T, long)
ARITHFN(+, sum_int2, __INT2_T, long)
ARITHFN(+, sum_int4, __INT4_T, long)
ARITHFN(+, sum_int8, __INT8_T, __INT8_T)
ARITHFN(+, sum_real4, __REAL4_T, __REAL4_T)
ARITHFN(+, sum_real8, __REAL8_T, __REAL8_T)
ARITHFN(+, sum_real16, __REAL16_T, __REAL16_T)
CSUMFN(sum_cplx8, __CPLX8_T, __REAL4_T)
CSUMFN(sum_cplx16, __CPLX16_T, __REAL8_T)
CSUMFN(sum_cplx32, __CPLX32_T, __REAL16_T)

ARITHFNLKN(+, sum_int1, __INT1_T, long, 1)
ARITHFNLKN(+, sum_int2, __INT2_T, long, 1)
ARITHFNLKN(+, sum_int4, __INT4_T, long, 1)
ARITHFNLKN(+, sum_int8, __INT8_T, __INT8_T, 1)
ARITHFNLKN(+, sum_real4, __REAL4_T, __REAL4_T, 1)
ARITHFNLKN(+, sum_real8, __REAL8_T, __REAL8_T, 1)
ARITHFNLKN(+, sum_real16, __REAL16_T, __REAL16_T, 1)
CSUMFNLKN(sum_cplx8, __CPLX8_T, __REAL4_T, 1)
CSUMFNLKN(sum_cplx16, __CPLX16_T, __REAL8_T, 1)
CSUMFNLKN(sum_cplx32, __CPLX32_T, __REAL16_T, 1)

ARITHFNLKN(+, sum_int1, __INT1_T, long, 2)
ARITHFNLKN(+, sum_int2, __INT2_T, long, 2)
ARITHFNLKN(+, sum_int4, __INT4_T, long, 2)
ARITHFNLKN(+, sum_int8, __INT8_T, __INT8_T, 2)
ARITHFNLKN(+, sum_real4, __REAL4_T, __REAL4_T, 2)
ARITHFNLKN(+, sum_real8, __REAL8_T, __REAL8_T, 2)
ARITHFNLKN(+, sum_real16, __REAL16_T, __REAL16_T, 2)
CSUMFNLKN(sum_cplx8, __CPLX8_T, __REAL4_T, 2)
CSUMFNLKN(sum_cplx16, __CPLX16_T, __REAL8_T, 2)
CSUMFNLKN(sum_cplx32, __CPLX32_T, __REAL16_T, 2)

ARITHFNLKN(+, sum_int1, __INT1_T, long, 4)
ARITHFNLKN(+, sum_int2, __INT2_T, long, 4)
ARITHFNLKN(+, sum_int4, __INT4_T, long, 4)
ARITHFNLKN(+, sum_int8, __INT8_T, __INT8_T, 4)
ARITHFNLKN(+, sum_real4, __REAL4_T, __REAL4_T, 4)
ARITHFNLKN(+, sum_real8, __REAL8_T, __REAL8_T, 4)
ARITHFNLKN(+, sum_real16, __REAL16_T, __REAL16_T, 4)
CSUMFNLKN(sum_cplx8, __CPLX8_T, __REAL4_T, 4)
CSUMFNLKN(sum_cplx16, __CPLX16_T, __REAL8_T, 4)
CSUMFNLKN(sum_cplx32, __CPLX32_T, __REAL16_T, 4)

ARITHFNLKN(+, sum_int1, __INT1_T, long, 8)
ARITHFNLKN(+, sum_int2, __INT2_T, long, 8)
ARITHFNLKN(+, sum_int4, __INT4_T, long, 8)
ARITHFNLKN(+, sum_int8, __INT8_T, __INT8_T, 8)
ARITHFNLKN(+, sum_real4, __REAL4_T, __REAL4_T, 8)
ARITHFNLKN(+, sum_real8, __REAL8_T, __REAL8_T, 8)
ARITHFNLKN(+, sum_real16, __REAL16_T, __REAL16_T, 8)
CSUMFNLKN(sum_cplx8, __CPLX8_T, __REAL4_T, 8)
CSUMFNLKN(sum_cplx16, __CPLX16_T, __REAL8_T, 8)
CSUMFNLKN(sum_cplx32, __CPLX32_T, __REAL16_T, 8)

static void (*l_sum[4][__NTYPES])() = TYPELIST1LK(l_sum_);
void (*I8(__fort_g_sum)[__NTYPES])() = TYPELIST1(g_sum_);

/* dim absent */

void ENTFTN(SUMS, sums)(char *rb, char *ab, char *mb, DECL_HDR_PTRS(rs),
                        F90_Desc *as, F90_Desc *ms)
{
  red_parm z;

  INIT_RED_PARM(z);
  __fort_red_what = "SUM";

  z.kind = F90_KIND_G(as);
  z.len = F90_LEN_G(as);
  z.mask_present = (F90_TAG_G(ms) == __DESC && F90_RANK_G(ms) > 0);
  if (!z.mask_present) {
    z.lk_shift = GET_DIST_SHIFTS(__LOG);
  } else {
    z.lk_shift = GET_DIST_SHIFTS(F90_KIND_G(ms));
  }
  z.l_fn = l_sum[z.lk_shift][z.kind];
  z.g_fn = I8(__fort_g_sum)[z.kind];
  z.zb = GET_DIST_ZED;
  I8(__fort_red_scalar)(&z, rb, ab, mb, rs, as, ms, NULL, __SUM);
}

/* dim present */

void ENTFTN(SUM, sum)(char *rb, char *ab, char *mb, char *db, DECL_HDR_PTRS(rs),
                      F90_Desc *as, F90_Desc *ms, F90_Desc *ds)
{
  red_parm z;

  INIT_RED_PARM(z);
  __fort_red_what = "SUM";

  z.kind = F90_KIND_G(as);
  z.len = F90_LEN_G(as);
  z.mask_present = (F90_TAG_G(ms) == __DESC && F90_RANK_G(ms) > 0);
  if (!z.mask_present) {
    z.lk_shift = GET_DIST_SHIFTS(__LOG);
  } else {
    z.lk_shift = GET_DIST_SHIFTS(F90_KIND_G(ms));
  }
  z.l_fn = l_sum[z.lk_shift][z.kind];
  z.g_fn = I8(__fort_g_sum)[z.kind];
  z.zb = GET_DIST_ZED;
  if (ISSCALAR(ms)) {
    DECL_HDR_VARS(ms2);

    mb = (char *)I8(__fort_create_conforming_mask_array)(__fort_red_what, ab, mb,
                                                        as, ms, ms2);
    I8(__fort_red_array)(&z, rb, ab, mb, db, rs, as, ms2, ds, __SUM);
    __fort_gfree(mb);
  } else {
    I8(__fort_red_array)(&z, rb, ab, mb, db, rs, as, ms, ds, __SUM);
  }
}

/* global SUM accumulation */

void ENTFTN(REDUCE_SUM, reduce_sum)(char *hb, __INT_T *dimsb, __INT_T *nargb,
                                    char *rb, DECL_HDR_PTRS(hd),
                                    F90_Desc *dimsd, F90_Desc *nargd,
                                    F90_Desc *rd)
{
#if defined(DEBUG)
  if (dimsd == NULL || dimsd->tag != __INT)
    __fort_abort("GLOBAL_SUM: invalid dims descriptor");
  if (nargd == NULL || nargd->tag != __INT)
    __fort_abort("REDUCE_SUM: invalid arg count descriptor");
  if (*nargb != 1)
    __fort_abort("REDUCE_SUM: arg count not 1");
#endif
  I8(__fort_global_reduce)(rb, hb, *dimsb, rd, hd, "SUM", I8(__fort_g_sum));
}

void ENTFTN(GLOBAL_SUM, global_sum)(char *rb, char *hb, __INT_T *dimsb,
                                    DECL_HDR_PTRS(rd), F90_Desc *hd,
                                    F90_Desc *dimsd)
{
  I8(__fort_global_reduce)(rb, hb, *dimsb, rd, hd, "SUM", I8(__fort_g_sum));
}
