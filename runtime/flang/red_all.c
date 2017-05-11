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

/* red_all.c -- intrinsic reduction function */

#include "stdioInterf.h"
#include "fioMacros.h"
#include "red.h"

static __INT_T mask_desc = __LOG; /* scalar mask descriptor */

LOGFN(&, all_log1, __LOG1_T)
LOGFN(&, all_log2, __LOG2_T)
LOGFN(&, all_log4, __LOG4_T)
LOGFN(&, all_log8, __LOG8_T)
LOGFN(&, all_int1, __INT1_T)
LOGFN(&, all_int2, __INT2_T)
LOGFN(&, all_int4, __INT4_T)
LOGFN(&, all_int8, __INT8_T)

LOGFNLKN(&, all_log1, __LOG1_T, 1)
LOGFNLKN(&, all_log2, __LOG2_T, 1)
LOGFNLKN(&, all_log4, __LOG4_T, 1)
LOGFNLKN(&, all_log8, __LOG8_T, 1)
LOGFNLKN(&, all_int1, __INT1_T, 1)
LOGFNLKN(&, all_int2, __INT2_T, 1)
LOGFNLKN(&, all_int4, __INT4_T, 1)
LOGFNLKN(&, all_int8, __INT8_T, 1)

LOGFNLKN(&, all_log1, __LOG1_T, 2)
LOGFNLKN(&, all_log2, __LOG2_T, 2)
LOGFNLKN(&, all_log4, __LOG4_T, 2)
LOGFNLKN(&, all_log8, __LOG8_T, 2)
LOGFNLKN(&, all_int1, __INT1_T, 2)
LOGFNLKN(&, all_int2, __INT2_T, 2)
LOGFNLKN(&, all_int4, __INT4_T, 2)
LOGFNLKN(&, all_int8, __INT8_T, 2)

LOGFNLKN(&, all_log1, __LOG1_T, 4)
LOGFNLKN(&, all_log2, __LOG2_T, 4)
LOGFNLKN(&, all_log4, __LOG4_T, 4)
LOGFNLKN(&, all_log8, __LOG8_T, 4)
LOGFNLKN(&, all_int1, __INT1_T, 4)
LOGFNLKN(&, all_int2, __INT2_T, 4)
LOGFNLKN(&, all_int4, __INT4_T, 4)
LOGFNLKN(&, all_int8, __INT8_T, 4)

LOGFNLKN(&, all_log1, __LOG1_T, 8)
LOGFNLKN(&, all_log2, __LOG2_T, 8)
LOGFNLKN(&, all_log4, __LOG4_T, 8)
LOGFNLKN(&, all_log8, __LOG8_T, 8)
LOGFNLKN(&, all_int1, __INT1_T, 8)
LOGFNLKN(&, all_int2, __INT2_T, 8)
LOGFNLKN(&, all_int4, __INT4_T, 8)
LOGFNLKN(&, all_int8, __INT8_T, 8)

static void (*l_all[4][__NTYPES])() = TYPELIST2LK(l_all_);
static void (*g_all[__NTYPES])() = TYPELIST2(g_all_);

/* dim absent */

void ENTFTN(ALLS, alls)(char *rb, char *mb, F90_Desc *rs, F90_Desc *ms)
{
  red_parm z;

  INIT_RED_PARM(z);
  __fort_red_what = "ALL";

  z.kind = F90_KIND_G(ms);
  z.len = F90_LEN_G(ms);
  z.mask_present = (F90_TAG_G(ms) == __DESC && F90_RANK_G(ms) > 0);
  if (!z.mask_present) {
    z.lk_shift = GET_DIST_SHIFTS(__LOG);
  } else {
    z.lk_shift = GET_DIST_SHIFTS(F90_KIND_G(ms));
  }
  z.l_fn = l_all[z.lk_shift][z.kind];
  z.g_fn = g_all[z.kind];
  z.zb = GET_DIST_TRUES(z.kind);
  I8(__fort_red_scalar)(&z, rb, mb, (char *)GET_DIST_TRUE_LOG_ADDR,
		         rs, ms, (F90_Desc *)&mask_desc, NULL, __ALL);
}

/* dim present */

void ENTFTN(ALL, all)(char *rb, char *mb, char *db, F90_Desc *rs, F90_Desc *ms,
                      F90_Desc *ds)
{
  red_parm z;

  INIT_RED_PARM(z);
  __fort_red_what = "ALL";

  z.kind = F90_KIND_G(ms);
  z.len = F90_LEN_G(ms);
  z.mask_present = (F90_TAG_G(ms) == __DESC && F90_RANK_G(ms) > 0);
  if (!z.mask_present) {
    z.lk_shift = GET_DIST_SHIFTS(__LOG);
  } else {
    z.lk_shift = GET_DIST_SHIFTS(F90_KIND_G(ms));
  }
  z.l_fn = l_all[z.lk_shift][z.kind];
  z.g_fn = g_all[z.kind];
  z.zb = GET_DIST_TRUES(z.kind);
  I8(__fort_red_array)(&z, rb, mb, (char *)GET_DIST_TRUE_LOG_ADDR, db,
		        rs, ms, (F90_Desc *)&mask_desc, ds, __ALL);
}

/* global ALL accumulation */

void ENTFTN(REDUCE_ALL, reduce_all)(char *hb, __INT_T *dimsb, __INT_T *nargb,
                                    char *rb, F90_Desc *hd, F90_Desc *dimsd,
                                    F90_Desc *nargd, F90_Desc *rd)

{
#if defined(DEBUG)
  if (dimsd == NULL || F90_TAG_G(dimsd) != __INT)
    __fort_abort("GLOBAL_ALL: invalid dims descriptor");
  if (nargd == NULL || F90_TAG_G(nargd) != __INT)
    __fort_abort("REDUCE_ALL: invalid arg count descriptor");
  if (*nargb != 1)
    __fort_abort("REDUCE_ALL: arg count not 1");
#endif
  I8(__fort_global_reduce)(rb, hb, *dimsb, rd, hd, "ALL", g_all);
}

void ENTFTN(GLOBAL_ALL, global_all)(char *rb, char *hb, __INT_T *dimsb,
                                    F90_Desc *rd, F90_Desc *hd, F90_Desc *dimsd)
{
  I8(__fort_global_reduce)(rb, hb, *dimsb, rd, hd, "ALL", g_all);
}
