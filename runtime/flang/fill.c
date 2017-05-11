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

#include "stdioInterf.h"
#include "fioMacros.h"

#include "fort_vars.h"
extern void (*__fort_scalar_copy[__NTYPES])(void *rp, void *sp, int size);

static void I8(fills_loop)(char *ab, F90_Desc *as, void *fb, __INT_T off0,
                           __INT_T dim)
{
  DECL_DIM_PTRS(asd);
  char *ap;

  __INT_T ak, al, an, au, cl, cn, clof, off;

  SET_DIM_PTRS(asd, as, dim - 1);
  ak = F90_DPTR_SSTRIDE_G(asd) * F90_DPTR_LSTRIDE_G(asd);

  cl = DIST_DPTR_CL_G(asd);
  cn = DIST_DPTR_CN_G(asd);
  clof = DIST_DPTR_CLOF_G(asd);

  for (; cn > 0; --cn, cl += DIST_DPTR_CS_G(asd), clof += DIST_DPTR_CLOS_G(asd)) {
    an = I8(__fort_block_bounds)(as, dim, cl, &al, &au);
    off = off0 +
          (F90_DPTR_SSTRIDE_G(asd) * al + F90_DPTR_SOFFSET_G(asd) - clof) *
              F90_DPTR_LSTRIDE_G(asd);
    if (dim > 1) {
      for (; an > 0; --an) {
        I8(fills_loop)(ab, as, fb, off, dim - 1);
        off += ak;
      }
    } else {
      ap = ab + off * F90_LEN_G(as);
      __fort_bcopysl(ap, fb, an, ak, 0, F90_LEN_G(as));
    }
  }
}

/* fill array section with scalar */

void I8(__fort_fills)(char *ab, F90_Desc *as, void *fb)
{
  if (F90_RANK_G(as) == 0) {
    __fort_scalar_copy[F90_KIND_G(as)](ab + DIST_SCOFF_G(as) * F90_LEN_G(as), fb,
                                      F90_LEN_G(as));
  } else if (~F90_FLAGS_G(as) & __OFF_TEMPLATE) {
    I8(__fort_cycle_bounds)(as);
    I8(fills_loop)(ab, as, fb, F90_LBASE_G(as) - 1, F90_RANK_G(as));
  }
}
