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

/** \file
 * \brief Stubs
 */

#include "stdioInterf.h"
#include "fioMacros.h"

static int tid = 0;

char *__fort_transnam = "rpm1";

#ifdef WINNT

/* pg access routines for data shared between windows dlls */

char *
__get_fort_transnam(void)
{
  return __fort_transnam;
}

#endif

/** \brief Abort the whole mess */
void
__fort_abortx(void)
{
  __fort_traceback();
  __abort(1, NULL);
}

/** \brief End of parallel program */
void
__fort_endpar(void)
{
}

/** \brief Begin parallel program */
void
__fort_begpar(int ncpus)
{
  SET_DIST_TCPUS(1);
  SET_DIST_LCPU(0);
  SET_DIST_TIDS(&tid);

  __fort_procargs();
  if (GET_DIST_TCPUS != 1) {
    fprintf(__io_stderr(),
            "0: RPM1 uses only 1 processor, using 1 processor\n");
    SET_DIST_TCPUS(1);
  }
  __fort_sethand();
}

void
__fort_barrier()
{
}

void ENTFTN(BARRIER, barrier)() {}

__INT_T
ENTFTN(TID, tid)(__INT_T *lcpu) { return (*lcpu); }
