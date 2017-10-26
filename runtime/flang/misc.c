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

#if !defined(PARAMID) && !defined(WINNT)
#include <sys/types.h>
#include <fcntl.h>
#include <time.h>
#endif
#include <errno.h>
#include "global.h"

__INT_T
__fort_time(void)
{
  __INT_T s;

#if defined(SUN4SOL2) || defined(SOL86) || defined(HP) || defined(TARGET_OSX)
  s = (int)time((time_t *)0);
#else
  s = time(NULL);
#endif
  if (!LOCAL_MODE) {
    __fort_rbcst(GET_DIST_IOPROC, &s, 1, 1, __CINT);
  }
  return s;
}
