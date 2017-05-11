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
 * \brief Default fperror() stub
 *
 *  Default fperror() routine invoked by the scutil compile-time
 *  evaluation routines.  It resides here in its own source file
 *  so that it can be overridden, which all compiler clients do.
 */

#include "legacy-folding-api.h"
#include <stdio.h>

void
fperror(int fpe)
{
  switch (fpe) {
  case FPE_NOERR:
    break;
  case FPE_INVOP:
    fprintf(stderr, "illegal input or NaN error\n");
    break;
  case FPE_FPOVF: /* == FPE_IOVF, FPE_DIVZ */
    fprintf(stderr, "overflow error\n");
    break;
  case FPE_FPUNF:
    fprintf(stderr, "underflow error\n");
    break;
  default:
    fprintf(stderr, "unknown floating-point error (%d)\n", fpe);
  }
}
