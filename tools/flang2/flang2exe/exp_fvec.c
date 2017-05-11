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

#include "gbldefs.h"
#include "error.h"
#include "global.h"
#include "symtab.h"
#include "regutil.h"
#include "machreg.h"
#include "ilm.h"
#include "ilmtp.h"
#include "ili.h"
#include "expand.h"
#include "machar.h"

extern void eval_ilm();

void
init_fvec()
{
}
void
fin_fvec()
{
}

void eval_fvec(ilmx) int ilmx;
{
  int opc;

  opc = ILM_OPC((ILM *)(ilmb.ilm_base + ilmx));
  if (IM_VEC(opc)) {
    interr("eval_fvec: vector not impl", ilmx, 3);
  } else {
    eval_ilm(ilmx);
  }
}
