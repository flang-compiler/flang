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

extern struct cgrp *__fort_genlist();
extern struct chdr *__fort_allchn();

/* compute the recv/send order for a binary broadcast */

void __fort_bcstchn(struct chdr *c,
                   int scpu,  /* sending cpu */
                   int ncpus,/* number of receiving cpus */
                   int *cpus)
{
  int lcpu;
  int n;
  char buf[80];

  lcpu = __fort_myprocnum();

  if (lcpu != scpu) {
    c->cp[c->cn].op = CPU_RECV;
    c->cp[c->cn].cpu = scpu;
    c->cp[c->cn].rp = &(c->rp[0]);
    c->cn++;
    return;
  }

  for (n = 0; n < ncpus; n++) {
    c->cp[c->cn].op = CPU_SEND;
    c->cp[c->cn].cpu = cpus[n];
    c->cp[c->cn].sp = &(c->sp[0]);
    c->cn++;
  }
}
