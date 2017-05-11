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

/* allocate 1 to 1 channel structure */

struct chdr *
    __fort_chn_1to1(cp, dnd, dlow, dcnts, dstrs, snd, slow, scnts,
                   sstrs) struct chdr *cp; /* previous structure in list */
int dnd;                                   /* source cpu DImenSIons */
int dlow;                                  /* source lowest numbered cpu */
int *dcnts;                                /* source number of cpus */
int *dstrs;                                /* source cpu stride */
int snd;                                   /* destination cpu DImenSIons */
int slow;                                  /* destination lowest numbered cpu */
int *scnts;                                /* destination number of cpus */
int *sstrs;                                /* destination cpu stride */
{
  int snstrs[MAXDIMS];
  int sncnts[MAXDIMS];
  int smults[MAXDIMS];
  int dnstrs[MAXDIMS];
  int dncnts[MAXDIMS];
  int dmults[MAXDIMS];
  chdr *c; /* channel structure pointer */
  int cpu;
  int lcpu;
  int n;
  int m;
  int nmax;
  int r;
  int di, si, ci;
  int tcpus;
  struct cgrp *sg;
  struct cgrp *dg;

  /* allocate channel */

  __fort_initndx(snd, scnts, sncnts, sstrs, snstrs, smults);
  sg = __fort_genlist(snd, slow, sncnts, snstrs);
  __fort_initndx(dnd, dcnts, dncnts, dstrs, dnstrs, dmults);
  dg = __fort_genlist(dnd, dlow, dncnts, dnstrs);

  c = __fort_allchn(cp, dg->ncpus, sg->ncpus, sg->ncpus + dg->ncpus);

  /* find next higher power than number of cpus */

  nmax = 1;
  tcpus = GET_DIST_TCPUS;
  while (tcpus > nmax) {
    nmax = nmax << 1;
  }

  /* generate actions in a non-dead-locking order */

  ci = 0; /* channel index */

  lcpu = GET_DIST_LCPU;
  for (r = 0; r < nmax; r++) {

    /* next cpu to check */

    cpu = r ^ lcpu;

    /* sending cpu? */

    si = sg->ncpus;
    while (--si >= 0) {
      if (sg->cpus[si] == cpu) {
        break;
      }
    }

    /* receiving cpu? */

    di = dg->ncpus;
    while (--di >= 0) {
      if (dg->cpus[di] == cpu) {
        break;
      }
    }

    /* this cpu both a sender and receiver? */

    if ((si >= 0) && (di >= 0)) {

      /* is it this cpu? */

      if (cpu == lcpu) {
        c->cp[ci].op = CPU_COPY;
        c->cp[ci].rp = &(c->rp[si]);
        c->cp[ci].sp = &(c->sp[di]);
        ci++;
        continue;
      }

      /* generate ops to prevent deadlock */

      if (cpu > lcpu) {
        c->cp[ci].op = CPU_SEND;
        c->cp[ci].cpu = cpu;
        c->cp[ci].sp = &(c->sp[di]);
        ci++;
        c->cp[ci].op = CPU_RECV;
        c->cp[ci].cpu = cpu;
        c->cp[ci].rp = &(c->rp[si]);
        ci++;
      } else {
        c->cp[ci].op = CPU_RECV;
        c->cp[ci].cpu = cpu;
        c->cp[ci].rp = &(c->rp[si]);
        ci++;
        c->cp[ci].op = CPU_SEND;
        c->cp[ci].cpu = cpu;
        c->cp[ci].sp = &(c->sp[di]);
        ci++;
      }
      continue;
    }

    /* sending only cpu? */

    if (si >= 0) {
      c->cp[ci].op = CPU_RECV;
      c->cp[ci].cpu = cpu;
      c->cp[ci].rp = &(c->rp[si]);
      ci++;
      continue;
    }

    /* receiving only cpu? */

    if (di >= 0) {
      c->cp[ci].op = CPU_SEND;
      c->cp[ci].cpu = cpu;
      c->cp[ci].sp = &(c->sp[di]);
      ci++;
      continue;
    }
  }

  c->cn = ci;

  __fort_free(sg);
  __fort_free(dg);
  return (c);
}
