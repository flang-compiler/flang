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

#include "fioMacros.h"

/* init structures used to find cpu indexes */

void __fort_initndx( int nd, /* number of dimensions */
                    int *cnts, /* cpu counts by dimension */
                    int *ncnts, /* cpu counts by dimension sorted by
                                 * stride (output) */
                    int *strs,  /* cpu strides by dimension */
                    int *nstrs, /* cpu striedes by dimension sorted by
                                 * stride (output) */
                    int *mults) /* multiplers by dimension sorted by
                                 * stride (output) */
{
  int n;
  int i;

  n = 1;
  for (i = 0; i < nd; i++) {
    mults[i] = n;
    n *= cnts[i];
    nstrs[i] = strs[i];
    ncnts[i] = cnts[i];
  }
  i = 0;
  while (i < (nd - 1)) {
    if (nstrs[i] > nstrs[i + 1]) {
      n = nstrs[i];
      nstrs[i] = nstrs[i + 1];
      nstrs[i + 1] = n;
      n = ncnts[i];
      ncnts[i] = ncnts[i + 1];
      ncnts[i + 1] = n;
      n = mults[i];
      mults[i] = mults[i + 1];
      mults[i + 1] = n;
      if (i > 0) {
        i--;
        continue;
      }
    }
    i++;
  }
}

/* find a cpu's index */

int __fort_findndx( int cpu, /* cpu whose index is wanted */
                   int nd, /* number of dimensions */
                   int low,/* lowest numbered cpu */
                   int *nstrs, /* strides by dimension sorted by stride */
                   int *mults) /* multipliers by dimension sorted by stride */
{
  int n;
  int i;
  int ndx;

  /* convert cpu number to index into cpu sequence */

  cpu = cpu - low;
  ndx = 0;
  for (i = (nd - 1); i >= 0; i--) {
    n = cpu / nstrs[i];
    cpu = cpu - n * nstrs[i];
    ndx += n * mults[i];
  }

  return (ndx);
}

/* generate list of cpu numbers */

struct cgrp *__fort_genlist(nd, low, cnts,
                           strs) int nd; /* number of dimensions */
int low;                                 /* lowest cpu number */
int cnts[];                              /* counts per dimension */
int strs[];                              /* strides per dimension */
{

  int dim;
  int idxs[MAXDIMS];
  struct cgrp *g;
  int cpu;
  int n;

  n = 1;
  for (dim = 0; dim < nd; dim++) {
    idxs[dim] = 1; /* reset indices */
    n *= cnts[dim];
  }
  g = (struct cgrp *)__fort_malloc(sizeof(struct cgrp) + (n - 1) * sizeof(int));
  cpu = low;
  g->ncpus = 0;
  do {
    g->cpus[g->ncpus++] = cpu;
    for (dim = 0; dim < nd; dim++) {/* increment cpu */
      if (idxs[dim] < cnts[dim]) {
        ++idxs[dim];
        cpu += strs[dim];
        break;
      }
      idxs[dim] = 1;
      cpu -= (cnts[dim] - 1) * strs[dim];
    }
  } while (dim < nd);
  return (g);
}
