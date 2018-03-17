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
#include <string.h>
#include <memory.h>
#ifndef _WIN32
#include <sys/time.h>
#endif

extern char *__fort_getopt();

/* tid comparison routine called by qsort */

static int
compar_tids(const void *a, const void *b)
{
  return *(int *)a - *(int *)b;
}

/* Verify the logical to physical processor map */

static void
__fort_check_map(int *processormap)
{
  int i, j, BadMap, tcpus;

  tcpus = GET_DIST_TCPUS;
  BadMap = 0;
  for (i = 0; i < tcpus; i++) {
    if (processormap[i] >= tcpus) {
      fprintf(__io_stderr(), "Invalid processor %d mapped to processor %d.\n",
              processormap[i], i);
      BadMap = 1;
    } else if (processormap[i] >= 0) {
      for (j = i + 1; j < tcpus; j++) {
        if (processormap[j] == processormap[i]) {
          fprintf(__io_stderr(), "Redundant mapping for processor %d.\n",
                  processormap[i]);
          processormap[j] = -1;
          BadMap = 1;
        }
      }
    }
  }
  if (BadMap)
    __abort(1, NULL);
}

/* get processor map */

static void
__fort_getmap(int *processormap)
{
  int j, k, m, n, tcpus, *usedmap;
  char *argp, *endp;

  /* -map <j>:<m>..<n>,... = map processors <m>..<n> to processors <l>..  */

  tcpus = GET_DIST_TCPUS;
  usedmap = (int *)__fort_malloc(tcpus * sizeof(int));
  for (j = 0; j < tcpus; j++) {
    processormap[j] = -1;
    usedmap[j] = 0;
  }
  argp = __fort_getopt("-map");
  if (argp != NULL) {
    for (j = 0; *argp != '\0';) {
      m = strtol(argp, &endp, 0);
      if (endp == argp)
        break;
      if (*endp == ':') {
        j = m;
        if (j < 0 || *++endp == '\0')
          break;
        argp = endp;
        m = strtol(argp, &endp, 0);
        if (endp == argp)
          break;
      }
      if (j >= tcpus || m < 0)
        break;
      n = m;
      if (*endp == '.' && *(endp + 1) == '.') {
        if (*(endp += 2) == '\0')
          break;
        argp = endp;
        n = strtol(argp, &endp, 0);
        if (endp == argp || n < 0)
          break;
      }
      k = n > m ? 1 : -1;
      for (; j < tcpus; m += k) {
        processormap[j++] = m;
        usedmap[m] = 1;
        if (m == n)
          break;
      }
      if (m != n)
        break;
      argp = endp;
      if (*endp == ',' && *++endp != '\0')
        argp = endp;
      else
        break;
    }
    if (*argp != '\0') {
      fprintf(__io_stderr(), "Bad map argument: %s\n", argp);
      __abort(1, NULL);
    }
  }
  for (j = 0; j < tcpus; j++) {
    if (processormap[j] < 0) {
      for (m = 0; m < tcpus && usedmap[m]; m++)
        ;
      if (m < tcpus) {
        processormap[j] = m;
        usedmap[m] = 1;
      }
    }
  }
  __fort_free(usedmap);

  __fort_check_map(processormap);
}

/* process -map argument
        __fort_tcpus must be set
        __fort_tids must be set (may be changed)
        the logical CPU number may change after this routine
 */

void
__fort_map()
{
  int i;
  int *processormap;
  int *btids;
  int tcpus;

  tcpus = GET_DIST_TCPUS;

  /* get arguments from command line and environment */

  processormap = (int *)__fort_malloc(tcpus * sizeof(int));
  btids = (int *)__fort_malloc(tcpus * sizeof(int));
  __fort_bcopy((char *)btids, (char *)GET_DIST_TIDS, tcpus * sizeof(int));

  __fort_getmap(processormap);

  /* map processors to tids */

  qsort(btids + 1, tcpus - 1, sizeof(int), compar_tids);

  for (i = 0; i < tcpus; i++)
    SET_DIST_TIDS_ELEM(processormap[i], btids[i]);
  __fort_free(btids);
  __fort_free(processormap);
}

void __fort_getarg() /* REMOVE */ { __fort_abort("getarg called"); }
