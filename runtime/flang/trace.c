/*
 * Copyright (c) 1995-2018, NVIDIA CORPORATION.  All rights reserved.
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

char *__fort_getopt(char *opt);
long __fort_strtol(char *str, char **ptr, int base); /* atol.c */

static int tracing = 0;
static int call_level = 0;

/** \brief Initialization */
int
__fort_trac_init(void)
{
  char *p, *q;
  int n;

  p = __fort_getopt("-trace");
  if (p) {
    n = (int)__fort_strtol(p, &q, 0);
    if (q == p)
      tracing = 1;
    else if (*q != '\0' || n < 0 || n >= GET_DIST_TCPUS)
      __fort_abort("invalid -trace processor");
    else
      tracing = (n == GET_DIST_LCPU);
  }
  return 0;
}

/* function entry  */

static char dots[] = "....................";

/* FIXME: still used? */
void
__fort_trac_function_entry(int line, int lines, int cline, char *func,
                           char *file, __CLEN_T funcl, __CLEN_T filel)
{
  ++call_level;
  if (tracing)
    printf("%d: %.*s %.*s (%.*s:%d..%d) called from line %d\n", GET_DIST_LCPU,
           call_level, dots, funcl, func, filel, file, line, line + lines - 1,
           cline);
}

/* FIXME: still used? */
/** \brief line entry
 * \param line - current line number
 */
void
__fort_trac_line_entry(int line)
{
}

/** \brief Update start receive message stats
 * \param cpu: sending cpu
 * \param len: total length in bytes
 */
void
__fort_trac_recv(int cpu, long len)
{
}

/** \brief Update done receive message stats
 * \param cpu - sending cpu
 */
void
__fort_trac_recv_done(int cpu)
{
}

/** \brief Update start send message stats
 * \param cpu: sending cpu
 * \param len: total length in bytes
 */
void
__fort_trac_send(int cpu, long len)
{
}

/** \brief Update done send message stats
 * \param cpu - receiving cpu
 */
void
__fort_trac_send_done(int cpu)
{
}

/** \brief Update start bcopy message stats
 * \param len: total length in bytes
 */
void
__fort_trac_copy(long len)
{
}

/** \brief Update done bcopy message stats */
void
__fort_trac_copy_done(void)
{
}

/* FIXME: still used */
/** \brief Function exit  */
void
__fort_trac_function_exit(void)
{
  --call_level;
}

/** \brief Termination */
void
__fort_trac_term(void)
{
}
