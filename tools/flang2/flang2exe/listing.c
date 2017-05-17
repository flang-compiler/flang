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

/* listing.c - Fortran source listing file module.

    contents:
      list_init(fd)
      list_line(txt)
      list_page()
*/

#include "gbldefs.h"
#include "global.h"
#include "version.h"

static int pgpos = 1; /* line position within page of next line */
static FILE *lf;      /* listing file descriptor */
static int pgno;      /* page number of next page */

#define LPP 60

extern void list_line();
static void list_ln();

/*******************************************************************/

void list_init(fd) FILE *fd;
{
  register char **p;
  char buf[80], buf1[40], buf2[20], buf3[20];
  static char *sevtxt[4] = {"inform", "warn", "severe", "fatal"};

  lf = fd;
  pgno = 1;

  /*  WARNING:  watch for overflowing buf  */
  if (!DBGBIT(14, 3)) {

    /* ... put out filename line. */
    list_ln("\nFilename: ", gbl.src_file);
  }

  list_line("");

}

/*******************************************************************/

void list_line(txt) char *txt;
{
  list_ln("", txt);
}

static void list_ln(beg, txt) char *beg, *txt;
{
  if (pgpos == 1 && !DBGBIT(14, 3)) {
    if (!lf)
      return; /* in case error msg written before file
               * opened */
    fprintf(lf, "\n\n\n%s(Version %8s)          %s      page %d\n\n",
            version.product, version.vsn, gbl.datetime, pgno);
    pgno++;
    pgpos = 6;
  }

  fprintf(lf, "%s%s\n", beg, txt);
#if DEBUG
  if (DBGBIT(0, 4))
    fprintf(gbl.dbgfil, "%s%s\n", beg, txt);
#endif
  pgpos++;

  if (pgpos == LPP + 4 && !DBGBIT(14, 3)) {
    fprintf(lf, "\n\n\n");
    pgpos = 1;
  }

}

/*******************************************************************/

void
list_page()
{

  if (lf)
    if (!(DBGBIT(14, 3) || DBGBIT(0, 32)))
      while (pgpos != 1)
        list_line("");

}
