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
    \brief Utilities for processing the data init file.
 */

#include "gbldefs.h"
#include "global.h"
#include "error.h"
#include "symtab.h"
#include "dtypeutl.h"
#include "semant.h"
#include "dinit.h"

static char mode;
static FILE *df = NULL;
#if DEBUG
static void dump_buff(char *);
#endif
static DREC t;

void
dinit_init(void)
{
  mode = 'n'; /* nonexistent file */
} /* dinit_init */

void
dinit_put(int dtype, INT conval)
{
  int n;

  if (mode != 'w') { /* create a new file */
    if ((df = tmpf("b")) == NULL)
      errfatal(5);
    mode = 'w';
#if DEBUG
    if (DBGBIT(6, 1))
      fprintf(gbl.dbgfil, "create dinit file\n");
#endif
  }

  t.dtype = dtype;
  t.conval = conval;
#if DEBUG
  if (DBGBIT(6, 1))
    dump_buff("put");
#endif

  n = fwrite((char *)&t, sizeof(t), 1, df);
  if (n != 1)
    error(10, 4, 0, "(data init file)", CNULL);
} /* dinit_put */

DREC *
dinit_read(void)
{
  int n;

  if (mode == 'n' || mode == 'e')
    return NULL;
  if (mode == 'w') { /* rewind before reading */
    n = fseek(df, 0L, 0);
    if (n == -1)
      perror("dinit_read - fseek error");
    assert(n == 0, "dinit_read:bad rewind", n, 4);
    mode = 'r';
#if DEBUG
    if (DBGBIT(6, 1))
      fprintf(gbl.dbgfil, "rewind dinit file to read\n");
#endif
  }

  n = fread((char *)&t, sizeof(t), 1, df);
  if (n == 0) { /* end of file */
    mode = 'e';
    return NULL;
  }
#if DEBUG
  if (DBGBIT(6, 1))
    dump_buff("get");
#endif

  return &t;
}

#if DEBUG
static void
dump_buff(char *str)
{
  char buf[32];

  fprintf(gbl.dbgfil, " dinit-%s ", str);
  if (t.dtype > 0) {
    getdtype(t.dtype, buf);
    fprintf(gbl.dbgfil, "dtype: %s  conval: %d\n", buf, t.conval);
  } else if (t.dtype == 0) {
  } else {
    switch (t.dtype) {
    case DINIT_LOC:
      fprintf(gbl.dbgfil, "DINIT_LOC  %s  ", getprint((int)t.conval));
      break;
    case DINIT_FMT:
      fprintf(gbl.dbgfil, "DINIT_FMT  %s  ", getprint((int)t.conval));
      break;
    case DINIT_NML:
      fprintf(gbl.dbgfil, "DINIT_NML  %s  ", getprint((int)t.conval));
      break;
    case DINIT_END:
      fprintf(gbl.dbgfil, "DINIT_END");
      break;
    case DINIT_REPEAT:
      fprintf(gbl.dbgfil, "DINIT_REPEAT   ");
      break;
    case DINIT_OFFSET:
      fprintf(gbl.dbgfil, "DINIT_OFFSET   ");
      break;
    case DINIT_LABEL:
      fprintf(gbl.dbgfil, "DINIT_LABEL %s ", getprint((int)t.conval));
      break;
    case DINIT_ZEROES:
      fprintf(gbl.dbgfil, "DINIT_ZEROES   ");
      break;
    case DINIT_STR:
      fprintf(gbl.dbgfil, "DINIT_STR  %s  ", getprint((int)t.conval));
      break;
    default:
      fprintf(gbl.dbgfil, "dtype: %4d    ", t.dtype);
      break;
    }
    fprintf(gbl.dbgfil, "   conval: %10d  (0x%08X)\n", t.conval, t.conval);
  }
} /* dump_buff */
#endif

long
dinit_ftell(void)
{
  if (df == NULL)
    return -1;
  return (ftell(df));
}

void
dinit_fseek(long off)
{
  int n;

  if (mode == 'n')
    return;
  mode = 'r';
  n = fseek(df, off, 0);
  if (n == -1)
    perror("dinit_fseek - fseek error");
  assert(n == 0, "dinit_fseek:bad rewind", n, 4);
#if DEBUG
  if (DBGBIT(6, 1))
    fprintf(gbl.dbgfil, "seek to beginning of file\n");
#endif
}

void
dinit_fseek_end(void)
{
  int n;
  if (mode == 'n')
    return;
  mode = 'w';
  n = fseek(df, 0, 2);
  if (n == -1)
    perror("dinit_fseek_end - fseek error");
  assert(n == 0, "dinit_fseek_end:bad seek-end", n, 4);
#if DEBUG
  if (DBGBIT(6, 1))
    fprintf(gbl.dbgfil, "seek to end of file\n");
#endif
} /* dinit_fseek_end  */

void
dinit_end(void)
{
  if (df) {
    fclose(df);
    df = NULL;
    mode = 'n'; /* nonexistent file */
#if DEBUG
    if (DBGBIT(6, 1))
      fprintf(gbl.dbgfil, "close dinit file\n");
#endif
  }
}

void
dinit_newfile(FILE *newdf)
{
  dinit_end();
  df = newdf;
  mode = 'w';
} /* dinit_newfile */

static char savemode;
static long savepos;

/** \brief Save and restore position and mode of the dinit file
 */
void
dinit_save(void)
{
  savemode = mode;
  savepos = 0;
  if (df) {
    savepos = ftell(df);
  }
} /* dinit_save */

void
dinit_restore(void)
{
  mode = savemode;
  if (df) {
    fseek(df, savepos, 0);
  }
} /* dinit_restore */
