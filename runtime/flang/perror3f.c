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

/* clang-format off */

/*	perror3f.c - Implements LIB3F perror subprogram.  */

/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"

#if !defined(WIN64) && !defined(WIN32)
extern char *strerror(); /* SVR4 only ? */
#endif
extern FILE *__getfile3f();

void ENT3F(PERROR, perror)(DCHAR(str) DCLEN(str))
{
  FILE *fp;
  char *p;
  char *str = CADR(str);
  int str_l = CLEN(str);

  p = strerror(__io_errno());
  fp = __getfile3f(0);
  if (str_l > 0) {
    do {
      fputc((int)*str, fp);
      str++;
    } while (--str_l > 0);
    fputc(':', fp);
    fputc(' ', fp);
  }
  fprintf(fp, "%s", p);
  if (__PC_DOS)
    fputc('\r', fp);
  fputc('\n', fp);

  return;
}
