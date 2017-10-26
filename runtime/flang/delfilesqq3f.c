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

/*	delfilesqq3f.c - Implements DFLIB delfilesqq subprogram.  */
#if defined(WIN64) || defined(WIN32)
#include <windows.h>
#endif
#include <string.h>
#include <stdlib.h>
/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"

#if defined(WIN64) || defined(WIN32)
extern char *__fstr2cstr();
int ENT3F(DELFILESQQ, delfilesqq)(DCHAR(ffiles) DCLEN(ffiles))
{
  char *files;
  int rslt = 0, i;
  WIN32_FIND_DATA FindFileData;
  HANDLE hFind;

  files = __fstr2cstr(CADR(ffiles), CLEN(ffiles));
  if (!files) {
    __io_errno();
    return 0;
  }
  hFind = FindFirstFile(files, &FindFileData);
  do {
    if (hFind == INVALID_HANDLE_VALUE)
      break;
    if (FindFileData.dwFileAttributes &
        (FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_HIDDEN |
         FILE_ATTRIBUTE_SYSTEM | FILE_ATTRIBUTE_READONLY)) {
      continue;
    }
    if (_unlink(FindFileData.cFileName) != -1) {
      ++rslt;
    }

  } while (FindNextFile(hFind, &FindFileData) != 0);
  FindClose(hFind);
  __cstr_free(files);
  return rslt;
}
#else
int ENT3F(DELFILESQQ, delfilesqq)(DCHAR(ffiles) DCLEN(ffiles))
{
  fprintf(__io_stderr(), "delfilesqq() not implemented on this target\n");
  return 0;
}

#endif
