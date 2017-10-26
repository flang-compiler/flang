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

/*	fullpathqq3f.c - Implements DFLIB fullpathqq subprogram.  */

#include <string.h>
#include <stdlib.h>
/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"

#if defined(WIN64) || defined(WIN32)

extern char *__fstr2cstr();
int ENT3F(FULLPATHQQ, fullpathqq)(DCHAR(fname),
                                  DCHAR(fpath) DCLEN(fname) DCLEN(fpath))
{
  char *path, *name;
  int rslt = 0;

  path = __fstr2cstr(CADR(fpath), CLEN(fpath));
  name = __fstr2cstr(CADR(fname), CLEN(fname));

  if (!path || !name) {
    __io_errno();
    goto rtn;
  }

  /*
    char *_fullpath(
    char *absPath,
    const char *relPath,
    size_t maxLength
    );
  */

  if (_fullpath(path, name, CLEN(fpath)) != NULL) {
    rslt = strlen(path);
    __fcp_cstr(CADR(fpath), CLEN(fpath), path);
  }

rtn:
  __cstr_free(path);
  __cstr_free(name);

  return rslt;
}
#else
int ENT3F(FULLPATHQQ, fullpathqq)(DCHAR(fname),
                                  DCHAR(fpath) DCLEN(fname) DCLEN(fpath))
{
  fprintf(__io_stderr(), "fullpathqq() not implemented on this target\n");
  return 0;
}

#endif
