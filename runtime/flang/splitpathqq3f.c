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

/*	splitpathqq3f.c - Implements DFLIB splitpathqq subprogram.  */

#include <string.h>
#include <stdlib.h>
/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"

#if defined(_WIN32)

extern char *__fstr2cstr();
int ENT3F(SPLITPATHQQ, splitpathqq)(DCHAR(fpath), DCHAR(fdrive), DCHAR(fdir),
                                    DCHAR(fname),
                                    DCHAR(fext) DCLEN(fpath) DCLEN(fdrive)
                                        DCLEN(fdir) DCLEN(fname) DCLEN(fext))
{
  char *path, *ext, *name, *dir, *drive;
  errno_t err;
  int rslt = 0;

  path = __fstr2cstr(CADR(fpath), CLEN(fpath));
  drive = __fstr2cstr(CADR(fdrive), CLEN(fdrive));
  dir = __fstr2cstr(CADR(fdir), CLEN(fdir));
  name = __fstr2cstr(CADR(fname), CLEN(fname));
  ext = __fstr2cstr(CADR(fext), CLEN(fext));

  if (!path || !drive || !dir || !name || !ext) {
    __io_errno();
    goto rtn;
  }

  /* errno_t _splitpath_s(
     const char * path,
     char * drive,
     size_t driveSizeInCharacters,
     char * dir,
     size_t dirSizeInCharacters,
     char * fname,
     size_t nameSizeInCharacters,
     char * ext,
     size_t extSizeInBytes
     );
  */

  err = _splitpath_s(path, drive, CLEN(fdrive), dir, CLEN(fdir), name,
                     CLEN(fname), ext, CLEN(fext));
  if (err) {
    __io_errno();
    goto rtn;
  }

  rslt = strlen(dir);
  __fcp_cstr(CADR(fext), CLEN(fext), ext);
  __fcp_cstr(CADR(fdrive), CLEN(fdrive), drive);
  __fcp_cstr(CADR(fdir), CLEN(fdir), dir);
  __fcp_cstr(CADR(fname), CLEN(fname), name);

rtn:
  __cstr_free(path);
  __cstr_free(drive);
  __cstr_free(dir);
  __cstr_free(name);
  __cstr_free(ext);

  return rslt;
}
#else
int ENT3F(SPLITPATHQQ, splitpathqq)(DCHAR(fpath), DCHAR(fdrive), DCHAR(fdir),
                                    DCHAR(fname),
                                    DCHAR(fext) DCLEN(fpath) DCLEN(fdrive)
                                        DCLEN(fdir) DCLEN(fname) DCLEN(fext))
{
  fprintf(__io_stderr(), "splitpathqq() not implemented on this target\n");
  return 0;
}

#endif
