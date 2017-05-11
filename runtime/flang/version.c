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
 */

#include "stdioInterf.h"
#include "fioMacros.h"
#include "release.h"

#define LIBRARY "LIBPGF90"
#define TRANSNAM ""

#ifndef VHOST
#define VHOST "Any"
#endif

static struct {
  char *lib;  /* library */
  char *host; /* host */
  char *vsn;  /* version number */
  char *bld;  /* build number */
  char *dvsn; /* date-based version number */
  char *copyright;
} version = {
    LIBRARY, VHOST,
    VSN,     BLD,
    DVSN,    ""};

/* print version info to stderr */

void
__fort_print_version()
{
  fprintf(__io_stderr(), "%s %s %s %s%s\n", version.lib, version.host,
          TRANSNAM, version.vsn, version.bld);
  fprintf(__io_stderr(), "%s\n", version.copyright);
}
