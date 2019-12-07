/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
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
