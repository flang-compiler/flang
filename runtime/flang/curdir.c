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

#include <sys/types.h>
#ifndef _WIN32
#include <sys/param.h>
#include <sys/utsname.h>
#elif 0
#include <Winsock2.h>
#endif
#include <stdlib.h>
#include "stdioInterf.h"
#include "fioMacros.h"

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

#if defined(WIN32) || defined(WIN64)
#define getcwd _getcwd
#endif

extern char *getcwd();
extern char *__fort_getopt();

WIN_MSVCRT_IMP char *WIN_CDECL getenv(const char *);

/* fix pathname for "funny" NFS mount points */

void __fort_fixmnt(new, old) char *new;
char *old;
{
  char *q;
  char s[MAXPATHLEN]; /* substitute patterns */
  char *smat;         /* match string */
  char *srep;         /* replace string */
  char *snxt;         /* next pattern */
  int n;

  q = __fort_getopt("-mount"); /* pattern */
  if (q == NULL) {
    q = "/tmp_mnt";
  }
  strcpy(s, q);

  snxt = s;
  while (snxt != NULL) {
    smat = snxt;
    snxt = strchr(snxt, ',');
    if (snxt != NULL) {
      *snxt = '\0';
      snxt++;
    }
    srep = strchr(smat, ':'); /* replace string */
    if (srep == NULL) {
      srep = "";
    } else {
      *srep = '\0';
      srep++;
    }
    n = strlen(smat); /* match string length */
    if (strncmp(old, smat, n) == 0) {
      strcpy(new, srep);
      strcat(new, old + n);
      return;
    }
  }
  strcpy(new, old);
}

/* get current working directory */

void __fort_getdir(curdir) char *curdir;
{
  char path[MAXPATHLEN];
  char *p;

  p = getcwd(path, MAXPATHLEN);
  if (p == NULL) {
    p = getenv("PWD");
    if (p == NULL) {
      __fort_abort("cannot find current directory\n");
    }
    strcpy(path, p);
  }
  __fort_fixmnt(curdir, path);
}

/* get current hostname */

void __fort_gethostname(host) char *host;
{
  char *p;
  int s;
#ifndef _WIN32
  struct utsname un;

  p = __fort_getopt("-curhost");
  if (p == NULL) {
    s = uname(&un); /* get hostname */
    if (s == -1) {
      __fort_abortp("uname");
    }
    p = un.nodename;
  }
#elif 0 
  s = gethostname(&p, 256);
  if (s != 0) {
     __fort_abortp("uname");
  }
#else
  strcpy(p, "localhost");
#endif
  strcpy(host, p);
}
