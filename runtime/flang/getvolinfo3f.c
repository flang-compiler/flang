/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

/*
 *	getvolinfo3f.c - DFWIN GetVolumeInformation subprogram
 *	This is just the interface to the actual system routine from
 *	the dwfin module routine.  This routine is called as an 'alias'
 *	when the module is called; this routine makes sure that the
 *	output character arguments are blank-filled.
 */

#include <string.h>

typedef char *LPSTR;
typedef int DWORD;

#if defined(_WIN64)
typedef long long LDWORD;
extern int GetVolumeInformationA();
#define ENTNAM(ss) _##ss

typedef LDWORD *LPDWORD;

static void fill(LPSTR, int, int);

extern int ENTNAM(pgdfw_GetVolumeInformation)(
    LPSTR RootPathName, LPSTR VolumeNameBuffer, DWORD VolumeNameSize,
    LPDWORD VolumeSerialNumber, LPDWORD MaximumComponentLength,
    LPDWORD FileSystemFlags, LPSTR FileSystemNameBuffer,
    DWORD FileSystemNameSize)
{
  int s;
  int zz;

  s = GetVolumeInformationA(RootPathName, VolumeNameBuffer, VolumeNameSize,
                            VolumeSerialNumber, MaximumComponentLength,
                            FileSystemFlags, FileSystemNameBuffer,
                            FileSystemNameSize);
  fill(VolumeNameBuffer, strlen(VolumeNameBuffer), VolumeNameSize);
  fill(FileSystemNameBuffer, strlen(FileSystemNameBuffer), FileSystemNameSize);
  return s;
}

static void
fill(LPSTR p, int ln, int sz)
{
  if (sz > ln) {
    int n;
    n = sz - ln;
    memset(p + ln, ' ', n);
  }
  return;
}
#endif
