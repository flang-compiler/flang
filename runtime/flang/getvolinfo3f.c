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

#if defined(_WIN32)
#if defined(_WIN32)
typedef long long LDWORD;
extern int GetVolumeInformationA();
#define ENTNAM(ss) _##ss

#else
typedef long LDWORD;
extern int __stdcall GetVolumeInformationA();
#define ENTNAM(ss) ss
#endif

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
#endif

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
