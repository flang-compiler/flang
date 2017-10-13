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

/** \file lz.h
    \brief definitions for LZ compression used with IPA
*/

#ifndef LZ
#define LZ

#include <stdarg.h>

#define NOLZ 1

/* handle returned from lzinitfile/ulzinit
 * passed to lz/ulz, lzfini/lzfinifile/ulzfini, lzprintf */
typedef struct {
  FILE *file; /* file to use with lzprintf */
  char *buff; /* buffer to collect line with lzprintf, or uncompressed line */
  int bufflen; /* buffer length, allocated size */
  size_t buffsize;
  long savefile;         /* ftell() result when lz*save called */
  int inout; /* 0 for in, 1 for out */
} lzhandle;

/* lzinitfile/ulzinit compression arguments */
#define LZNOCOMPRESS 0
#define LZCOMPRESS 1
#define LZIPKINFO 2
#define LZIPMINFO 3
#define LZIPOINFO 4
#define LZIPPINFO 5
#define LZIPNINFO 6
#define LZIPXINFO 7
#define LZZLIB 8
#define LZMAX 9

/* read from 'in', return an uncompressed line */
char *ulz(lzhandle *lzh);

/* read one char from 'in' */
char ulzgetc(lzhandle *lzh);

/* call before calling ulz at all */
lzhandle *ulzinit(FILE *in, int compress);

/* call when done calling ulz */
void ulzfini(lzhandle *lzh);

/* encode a single text line, wrote to out */
void lz(lzhandle *lzh, char *line, int linelen);

/* call this to set the output file before calling lzprintf */
lzhandle *lzinitfile(FILE *out, int compress);

/* call this after the last write with lzprintf;
 * lsfinifile calls lzfini */
void lzfinifile(lzhandle *lzh);

/* call this to reset the compression tables between sections */
void lzreinit(lzhandle *);

/* call lzprintf like printf(fmt,arg,arg,...);
 * lzprintf calls lz */
void lzprintf(lzhandle *lzh, const char *fmt, ...);

#if defined(HOST_WIN)
#define vsnprintf _vsnprintf
#endif

/* call lzwritesave to save position in file and state of dictionary */
void lzsave(lzhandle *);

/* call lzwriterestore to restore position in file and state of dictionary */
void lzrestore(lzhandle *);

void lzreset(lzhandle *lzh);
#endif
