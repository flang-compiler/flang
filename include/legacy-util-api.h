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
/** \file
 * \brief Legacy utility interfaces
 *
 *  This header comprises declarations and definitions from the original
 *  scutil/hammer/linux86-64/include/scutil.h header file that don't
 *  pertain to constant representation or the compile-time evaluation
 *  of operations.
 */

#ifndef SCUTIL_UTIL_API_H_
#define SCUTIL_UTIL_API_H_
#ifdef __cplusplus
extern "C" {
#endif

/*
 *  TODO: "include what you use" these headers directly where needed
 *  instead of bundling them all here.
 */

#include <ctype.h>
#include <limits.h> /* PATH_MAX */
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h> /* time() */
#ifndef _WIN32
#include <unistd.h> /* getcwd() */
#endif

/* See tmpfile(3). */
FILE *tmpf(char *ignored);

/* Copy to 'basename' the final path component, less any undesirable suffix. */
void basenam(const char *orig_path, const char *optional_suffix,
             char *basename);

/* Strip off anything after the last '/', but keep the '/' (unlike dirname(1)).
 * Return "./" if there's no '/'.
 */
void dirnam(const char *orig_path, char *dirname);

/* Locate 'file' in a list of directories, like the shell's $PATH.
 * Write the successful pathname to 'path'.  Returns 0 on success, -1 else.
 */
int fndpath(const char *file, char *path, size_t max_length,
            const char *dirs);

/* Directory name separator for fndpath: */
#define DIRSEP		':'
/* Directory names for fndpath:  */
#define	DIRWORK		""
#define	DIRSINCS	"/usr/include"

/* If 'pattern' ends in 'oldext', replace that suffix in place with 'newext'.
 * Always returns 'pattern'.
 */
char *mkperm(char *pattern, const char *oldext, const char *newext);

/* Predefined file name extensions for mkperm():  */
#define	UNKFILE		""		/* No extension. */
#define	LISTFILE	".lst"		/* Listing file. */
#define	OBJFILE		".o"		/* Object module file. */
#define	IMGFILE		".out"		/* Image module file. */
#define	SYMFILE		".dbg"		/* Symbol file. */
#define	OVDFILE		".ovd"		/* Overlay description file. */
#define	OBJLFILE	".a"		/* Object library file. */
#define	IMGLFILE	".ilb"		/* Image library file. */
#define	MAPFILE		".map"		/* Map file from linker. */
#define CFILE		".c"		/* C source */
#define IFILE		".i"		/* C .i file */
#define FTNFILE		".f"		/* Fortran source */
#define ASMFILE		".s"		/* asm source */

/* Measures user+system CPU milliseconds that elapse between calls. */
unsigned long getcpu(void);

#ifdef __cplusplus
}
#endif
#endif /* SCUTIL_UTIL_API_H_ */
