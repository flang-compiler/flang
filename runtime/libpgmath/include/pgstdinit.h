/*
 * Copyright (c) 1998-2018, NVIDIA CORPORATION.  All rights reserved.
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

#if !defined(__PGSTDINIT_H__)
#define __PGSTDINIT_H__

#include <stdio.h>

#ifdef WIN64
#ifdef __PGI
typedef unsigned long long size_t;
#endif
#else
#if !defined(_SIZE_T) && !defined(_SIZE_T_) && !defined(_SIZE_T__) &&          !defined(_SIZE_T_DEFINED) && !defined(__SIZE_T__)
#undef _SIZE_T
#undef _SIZE_T_
#undef _SIZE_T__
#undef _SIZE_T_DEFINED
#undef __SIZE_T__
#define _SIZE_T
#define _SIZE_T_
#define _SIZE_T__
#define _SIZE_T_DEFINED
#define __SIZE_T__
#if defined(LINUX8664) || defined(TARGET_OSX_X8664) || defined(TARGET_LLVM_64)
typedef unsigned long size_t;
#else
typedef unsigned size_t;
#endif
#endif
#endif

/* declarations in runtime must match declarations in MS msvcrt.dll
 * to achieve consistent DLL linkage.
 */
#if defined(_DLL) && (defined(TARGET_WIN) || defined(WIN64) || defined(WIN32))
#define environ _environ
#define WIN_CDECL __cdecl
#define WIN_MSVCRT_IMP extern __declspec(dllimport)
#else
#define WIN_CDECL
#define WIN_MSVCRT_IMP extern
#endif

#ifndef NULL
#define NULL 0
#endif

#ifndef EOF
#define EOF -1
#endif

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

/* defines to use real host stdio routines */

#define __io_fclose(fp) fclose(fp)
#define __io_fflush(fp) fflush(fp)
#ifndef TARGET_WIN
#define __io_fgetc(fp) fgetc(fp)
#endif
#define __io_fgets(ptr, n, fp) fgets(ptr, n, fp)

#if defined(LINUX) && defined(PGLF64) /* LINUX 64-bit filesystems */
#define __io_fopen(file, typ) fopen64(file, typ)
#else
#define __io_fopen(file, typ) fopen(file, typ)
#endif

#define __io_fprintf fprintf
#ifndef TARGET_WIN_X8664
#define __io_fputc(c, fp) fputc(c, fp)
#endif
#define __io_fputs(ptr, fp) fputs(ptr, fp)
#define __io_fread(ptr, size, nitems, fp) fread(ptr, size, nitems, fp)
#define __io_freopen(file, typ, fp) freopen(file, typ, fp)
#define __io_fscanf fscanf

#if defined(SGI) /* SGI */
#include <unistd.h>
typedef off64_t seekoff_t;
#define __io_fseek(fp, off, wh) fseek64(fp, off, wh)
#define __io_ftell(fp) ftell64(fp)
typedef off64_t seekoff64_t;
#define __io_fseek64(fp, off, wh) fseek64(fp, off, wh)
#define __io_ftell64(fp) ftell64(fp)
typedef off64_t seekoffx_t;
#define __io_fseekx(fp, off, wh) fseek64(fp, off, wh)
#define __io_ftellx(fp) ftell64(fp)

#elif defined(LINUX) && defined(PGLF64) /* LINUX 64-bit filesystems */
typedef long seekoff_t;
#define __io_fseek(fp, off, wh) fseek(fp, off, wh)
#define __io_ftell(fp) ftell(fp)
typedef long long seekoff64_t;
#define __io_fseek64(fp, off, wh) fseeko64(fp, off, wh)
#define __io_ftell64(fp) ftello64(fp)
typedef long long seekoffx_t;
#define __io_fseekx(fp, off, wh) fseeko64(fp, off, wh)
#define __io_ftellx(fp) ftello64(fp)

#elif defined(TARGET_WIN)
typedef long seekoff_t;
#define __io_fseek(fp, off, wh) fseek(fp, off, wh)
#define __io_ftell(fp) ftell(fp)
typedef long long seekoff64_t;
#define __io_fseek64(fp, off, wh) _fseeki64(fp, off, wh)
#define __io_ftell64(fp) _ftelli64(fp)
typedef long long seekoffx_t;
#define __io_fseekx(fp, off, wh) _fseeki64(fp, off, wh)
#define __io_ftellx(fp) _ftelli64(fp)

#elif defined(TARGET_OSX)
#include <unistd.h>
typedef long seekoff_t;
#define __io_fseek(fp, off, wh) fseek(fp, off, wh)
#define __io_ftell(fp) ftell(fp)
typedef long long seekoff64_t;
#define __io_fseek64(fp, off, wh) fseek(fp, (long)off, wh)
#define __io_ftell64(fp) (long long) ftell(fp)
typedef long seekoffx_t;
#define __io_fseekx(fp, off, wh) fseek(fp, off, wh)
#define __io_ftellx(fp) ftell(fp)

#else /* the rest */
typedef long seekoff_t;
#define __io_fseek(fp, off, wh) fseek(fp, off, wh)
#define __io_ftell(fp) ftell(fp)
typedef long long seekoff64_t;
#define __io_fseek64(fp, off, wh) fseek(fp, (long)off, wh)
#define __io_ftell64(fp) (long long) ftell(fp)
typedef long seekoffx_t;
#define __io_fseekx(fp, off, wh) fseek(fp, off, wh)
#define __io_ftellx(fp) ftell(fp)
#endif

#define __io_gets(ptr) gets(ptr)
#define __io_perror(ptr) perror(ptr)
#define __io_printf printf
#define __io_puts(ptr) puts(ptr)
#define __io_remove(ptr) remove(ptr)
#define __io_rename(ptr1, ptr2) rename(ptr1, ptr2)
#define __io_rewind(fp) rewind(fp)
#define __io_scanf scanf
#define __io_setbuf(fp, ptr) setbuf(fp, ptr)
#define __io_setvbuf(fp, ptr, typ, size) setvbuf(fp, ptr, typ, size)
#define __io_sprintf sprintf
#define __io_sscanf sscanf
#define __io_tmpfile() tmpfile()
#define __io_tmpnam(ptr) tmpnam(ptr)
#ifndef TARGET_WIN
#define __io_ungetc(c, fp) ungetc(c, fp)
#endif

/* some conversions */

#if defined(C90)
extern long double strtold();
extern char *_Lecvt();
#define __io_strtod(p, ep) strtold(p, ep)
#define __io_ecvt(v, n, d, s, r) _Lecvt(v, n, d, s)
#else
#define __io_strtod(p, ep) __fortio_strtod(p, ep)
#define __io_ecvt(v, n, d, s, r) __fortio_ecvt(v, n, d, s, r)
#endif
#define __io_fcvt(v, n, sf, d, s, r) __fortio_fcvt(v, n, sf, d, s, r)

/* and defines for other routines */

#define __io_truncate(name, len) truncate(name, len)
#define __io_ftruncate(fd, len) ftruncate(fd, len)
#define __io_access(path, mode) access(path, mode)
#define __io_unlink(path) unlink(path)
#define __io_getenv(name) getenv(name)
#define __io_abort() exit(1)

/* finally the prototypes */

#ifdef __STDC__

int __io_errno(void);
void __io_set_errno(int);
FILE *__io_stdin(void);
FILE *__io_stdout(void);
FILE *__io_stderr(void);
int __io_getc(FILE *);
#ifndef TARGET_WIN
int __io_putc(int x, FILE *);
#endif
int __io_getchar(void);
int __io_putchar(int x);
void __io_clearerr(FILE *);
int __io_getfd(FILE *);
int __io_isatty(int fd);
int __io_binary_mode(FILE *);
int __io_setmode_binary(FILE *);
int __io_ispipe(FILE *);
int __io_feof(FILE *);
int __io_ferror(FILE *);
size_t __io_fwrite(const void *, size_t, size_t, FILE *);
int __io_timezone(void *);
int fclose(FILE *);
int fflush(FILE *);
#if defined(TARGET_WIN) || defined(WIN32)
int __io_fgetc(FILE *);
#else
int fgetc(FILE *);
#endif
char *fgets(char *, int, FILE *);
#if defined(LINUX) && defined(PGLF64)
FILE *fopen64(const char *, const char *);
#endif
FILE *fopen(const char *, const char *);
int fprintf(FILE *, const char *, ...);
#ifdef TARGET_WIN_X8664
int __io_fputc(int, FILE *);
#endif
int fputs(const char *, FILE *);
size_t fread(void *, size_t, size_t, FILE *);
FILE *freopen(const char *, const char *, FILE *);
int fscanf(FILE *, const char *, ...);
int fseek(FILE *, long, int);
long ftell(FILE *);
#if defined(LINUX) && defined(PGLF64)
int fseeko64(FILE *, seekoff64_t, int);
seekoff64_t ftello64(FILE *);
#endif
#if defined(TARGET_WIN)
int _fseeki64(FILE *, seekoffx_t, int);
seekoffx_t _ftelli64(FILE *);
#endif
size_t fwrite(const void *, size_t, size_t, FILE *);
char *gets(char *);
int printf(const char *, ...);
int puts(const char *);
int remove(const char *);
int rename(const char *, const char *);
void rewind(FILE *);
int scanf(const char *, ...);
void setbuf(FILE *, char *);
int setvbuf(FILE *, char *, int, size_t);
FILE *tmpfile(void);
char *tmpnam(char *);
#if defined(TARGET_WIN) || defined(WIN32)
int ungetc(int, FILE *);
#endif
char *__io_tempnam(const char *, const char *);

WIN_MSVCRT_IMP void WIN_CDECL perror(const char *);

#else

int __io_errno();
void __io_set_errno();
FILE *__io_stdin();
FILE *__io_stdout();
FILE *__io_stderr();
int __io_getc();
int __io_putc();
int __io_getchar();
int __io_putchar();
void __io_clearerr();
int __io_getfd();
int __io_feof();
int __io_ferror();
int __io_isatty();
int __io_binary_mode();
int __io_setmode_binary();
int __io_ispipe();
size_t __io_fwrite();
int __io_timezone();
int fclose();
int fflush();
int fgetpos();
char *fgets();
FILE *fopen();
int fprintf();
int fputs();
size_t fread();
FILE *freopen();
int fscanf();
int fseek();
int fsetpos();
long int ftell();
#if defined(LINUX) && defined(PGLF64)
int fseeko64();
seekoff64_t ftello64();
#endif
#if defined(TARGET_WIN)
int _fseeki64(FILE *, seekoffx_t, int);
seekoffx_t _ftelli64(FILE *);
#endif
size_t fwrite();
char *gets();
int printf();
int puts();
int remove();
int rename();
void rewind();
int scanf();
void setbuf();
int setvbuf();
int sprintf();
int sscanf();
FILE *tmpfile();
char *tmpnam();
int ungetc();
char *__io_tempnam();

WIN_MSVCRT_IMP void WIN_CDECL perror(const char *);

#endif

extern void *__aligned_malloc(size_t, size_t); /* pgmemalign.c */
extern void __aligned_free(void *);

#endif
