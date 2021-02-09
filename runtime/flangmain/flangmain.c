/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

#include "FuncArgMacros.h"

#if defined(__PGI)
#pragma global - Mx, 119, 2048
#pragma global - x 119 0x10000000
#pragma global - x 129 0x200
#endif

extern char **__io_environ();
extern void __io_set_argc(int);
extern void __io_set_argv(char **);

#if defined(PGDLL) && defined(_WIN32) && !defined(_WIN64)
struct {
  char *pghpf_01p;
  char *pghpf_02p;
  char *pghpf_03p;
  char *pghpf_04p;
} pghpf_0;

struct {
  char *pghpf_0cp;
} pghpf_0c;

struct {
  char *pg_typep;
} pg_type;

char *__get_fort_01_addr(void);
char *__get_fort_02_addr(void);
char *__get_fort_03_addr(void);
char *__get_fort_04_addr(void);
char *__get_fort_0c_addr(void);
char *__get_fort_type_addr(void);
#endif

int main(int argc, char** argv)
{
  int i = 0;

  __io_set_argc(argc);
  __io_set_argv(argv);

#if defined(PGDLL) && defined(_WIN32) && !defined(_WIN64)
  pghpf_0.pghpf_01p = __get_fort_01_addr();
  pghpf_0.pghpf_02p = __get_fort_02_addr();
  pghpf_0.pghpf_03p = __get_fort_03_addr();
  pghpf_0.pghpf_04p = __get_fort_04_addr();
  pghpf_0c.pghpf_0cp = __get_fort_0c_addr();
  pg_type.pg_typep = __get_fort_type_addr();
#endif
  MAIN_(argc, argv, __io_environ());
  ENTF90(EXIT, exit)(&i);
}
