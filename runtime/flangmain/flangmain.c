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

#include "FuncArgMacros.h"

#if defined(__PGI)
#pragma global - Mx, 119, 2048
#pragma global - x 119 0x10000000
#pragma global - x 129 0x200
#endif

extern char **__io_environ();
extern void __io_set_argc(int);
extern void __io_set_argv(char **);

#if defined(PGDLL) && defined(WINNT) && !defined(WIN64) && !defined(WIN32)
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

int main(argc, argv) int argc;
char **argv;
{
  int i = 0;

#if (defined(INTERIX86) || defined(INTERIX8664) || defined(WIN64) ||           defined(WIN32) || defined(TARGET_OSX_X86))
  //_pgimain(argc, argv);
#endif

  __io_set_argc(argc);
  __io_set_argv(argv);

#if   defined(WINNT) && !defined(WIN64) && !defined(WIN32)
#if defined(PGDLL)
  pghpf_0.pghpf_01p = __get_fort_01_addr();
  pghpf_0.pghpf_02p = __get_fort_02_addr();
  pghpf_0.pghpf_03p = __get_fort_03_addr();
  pghpf_0.pghpf_04p = __get_fort_04_addr();
  pghpf_0c.pghpf_0cp = __get_fort_0c_addr();
  pg_type.pg_typep = __get_fort_type_addr();
#endif
  MAIN(argc, argv, __io_environ());
  ENTF90(EXIT, exit)(&i);
#else
  MAIN_(argc, argv, __io_environ());
  ENTF90(EXIT, exit)(&i);
#endif
}
