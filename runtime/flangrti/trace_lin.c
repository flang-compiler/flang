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

#include <stdioInterf.h>
#ifndef _WIN32
#include <sys/ucontext.h>
#include "dumpregs.h"
#include <signal.h>
#include <execinfo.h>

/* codes and strings for signals */

struct cods {
  int code;  /* signal code */
  char *str; /* string */
};

#define CODNULL ((struct cods *)0)

static struct cods codill[] = {{ILL_ILLOPC, "illegal opcode"},
                               {ILL_ILLOPN, "illegal operand"},
                               {ILL_ILLADR, "illegal addressing mode"},
                               {ILL_ILLTRP, "illegal trap"},
                               {ILL_PRVOPC, "privileged opcode"},
                               {ILL_PRVREG, "privileged register"},
                               {ILL_COPROC, "coprocessor error"},
                               {ILL_BADSTK, "internal stack error"},
                               {0, NULL}};

static struct cods codfpe[] = {{FPE_INTDIV, "integer divide by zero"},
                               {FPE_INTOVF, "integer overflow"},
                               {FPE_FLTDIV, "floating point divide by zero"},
                               {FPE_FLTOVF, "floating point overflow"},
                               {FPE_FLTUND, "floating point underflow"},
                               {FPE_FLTRES, "floating point inexact result"},
                               {FPE_FLTINV, "floating point invalid operation"},
                               {FPE_FLTSUB, "subscript out of range"},
                               {0, NULL}};

static struct cods codsegv[] = {
    {SEGV_MAPERR, "address not mapped to object"},
    {SEGV_ACCERR, "invalid permissions for mapped object"},
    {0, NULL}};

static struct cods codbus[] = {{BUS_ADRALN, "invalid address alignment"},
                               {BUS_ADRERR, "non-existent physical address"},
                               {BUS_OBJERR, "object specific hardware error"},
                               {0, NULL}};

/* signals handled and message strings */

struct sigs {
  int sig;          /* signal value */
  struct cods *cod; /* address of optional code info */
  char *str;        /* message string */
};

static struct sigs sigs[] = {
    {SIGINT, CODNULL, "interrupt"},
    {SIGILL, codill, "illegal instruction"},
    {SIGABRT, CODNULL, "abort"},
    {SIGFPE, codfpe, "floating point exception"},
    {SIGSEGV, codsegv, "segmentation violation"},
    {SIGTERM, CODNULL, "software termination"},
    {SIGPIPE, CODNULL, "write on a pipe with no one to read it"},
    {SIGSYS, CODNULL, "bad argument to system call"},
    {SIGHUP, CODNULL, "hangup"},
    {SIGBUS, codbus, "bus error"},
    {SIGQUIT, CODNULL, "quit"},
    {SIGTRAP, CODNULL, "trace trap"},
    {SIGIOT, CODNULL, "IOT instruction"},
    {0, CODNULL, NULL} /* end of list */
};

static gregset_t *regs; /* pointer to regs at signal  */

/* walk the stack back */

#define MAXTRACE (32 * 1024)

void
__abort_trace(int skip)
{
  void *array[MAXTRACE];
  size_t size;
  char **strings;
  size_t i;

  if (regs != (gregset_t *)0) {
    dumpregs(regs);
  }

  size = backtrace(array, MAXTRACE);
  if (skip + 1 >= size) {
    fprintf(__io_stderr(), "  --- traceback not available\n");
    return;
  }
  strings = backtrace_symbols(array, size);
  if (size < 100) {
    for (i = skip + 1; i < size; i++)
      fprintf(__io_stderr(), "  %s\n", strings[i]);
  } else {
    for (i = skip + 1; i < 40; i++)
      fprintf(__io_stderr(), "  %s\n", strings[i]);
    fprintf(__io_stderr(), "  --- skipping traceback entries\n");
    for (i = size - 40; i < size; i++)
      fprintf(__io_stderr(), "  %s\n", strings[i]);
  }
  free(strings);
}

/*
 * this routine is a signal handler, it prints a message and terminates the
 * process
 */

static void
__abort_sig_hand(int sig, siginfo_t *in, ucontext_t *u)
{
  char *p;
  char b[128];
  int n, m;
  struct sigaction new;
  struct sigaction old;

  new.sa_sigaction = (void (*)(int, siginfo_t *, void *))SIG_DFL;
  sigemptyset(&new.sa_mask);
  new.sa_flags = SA_SIGINFO;
  n = 0;
  while (sigs[n].sig != 0) {
    sigaction(sigs[n].sig, &new, &old);
    n++;
  }

  regs = getRegs(u);

  n = 0;
  while ((sigs[n].sig != 0) && (sigs[n].sig != sig)) {
    n++;
  }
  if (sigs[n].sig == 0) {
    sprintf(b, "signal %d", sig);
    p = b;
  } else {
    p = sigs[n].str;
    m = 0;
    if (sigs[n].cod != CODNULL) {
      while ((sigs[n].cod[m].code != 0) &&
             (sigs[n].cod[m].code != in->si_code)) {
        m++;
      }
      if (sigs[n].cod[m].code != 0) {
        sprintf(b, "%s, %s", p, sigs[n].cod->str);
        p = b;
      }
    }
  }
  __abort(3, p);
}

/*
 * this routine initializes the signal handlers
 */

void
__abort_sig_init(void)
{
  struct sigaction new;
  struct sigaction old;
  int n;

  new.sa_sigaction = (void (*)(int, siginfo_t *, void *))__abort_sig_hand;
  sigemptyset(&new.sa_mask);
  new.sa_flags = SA_SIGINFO;
  n = 0;
  while (sigs[n].sig != 0) {
    sigaction(sigs[n].sig, &new, &old);
    n++;
  }
}

#elif 0
#include <Windows.h>
#include <stdlib.h>
#include <signal.h>
#include <tchar.h>
#include <DbgHelp.h>

void
__abort_trace(int skip)
{
     unsigned int   i;
     void         * stack[ 100 ];
     unsigned short frames;
     SYMBOL_INFO  * symbol;
     HANDLE         process;

     process = GetCurrentProcess();

     SymInitialize( process, NULL, TRUE );

     frames               = CaptureStackBackTrace( 0, 100, stack, NULL );
     symbol               = ( SYMBOL_INFO * )calloc( sizeof( SYMBOL_INFO ) + 256 * sizeof( char ), 1 );
     symbol->MaxNameLen   = 255;
     symbol->SizeOfStruct = sizeof( SYMBOL_INFO );

     for( i = 0; i < frames; i++ )
     {
         SymFromAddr( process, ( DWORD64 )( stack[ i ] ), 0, symbol );

         printf( "%i: %s - 0x%0X\n", frames - i - 1, symbol->Name, symbol->Address );
     }  

     free( symbol );
    
    exit(1);
}

void
__abort_sig_init(void)
{ 
    signal(SIGSEGV , __abort_trace);
    signal(SIGILL , __abort_trace);
    signal(SIGABRT, __abort_trace);
    signal(SIGFPE, __abort_trace);
/*
    SIGABRT	Abnormal termination
    SIGFPE	Floating-point error
    SIGILL	Illegal instruction
    SIGINT	CTRL+C signal
    SIGSEGV	Illegal storage access
    SIGTERM	Termination request

*/
}
#else
void
__abort_trace(int skip)
{ }

void
__abort_sig_init(void)
{ }
#endif
