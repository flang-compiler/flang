/*
 * Copyright (c) 2006-2017, NVIDIA CORPORATION.  All rights reserved.
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

#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include "llcrit.h"
#include "stdioInterf.h"


/* options */

#define T_DEBUG 1  /* enable just in time debugging */
#define T_TRACE 2  /* enable stack traceback */
#define T_SIGNAL 4 /* enable signal handlers */
#define T_ABORT 8  /* enable abort call */
#define T_TEST 16  /* enable test code */

/* list of options */

struct opts {
  char *opt; /* option string */
  int val;   /* value */
};

static struct opts opts[] = {{"debug", T_DEBUG | T_SIGNAL},
                             {"trace", T_TRACE | T_SIGNAL},
                             {"signal", T_SIGNAL},
                             {"abort", T_ABORT},
                             {"test", T_TEST},
                             {NULL, 0}};

/* the default options */

static int tracopt = 0;

static char *fn; /* image name */

/* this routine allows a debugger to set a breakpoint before exit */

void
dbg_stop_before_exit(void)
{
}

/*
 * this routine prints a message and terminates the process
 * sv values
 * 0 - no traceback
 * 1 - traceback (other errors)
 * 2 - traceback (perror)
 * 3 - traceback (signal)
 */

#if defined(_WIN32)
#define getpid _getpid
#define _Exit _exit
#endif

void
__abort(int sv, char *msg)
{
  char cmd[128];
  char *p;
  int n;
  const char * dbg_env = "F90_TERM_DEBUG";
  const char * dbg_cmd = "gdb -p %d";

  if (msg != NULL) {
    fprintf(__io_stderr(), "Error: %s\n", msg);
  }
  dbg_stop_before_exit(); /* for the debugger */
  if (sv == 0) {   /* nothing more to do */
    exit(127);
  }
  fflush(__io_stderr());
  if (tracopt & T_DEBUG) {
    p = getenv(dbg_env);
    p = (char *)(p == NULL ? dbg_cmd : p);
    sprintf(cmd, p, getpid());
    system(cmd);
  } else if (tracopt & T_TRACE) {
    if (sv == 3) /* don't display "abort" routines */
      n = 2;
    else if (sv == 2)
      n = 2;
    else
      n = 1;
    __abort_trace(n);
  }
  if (tracopt & T_TEST) {
    if (sv == 3) /* don't display "abort" routines */
      n = 3;
    else if (sv == 2)
      n = 3;
    else
      n = 2;
    __abort_trace(n);
  }
  if (tracopt & T_ABORT) { /* try for core file */
    signal(SIGABRT, SIG_DFL);
    abort();
  }
  _Exit(127);
}

/*
 * this routine assumes errno is set, calls perror, and terminates the
 * process
 */

void
__abort_err(char *msg)
{
  fprintf(__io_stderr(), "Error: ");
  perror(msg);
  __abort(2, NULL);
}

/*
 * this routine is called from the initialization
 */

void
__abort_init(char *path)
{
  char *p;
  struct opts *op;
  int n;
  int neg;

#if defined(_WIN32)
  fn = path;
#endif
  p = getenv("TRACE_TERM");
  if (p != NULL) {
    while (*p != '\0') {
      if (strncmp(p, "no", 2) == 0) {
        neg = 1;
        p += 2;
      } else {
        neg = 0;
      }
      op = opts;
      while (1) {
        if (op->opt == NULL) {
          fprintf(__io_stderr(), "Error: TRACE_TERM invalid value\n");
          exit(127);
        }
        n = strlen(op->opt);
        if (strncmp(p, op->opt, n) == 0) {
          break;
        }
        op++;
      }
      if (neg == 0) {
        tracopt |= op->val;
      } else {
        tracopt &= ~op->val;
      }
      p += n;
      if (*p == ',') {
        p++;
      } else if (*p == '\0') {
        break;
      } else {
          fprintf(__io_stderr(), "Error: TRACE_TERM invalid value\n");
        exit(127);
      }
    }
  }
  if (tracopt & T_SIGNAL) {
    __abort_sig_init();
  }
}

/*
 * this routine is called from the initialization.
 * mask align fault for Barcelona B0 processor.
 */
void
__ctrl_init(void)
{
}

/* return image path */

char *
__abort_image(void)
{
  return (fn);
}

