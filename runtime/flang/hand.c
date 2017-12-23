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

#ifndef _WIN32
#include <sys/signal.h>
#include "stdioInterf.h"
#include "fioMacros.h"

#if defined(_WIN32)
#define write _write
#endif

extern char *__fort_getopt(char *);

/* signals handled and message strings */

struct sigs {
  int sig;   /* signal value */
  char *str; /* message string */
};

static struct sigs sigs[] = {
    {SIGHUP, "hangup"},
    {SIGINT, "interrupt"},
    {SIGQUIT, "quit"},
    {SIGILL, "illegal instruction"},
    {SIGTRAP, "trace trap"},
    {SIGIOT, "IOT instruction"},
    {SIGBUS, "bus error"},
    {SIGFPE, "floating point exception"},
    /*	{SIGKILL,"kill"}, */
    {SIGBUS, "bus error"},
    {SIGSEGV, "segmentation violation"},
    {SIGPIPE, "write on a pipe with no one to read it"},
    /*	{SIGALRM,"alarm clock"}, */
    /*	{SIGTERM,"software termination"},*/
    {SIGTERM, NULL},
/*	{SIGURG,"urgent condition on IO channel"}, */
/*	{SIGSTOP,"sendable stop signal not from tty"}, */
/*	{SIGTSTP,"stop signal from tty"}, */
/*	{SIGCONT,"continue a stopped process"}, */
/*	{SIGCHLD,"to parent on child stop or exit"}, */
/*	{SIGTTIN,"to readers pgrp upon background tty read"}, */
/*	{SIGTTOU,"like TTIN for output if (tp->t_local&LTOSTOP)"}, */
/*	{SIGIO,"input/output possible signal"}, */
/*	{SIGXCPU,"exceeded CPU time limit"}, */
/*	{SIGXFSZ,"exceeded file size limit"}, */
/*	{SIGVTALRM,"virtual time alarm"}, */
/*	{SIGPROF,"profiling time alarm"}, */
/*	{SIGWINCH,"window changed"}, */
/*	{SIGLOST,"resource lost (eg, record-lock lost)"}, */
/*	{SIGUSR1,"user defined signal 1"}, */
/*	{SIGUSR2,"user defined signal 2"}, */
    {0, NULL} /* end of list */
};

/* print signal message */

void __fort_psignal(lcpu, s) int lcpu;
int s;
{
  char buf[256];
  int n;

  n = 0;
  while ((sigs[n].sig != 0) && (sigs[n].sig != s)) {
    n++;
  }
  if (sigs[n].sig == 0) {
    sprintf(buf, "%d: killed by unknown signal %d\n", lcpu, s);
    write(2, buf, strlen(buf));
  } else if (sigs[n].str != NULL) {
    sprintf(buf, "%d: %s\n", lcpu, sigs[n].str);
    write(2, buf, strlen(buf));
  }
}

/* handler for signals */

static void sighand(s) int s;
{
  int lcpu;

  lcpu = __fort_myprocnum();
  __fort_psignal(lcpu, s); /* print message */
#if !defined(_WIN32)
  sleep(1); /* wait for message to clear */
#endif
  __fort_abort(NULL); /* abort */
}

/* set handlers for signals */

void
__fort_sethand()
{
  char *p;
  int n;

  p = __fort_getopt("-sigmsg");
  if (p == NULL) {
    return;
  }
  if ((*p == 'y') || (*p == 'Y') || (*p == 'a') || (*p == 'A') ||
      (*p == '\0')) {
    n = 0;
    while (sigs[n].sig != 0) {
      signal(sigs[n].sig, sighand);
      n++;
    }
  } else {
    while (*p != '\0') {
      n = __fort_strtol(p, &p, 0);
      signal(n, sighand);
      p = (*p == ',' ? p + 1 : p);
    }
  }
}

#else
void
__fort_sethand()
{
}
#endif