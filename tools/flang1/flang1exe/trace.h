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

#if DEBUG
#include <stdarg.h>

/* print a message, continue */
#define Trace(a) TraceOutput a

static void
TraceOutput(const char *fmt, ...)
{
  va_list argptr;
  va_start(argptr, fmt);

  if (DBGBIT(TRACEFLAG, TRACEBIT)) {
    if (gbl.dbgfil) {
      fprintf(gbl.dbgfil, TRACESTRING);
      vfprintf(gbl.dbgfil, fmt, argptr);
      fprintf(gbl.dbgfil, "\n");
    } else {
      fprintf(stderr, TRACESTRING);
      fprintf(stderr, "Trace: ");
      vfprintf(stderr, fmt, argptr);
      fprintf(stderr, "\n");
    }
    va_end(argptr);
  }
} /* TraceOutput */
#else
/* DEBUG not set */
/* eliminate the trace output */
#define Trace(a)
#endif
