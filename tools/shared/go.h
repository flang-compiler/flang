/*
 * Copyright (c) 2015-2018, NVIDIA CORPORATION.  All rights reserved.
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

#ifndef GO_H_
#define GO_H_

#if DEBUG

/**
 * \file
 * go() - an internal compiler debugging tool.
 *
 * go() is a trace output routine.  It works like an fprintf() to
 * the compiler's debug output file.  There is an implied newline.
 * Set -Mq,64,1 or -Mq,65,(odometer cutoff) to enable the trace output.
 *
 * More important, go() is also an assist to locating exactly which
 * discretionary change to the program may have caused it to break or to slow
 * down.  go() has a hidden odometer that increments by one on each invocation.
 * Its return value indicates whether this odometer has passed a cutoff value
 * set by -Mq,65.  By setting -Mq,65, you can do a binary search and quickly
 * isolate the change that broke the compiler's output.
 *
 * Typical usage:
 *
 *	if (particular_change_is_possible() &&
 *	    particular_change_seems_profitable() &&
 *	    go("changing %d to %d at %d", x, y, z)) {
 *		apply_the_change();
 *	}
 *
 * This header also defines dbgprintf(), a debug output routine
 * whose output is similarly enabled by -Mq,64,1 and -Mq,65.  dbgprintf()
 * works exactly like fprintf() and has no implied newline.
 *
 * This header also defines CHECK(), a convenient wrapper for assert().
 */

#define CHECK(x)                                                        \
  assert((x), "CHECK(" #x "): false at " __FILE__ ":", __LINE__, ERR_Fatal)

/**
   \brief fprintf() to gbl.dbgfil, or to the standard error output if none
 */
void dbgprintf(const char *format, ...);

/**
   \brief implements go() by returning a function pointer to something that
   works like fprintf() but returns a Boolean
 */
int (*go_odometer(const char *file, int line))(const char *format, ...);

#define go go_odometer(__FILE__, __LINE__)

#else // DEBUG == 0

/* In non-DEBUG mode, just evaluate argument(s) only for side effects by
 * means of a parenthesized comma-expression.
 */
#define CHECK
#define dbgprintf

INLINE static int go(const char *format, ...) {
  return 1;
}

#endif // DEBUG != 0

#endif /* GO_H_ */
