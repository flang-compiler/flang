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
 * \brief Error handling and reporting.
 */

#ifndef PGERROR_H
#define PGERROR_H

#include "universal.h"

BEGIN_DECL_WITH_C_LINKAGE

/** \brief Severity of an error message.
 */
enum error_severity {
  ERR_Informational = 1,
  ERR_Warning = 2,
  ERR_Severe = 3,
  ERR_Fatal = 4
};

/** \brief Error code type
 */
typedef enum error_code error_code_t;

/** \brief Construct and issue error message
 *
 * Construct error message and issue it to user terminal and to listing file
 * if appropriate.
 *
 * \param ecode:	error number
 * \param sev:	        error severity in range 1 ... 4
 * \param eline:	source file line number
 * \param op1:		strings to be expanded into error message * or 0
 * \param op2:		strings to be expanded into error message * or 0
 */
void error(error_code_t ecode, enum error_severity sev, int eline,
           const char *op1, const char *op2);

/** \brief Issue internal compiler error using printf-style formatting.
 *
 * \param sev:   error severity.
 * \param fmt:   printf-style format string
 * \param ...:   args for format string
 */
void interrf(enum error_severity sev, const char *fmt, ...);

/** \brief Issue internal compiler error.
 *
 * \param txt:   null terminated text string identifying.
 * \param val:   integer value to be written with message.
 * \param sev:   error severity.
 */
void interr(const char *txt, int val, enum error_severity sev);

/** \brief Issue an informational message for gbl.lineno.
 */
void errinfo(error_code_t);

/** \brief Issue a warning for gbl.lineno.
 */
void errwarn(error_code_t);

/** \brief Issue a severe error message for gbl.lineno.
 */
void errsev(error_code_t);

/** \brief Issue a fatal error for gbl.lineno.
 */
void errfatal(error_code_t);

/** \brief Massage label name before calling error().
 */
void errlabel(error_code_t ecode, enum error_severity sev, int eline, char *nm,
              char *op2);

/** \brief Convert an integer to a string, for printing numbers in error
 * messages.
 *
 * This returns a pointer to static data, so only use it once.
 */
char *errnum(int x);

int error_max_severity(void);

void errini(void);
void errversion(void);
void erremit(int x);

#ifdef PGFTN
int summary(LOGICAL final, int ipafollows);
#endif

/** \brief Assert that cond is true, and emit an internal compiler error
 * otherwise.
 *
 * Note that unlike the C standard <assert.h> assert() macro, this version is
 * active in both debug and release builds, and expands to a statement,
 * not an expression.
 */
#define assert(cond, txt, val, sev) \
  if (cond)                         \
    ;                               \
  else                              \
  interr((txt), (val), (sev))

/** \brief If DEBUG!=0 and cond is false, emit an internal compiler error.
 *
 * Like the C standard <assert.h> assert macro, this version expands to an
 * expression and is active only in debug builds.  Severity of an error
 * is implicitly maximal.
 *
 * \param cond is the condition to be checked
 * \param txt is an additional info string, which may be NULL.
 */
#if DEBUG
#define DEBUG_ASSERT(cond, txt) \
  ((cond) ? (void)0 : dassert_err(__FILE__, __LINE__, #cond, (txt)))
void dassert_err(const char *, int line, const char *exp, const char *txt);
#else
#define DEBUG_ASSERT(cond, txt) ((void)0)
#endif

#if DEBUG
void asrt_failed(const char* file, int line);
#endif

END_DECL_WITH_C_LINKAGE

#endif /* PGERROR_H */
