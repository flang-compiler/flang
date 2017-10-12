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
 * \brief Error handling and reporting module.
 */
#include <stdarg.h>

#include "gbldefs.h"
#include "global.h"
#include "error.h"

/** only for summary() **/
#include "symtab.h"

#include "version.h"
/* include error text definitions from errmsg utility: */
#define ERRMSG_GET_ERRTXT_TABLE 1
#include "errmsgdf.h"
#undef ERRMSG_GET_ERRTXT_TABLE

static char *errfill(const char *, const char *, const char *);
static void display_error(error_code_t ecode, enum error_severity sev,
                          int eline, const char *op1, const char *op2, int col,
                          const char *srcFile);

static int ndiags[5];
static int maxfilsev = 0;  /* max severity for entire source file */
static int totaldiags = 0; /* total number of messages for this source
                            * file */
static int emit_errmsg = 1;

/** \brief Initialize error counts for new user subprogram */
void
errini()

{
  ndiags[1] = ndiags[2] = ndiags[3] = ndiags[4] = gbl.maxsev = totaldiags = 0;
}

/** \brief Print version info */
void
errversion()
{
  fprintf(stderr, "%s/%s %s %s%s%s\n", version.lang, version.target,
          version.host, version.vsn, version.product, version.bld);
  fprintf(stderr, "%s\n", version.copyright);
}

/** \brief Construct and issue an error message.
 *
 * Construct error message and issue it to user terminal and to listing file
 * if appropriate.
 *
 * \param ecode  error number
 * \param sev    error severity (a value in the err_severity enum)
 * \param eline  source file line number
 * \param op1    string to be expanded into error message * or 0
 * \param op2    string to be expanded into error message * or 0
 */
void
error(error_code_t ecode, enum error_severity sev, int eline, const char *op1,
      const char *op2)
{
  display_error(ecode, sev, eline, op1, op2, 0, NULL);
}

static void
display_error(error_code_t ecode, enum error_severity sev, int eline,
              const char *op1, const char *op2, int col, const char *srcFile)
{
  static char sevlett[5] = {'X', 'I', 'W', 'S', 'F'};
  char *formatstr;
  char buff[400];
  int lastmsg;
  char *msgstr;

  if (sev < ERR_Informational || sev > ERR_Fatal)
    sev = ERR_Fatal;
  /*  check if informationals and warnings are inhibited  */
  if (gbl.nowarn && sev <= ERR_Warning)
    return;
  /* don't count informs if -inform warn */
  if (sev > ERR_Informational || sev >= flg.inform)
    ndiags[sev]++;

  if ((sev > ERR_Informational || sev >= flg.inform) && sev > gbl.maxsev) {
    gbl.maxsev = sev;
    if (sev > maxfilsev)
      maxfilsev = sev;
  }

  if (sev >= flg.inform) {
    if (gbl.curr_file != NULL || srcFile != NULL) {
      if (eline) {
        if (col > 0)
          formatstr = "%s-%c-%04d-%s (%s: %d.%d)";
        else
          formatstr = "%s-%c-%04d-%s (%s: %d)";
      } else
        formatstr = "%s-%c-%04d-%s (%s)";
    } else
      formatstr = "%s-%c-%04d-%s";

    lastmsg = sizeof(errtxt) / sizeof(char *);
    if (ecode < lastmsg) {
      msgstr = errtxt[ecode];
    } else {
      msgstr = "Unknown error code";
    }

    if (!XBIT(0, 0x40000000) && col <= 0 && srcFile == NULL)
      snprintf(&buff[1], sizeof(buff) - 1, formatstr, version.lang,
               sevlett[sev], ecode, errfill(msgstr, op1, op2), gbl.curr_file,
               eline);
    else {
      static char *sevtext[5] = {"X", "info", "warning", "error", "error"};
      if (col > 0 && (srcFile != NULL || gbl.curr_file != NULL)) {
        snprintf(&buff[1], sizeof(buff) - 1, "\n%s:%d:%d: %s %c%04d: %s",
                 (srcFile != NULL) ? srcFile : gbl.curr_file, eline, col,
                 sevtext[sev], sevlett[sev], ecode, errfill(msgstr, op1, op2));
      } else if (srcFile != NULL) {
        snprintf(&buff[1], sizeof(buff) - 1, "\n%s:%d: %s %c%04d: %s", srcFile,
                 eline, sevtext[sev], sevlett[sev], ecode,
                 errfill(msgstr, op1, op2));
      } else if (gbl.curr_file != NULL) {
        snprintf(&buff[1], sizeof(buff) - 1, "%s(%d) : %s %c%04d : %s",
                 gbl.curr_file, eline, sevtext[sev], sevlett[sev], ecode,
                 errfill(msgstr, op1, op2));
      } else
        snprintf(&buff[1], sizeof(buff) - 1, "%s : %s %c%04d : %s", "",
                 sevtext[sev], sevlett[sev], ecode, errfill(msgstr, op1, op2));
    }
    if (emit_errmsg)
      fprintf(stderr, "%s\n", &buff[1]);
#if DEBUG
    if (DBGBIT(0, 2))
      fprintf(gbl.dbgfil, "%s\n", &buff[1]);
#endif
    if (flg.list || flg.code || flg.xref) {
      if (flg.dbg[14]) {
        buff[0] = '#'; /* make sure listing is assembleable */
        list_line(buff);
      } else {
        list_line(&buff[1]);
      }
    }
  }

  if (sev == ERR_Fatal) {
    if (col <= 0 || (srcFile == NULL && gbl.curr_file == NULL)) {
      finish();
    }
  }

  if (sev >= ERR_Severe)
    totaldiags++;

  if (totaldiags >= flg.errorlimit && !DBGBIT(0, 64))
    errfatal(F_0008_Error_limit_exceeded);
}

/** \brief Massage label name if necesseary.
 *
 * \param ecode:    error number
 * \param sev  :    error severity in range 1 ... 4
 * \param eline:    source file line number
 * \param nm   :    label name
 * \param op2  :    extra string
 */
void
errlabel(error_code_t ecode, enum error_severity sev, int eline, char *nm,
         char *op2)
{
  nm += 2; /* skip past .L */
  while (*nm == '0')
    nm++; /* skip over leading 0's */
  if (*nm == 0)
    nm--;
  error(ecode, sev, eline, nm, op2);
}

/** \brief Expand error message template: replace '$' with text of the other
 * two operands
 *
 * \param intxt error message template
 * \param op1 first operand for substitution
 * \param op2 second operand for substitution
 * \return pointer to result
 */
static char *
errfill(const char *intxt, const char *op1, const char *op2)
{
  static char outtxt[200]; /* holds result string */
  char *p;                 /* points into outtxt */
  const char *op;
  extern char *getprint();

  /* calculate length of txt and operands to avoid overflow */
  int intxt_len;
  int op_len, op1_len, op2_len;
  int op_adj_len, op2_adj_len;
  int buf_len, tot_len, len_left;

  buf_len = 200;
  op_adj_len = op2_adj_len = 0;
  intxt_len = (intxt != NULL) ? strlen(intxt) : 0;
  op1_len = op_len = (op1 != NULL) ? strlen(op1) : 0;
  op2_len = (op2 != NULL) ? strlen(op2) : 0;
  tot_len = intxt_len + op1_len + op2_len;
  len_left = buf_len;
  if (tot_len > buf_len) {
    len_left = buf_len - intxt_len;
    if (!op2_len)
      op_adj_len = len_left;
    else {
      if (op_len > len_left / 2) {
        if (op2_len > len_left / 2)
          op_adj_len = op2_adj_len = len_left / 2;
        else
          op_adj_len = len_left - op2_len;
      } else
        op2_adj_len = len_left - op1_len;
    }
  }

  p = outtxt;
  op = op1;

  while ((*p = *intxt++) != 0) {
    if (*p++ == '$') {
      p--;
      if (op == 0)
        op = "";
      if (tot_len > buf_len) {
        if (op_adj_len && (op_len != op_adj_len)) {
          strncpy(p, op, op_adj_len - 3);
          p += op_adj_len - 3;
          strcpy(p, "...");
          p += 3;
        } else {
          strncpy(p, op, op_len);
          p += op_len;
        }
        op_len = op2_len;
        op_adj_len = op2_adj_len;
      } else {
        strcpy(p, op);
        p += strlen(op);
      }
      op = op2;
    }
  }
  return (outtxt);
}

/* Do printf-style formatting of the message by: compute the size of the
 * required buffer, allocate it, sprintf into it, then free it. */
void
interrf(enum error_severity sev, const char *fmt, ...)
{
  size_t size;
  char *buffer;
  va_list ap;

#if !DEBUG
  if (sev == ERR_Informational)
    return;
#endif
  va_start(ap, fmt);
  size = vsnprintf(NULL, 0, fmt, ap);
  va_end(ap);
  NEW(buffer, char, size + 1);
  va_start(ap, fmt);
  vsprintf(buffer, fmt, ap);
  va_end(ap);
  error(0, sev, gbl.lineno, buffer, 0);
  FREE(buffer);
}

/** \brief Issue internal compiler error.
 * \param txt:   null terminated text string identifying
 * \param val:   integer value to be written with message
 * \param sev:   error severity
 */
void
interr(const char *txt, int val, enum error_severity sev)
{
  interrf(sev, "%s %7d", txt, val);
}

#if DEBUG
/**
  * Prints information on behalf of failed DEBUG_ASSERT.
  * \param filename: name of file where assertion failed.
  * \param line: line number where assertion failed.
  * \param expr: string representation of assertion that failed.
  * \param txt: optional text to print via interr.  Use NULL if there is no
 * text.
  */
void
dassert_err(const char *filename, int line, const char *expr, const char *txt)
{
  /* Since we reach here only in DEBUG mode, there's no point in
     being clever about creating a single string to pass to interr.
     Just get the information to the compiler developer via stderr. */
  (void)fprintf(stderr, "%s:%d: DEBUG_ASSERT %s failed\n", filename, line,
                expr);
  interr(txt, 0, error_max_severity());
}

/**
  * Prints information on behalf of failed asrt.
  * \param file: filename: name of file where assertion failed.
  * \param line: line number where assertion failed
  */
void
asrt_failed(const char *filename, int line)
{
  fprintf(stderr, "asrt failed. line %d, file %s\n", line, filename);
  /* Call interr so that we have a common place to set a breakpoint when
     running under a debugger. */
  interr("asrt failed", 0, ERR_Warning);
}
#endif

char *
errnum(int num)
{
  static char n[20];
  sprintf(n, "%d", num);
  return n;
} /* errnum */

/** \brief Issue an informational message */
void
errinfo(error_code_t ecode)
{
  error(ecode, ERR_Informational, gbl.lineno, CNULL, CNULL);
}

/** \brief Issue a warning */
void
errwarn(error_code_t ecode)
{
  error(ecode, ERR_Warning, gbl.lineno, CNULL, CNULL);
}

/** \brief Issue severe error */
void
errsev(error_code_t ecode)
{
  error(ecode, ERR_Severe, gbl.lineno, CNULL, CNULL);
}

/** \brief Issue fatal error */
void
errfatal(error_code_t ecode)
{
  error(ecode, ERR_Fatal, gbl.lineno, CNULL, CNULL);
}

/** Write either error summary message for current subprogram unit, or final
 * execution summary line to user's terminal. Return the maximum error severity
 * seen for the entire file.
 */
int
summary(LOGICAL final, int ipafollows)
{
  static char *t[5] = {
      "%s/%s %s %s%s%s: compilation successful\n",
      "%s/%s %s %s%s%s: compilation completed with informational messages\n",
      "%s/%s %s %s%s%s: compilation completed with warnings\n",
      "%s/%s %s %s%s%s: compilation completed with severe errors\n",
      "%s/%s %s %s%s%s: compilation aborted\n"};
  static int empty_file = TRUE;

  if (!final) {
    if (!flg.terse || gbl.maxsev > 1)
      fprintf(stderr,
              "%3d inform, %3d warnings, %3d severes, %1d fatal for %s\n",
              ndiags[1], ndiags[2], ndiags[3], ndiags[4], SYMNAME(gbl.currsub));
    empty_file = FALSE;
  } else if (!empty_file || !ipafollows) {
    if (empty_file && maxfilsev < 3)
      errwarn(6);
    if (!flg.terse || gbl.maxsev > 1)
      fprintf(stderr, t[maxfilsev], version.lang, version.target, version.host,
              version.vsn, version.product, version.bld);
  }
  return maxfilsev;
}

void
erremit(int x)
{
  emit_errmsg = x;
}

/** \brief Floating point error */
void
fperror(int errcode)
{
  /* floating point error codes */
  static struct {
    int ovf;
    int unf;
    int invop;
  } lineno = {-1, -1, -1};

  gbl.fperror_status = errcode;
  if (gbl.nofperror)
    return;
  switch (errcode) {
  case FPE_NOERR:
    break;
  case FPE_FPOVF: /* floating point overflow */
    if (lineno.ovf == gbl.lineno)
      break;
    lineno.ovf = gbl.lineno;
    errwarn(129);
    break;
  case FPE_FPUNF: /* floating point underflow */
    if (lineno.unf == gbl.lineno)
      break;
    lineno.unf = gbl.lineno;
    errwarn(130);
    break;
  case FPE_INVOP: /* invalid operand */
    if (lineno.invop == gbl.lineno)
      break;
    lineno.invop = gbl.lineno;
    errwarn(132);
    break;
  default:
    interr("invalid floating point error code", (int)errcode, 3);
  }
}

int
error_max_severity()
{
  return maxfilsev;
}

