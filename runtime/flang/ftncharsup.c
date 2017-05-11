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
 * Fortran character support routines
 */

#include "global.h"
#include "stdarg.h"
#include "enames.h"
#include <string.h>
#include "llcrit.h"
#include "mpalloc.h"

#ifndef NULL
#define NULL (void *)0
#endif


/* ***********************************************************************/
/** \brief
 * Copies a series of character strings into another.
 * 
 * This function is used
 * to implement character assignments and concatenations. It pads with
 * blanks if the destination string is longer than the sum of the lengths
 * of the source strings and truncates if the sum of the lengths of the
 * source strings is longer than the destination string.
 *
 * Allow the target of the concatenation to appear in its right-hand side
 * which is standard f90 and is a common extension to f77.
 *
 * \param    n              number of source strings
 * \param    to             pointer to destination string
 * \param    to_len         length of destination string
 * <pre>
 *  Vargs:
 *    {from           pointer to kth source string
 *     from_len}*     length of kth source string
 * </pre>
 */
/* ***********************************************************************/
void
Ftn_str_copy(int n, char *to, int to_len, ...)
{
  va_list ap;
  char *from;
  int from_len;
  int idx2;
  int cnt;
  typedef struct {
    char *str;
    int len;
    int dyn;
  } SRC_STR;
  SRC_STR *src_p, *qq;
  SRC_STR aa[4]; /* statically allocate for 4 source strings */
  int src_p_allocd;
  char *to_p;
  char *to_end;
  char *from_end;
  int any_allocd;

  if (to_len <= 0) {
    return;
  }
  if (n <= (sizeof(aa) / sizeof(SRC_STR))) {
    qq = src_p = aa;
    src_p_allocd = 0;
  } else {
    qq = src_p = (SRC_STR *)_mp_malloc(sizeof(SRC_STR) * n);
    src_p_allocd = 1;
  }
  va_start(ap, to_len);
#ifdef DEBUG
  printf("to_len = %d\n", to_len);
#endif
  to_end = to - 1;
  any_allocd = idx2 = 0;
  for (cnt = n; cnt > 0; cnt--, qq++) {
    from = va_arg(ap, char *);
    from_len = va_arg(ap, int);
#ifdef DEBUG
    printf("from_len = %d\n", from_len);
#endif
    if (from_len < 0)
      from_len = 0;
    qq->str = from;
    qq->len = from_len;
    qq->dyn = 0;
    to_end += from_len;
    from_end = from + (from_len - 1);
    if ((from >= to && from <= to_end) ||
        (from_end >= to && from_end <= to_end))
      if (from_len) {
        qq->str = _mp_malloc(from_len);
        memcpy(qq->str, from, from_len);
        qq->dyn = 1;
#ifdef DEBUG
        printf("string %d overlaps\n", n - cnt);
        printf("mallocd %08x\n", qq->str);
#endif
        any_allocd = 1;
      }
    idx2 += from_len;
    if (idx2 >= to_len)
      break;
  }
  va_end(ap);

  qq = src_p;
  to_p = to;
  to_end = to + to_len; /* position after the end of the destination */
  for (cnt = n; cnt > 0; cnt--, qq++) {
    from = qq->str;
    for (from_len = qq->len; from_len > 0; from_len--, from++) {
      *to_p++ = *from;
      if (to_p == to_end)
        goto exit_return;
#ifdef DEBUG
      printf("from_char = %c\n", *from);
#endif
    }
  }

exit_return:
  while (to_p < to_end) { /* remember, to_end is 1 after end */
    /* blank fill to right */
    *to_p++ = ' ';
  }

  if (any_allocd) {
    idx2 = 0;
    qq = src_p;
    for (cnt = n; cnt > 0; cnt--, qq++) {
      if (qq->dyn) {
        _mp_free(qq->str);
#ifdef DEBUG
        printf("freed   %08x\n", qq->str);
#endif
      }
      idx2 += qq->len;
      if (idx2 >= to_len)
        break;
    }
  }

  if (src_p_allocd)
    _mp_free(src_p);
}

/** \brief single source, no overlap */
void
Ftn_str_cpy1(char *to, int to_len, char *from, int from_len)
{
  char *to_p, *to_end;

  if (to_len <= 0) {
    return;
  }
  if (from_len < 0)
    from_len = 0;
  if (to_len <= from_len) {
    memcpy(to, from, to_len);
    return;
  }
  memcpy(to, from, from_len);
  /*memset(to+from_len, ' ', to_len - from_len);*/
  to_p = to + from_len;
  to_end = to + to_len;
  while (to_p < to_end) { /* remember, to_end is 1 after end */
    /* blank fill to right */
    *to_p++ = ' ';
  }
  return;
}

/* ***********************************************************************/
/** \brief
 * Implements the INDEX intrinsic; is an integer function which returns the
 * value according to the INDEX intrinsic.
 */
/* ***********************************************************************/
int Ftn_str_index(a1, a2, a1_len,
                  a2_len) char *a1; /* pointer to string being searched */
char *a2;                           /* pointer to string being searched for */
int a1_len;                         /* length of a1 */
int a2_len;                         /* length of a2 */
{
  int idx1, idx2, match;
  if (a1_len < 0)
    a1_len = 0;
  if (a2_len < 0)
    a2_len = 0;
  for (idx1 = 0; idx1 < a1_len; idx1++) {
    if (a2_len > (a1_len - idx1))
      return (0);
    match = TRUE;
    for (idx2 = 0; idx2 < a2_len; idx2++) {
      if (a1[idx1 + idx2] != a2[idx2]) {
        match = FALSE;
        break;
      }
    }
    if (match)
      return (idx1 + 1);
  }
  return (0);
}

/* ***********************************************************************/
/** \brief
 * Implements realational operators with string operands and the lexical
 * intrinsics. Returns integer value:
 * -  0 => strings are the same
 * - -1 => a1 lexically less than a2
 * -  1 => a1 lexically greater than a2
 * If the strings are of unequal lengths, treats shorter string as if it were
 * padded with blanks.
 */
/* ***********************************************************************/
int Ftn_strcmp(a1, a2, a1_len,
               a2_len) char *a1; /* first string to be compared */
char *a2;                        /* second string to be compared */
int a1_len;                      /* length of a1 */
int a2_len;                      /* length of a2 */
{
  int ret_val, idx1;
  if (a1_len < 0)
    a1_len = 0;
  if (a2_len < 0)
    a2_len = 0;
  if (a1_len == a2_len) {
    while (a1_len > 0) {
      if (*a1 != *a2) {
        if ((unsigned)(*a1) > (unsigned)(*a2))
          return 1;
        return -1;
      }
      ++a1;
      ++a2;
      a1_len--;
    }
    return 0;
  }
  if (a1_len > a2_len) {
    /* first compare the first a2_len characters of the strings */
    ret_val = memcmp(a1, a2, a2_len);
    if (ret_val != 0) {
      if (ret_val < 0)
        return (-1);
      if (ret_val > 0)
        return (1);
    }
    /*
     * if the last (a1_len - a2_len) characters of a1 are blank, then the
     * strings are equal; otherwise, compare the first non-blank char. to
     * blank
     */

    for (idx1 = 0; idx1 < (a1_len - a2_len); idx1++) {
      if (a1[a2_len + idx1] != ' ') {
        if (a1[a2_len + idx1] > ' ')
          return (1);
        return (-1);
      }
    }
    return (0);
  } else {
    /* a2_len > a1_len */
    /* first compare the first a1_len characters of the strings */
    ret_val = memcmp(a1, a2, a1_len);
    if (ret_val != 0) {
      if (ret_val < 0)
        return (-1);
      if (ret_val > 0)
        return (1);
    }
    /*
     * if the last (a2_len - a1_len) characters of a2 are blank, then the
     * strings are equal; otherwise, compare the first non-blank char. to
     * blank
     */

    for (idx1 = 0; idx1 < (a2_len - a1_len); idx1++) {
      if (a2[a1_len + idx1] != ' ') {
        if (a2[a1_len + idx1] > ' ')
          return (-1);
        return (1);
      }
    }
    return (0);
  }
}

/* ***********************************************************************/
/** \brief
 * Utility routines to allocatespace for character expressions
 * whose lengths are known only at run-time.
 * The compiler creates a variable to locate the list of blocks allocated
 * during a subprogram. This variable is initialized to NULL upon entry to
 * the subprogram.  When the subprogram exits, all of the blocks are freed.
 * Each block of space consists of n 'words':
 * - ++  first word        - pointer to the next allocated block,
 * - ++  remaining word(s) - space for the character data.
 *
 * \param     size - number of bytes needed,
 * \param     hdr  - pointer to the compiler-created variable locating the
 *            list of allocated blocks. Ftn_str_malloc updates this  variable.
 * \returns  returns a pointer to the space after the 'next pointer'.
 *
 * Note that KANJI versions are unneeded since the compiler just calls
 * Ftn_str_malloc() with an adjusted length.
 */
/* ***********************************************************************/
char **
Ftn_str_malloc(int size, char ***hdr)
{
  int nbytes;
  char **p, **q;

/*
 * round request to the size of a pointer & also accommodate a 'next'
 * pointer
 */
#define PTRSZ sizeof(char *)
  nbytes = ((size + PTRSZ - 1) / PTRSZ) * PTRSZ + PTRSZ;
  p = (char **)_mp_malloc(nbytes);
  if (p == NULL) {
    MP_P_STDIO;
    fprintf(__io_stderr(),
            "FTN-F-STR_MALLOC  unable to allocate area of %d bytes\n", size);
    MP_V_STDIO;
    Ftn_exit(1);
  }
  q = *hdr;
  *p = (char *)q; /* link this block to the blocks already allocated */
  *hdr = p;       /* update the list pointer */
  return p + 1;
}

/* ***********************************************************************/
/** \brief
 * Utility routine to deallocate space for character expressions
 * whose lengths are known only at run-time.
 *
 * The compiler creates a variable to locate the list of blocks allocated
 * during a subprogram. This variable is initialized to NULL upon entry to
 * the subprogram.  When the subprogram exits, all of the blocks are freed.
 * Each block of space consists of n 'words':
 * - ++  first word        - pointer to the next allocated block,
 * - ++  remaining word(s) - space for the character data.
 *
 *  \param first - pointer to the compiler-created variable locating the list of
 *                 allocated blocks. Ftn_str_free traverses the list of 
 *                 allocated blocks and frees each block.
 */
/* ***********************************************************************/
void
Ftn_str_free(char **first)
{
  char **p, **next;
  /* traverse the list */
  for (p = first; p != NULL;) {
    next = (char **)(*p);
    _mp_free(p);
    p = next;
  }
}

#define __HAVE_LONGLONG_T

typedef long _LONGLONG_T;
typedef unsigned long _ULONGLONG_T;

/* ***********************************************************************/
/** \brief
 * Copies a series of character strings into another.
 * 
 * This function is used
 * to implement character assignments and concatenations. It pads with
 * blanks if the destination string is longer than the sum of the lengths
 * of the source strings and truncates if the sum of the lengths of the
 * source strings is longer than the destination string.
 *
 * Allow the target of the concatenation to appear in its right-hand side
 * which is standard f90 and is a common extension to f77.
 *
 * \param   n              number of source strings
 * \param   to             pointer to destination string
 * \param   to_len         length of destination string
 * <pre>
 * Varargs:
 *   {from           pointer to kth source string
 *   from_len}*      length of kth source string
 * </pre>
 */
/* ***********************************************************************/
void
Ftn_str_copy_klen(int n, char *to, _LONGLONG_T to_len, ...)
{
  va_list ap;
  char *from;
  _LONGLONG_T from_len;
  int idx2;
  int cnt;
  typedef struct {
    char *str;
    _LONGLONG_T len;
    int dyn;
  } SRC_STR;
  SRC_STR *src_p, *qq;
  SRC_STR aa[4]; /* statically allocate for 4 source strings */
  int src_p_allocd;
  char *to_p;
  char *to_end;
  char *from_end;
  int any_allocd;

  if (to_len <= 0) {
    return;
  }
  if (n <= (sizeof(aa) / sizeof(SRC_STR))) {
    qq = src_p = aa;
    src_p_allocd = 0;
  } else {
    qq = src_p = (SRC_STR *)_mp_malloc((size_t)sizeof(SRC_STR) * n);
    src_p_allocd = 1;
  }
  va_start(ap, to_len);
#ifdef DEBUG
  printf("to_len = %d\n", to_len);
#endif
  to_end = to - 1;
  any_allocd = idx2 = 0;
  for (cnt = n; cnt > 0; cnt--, qq++) {
    from = va_arg(ap, char *);
    from_len = va_arg(ap, _LONGLONG_T);
#ifdef DEBUG
    printf("from_len = %ld\n", from_len);
#endif
    if (from_len < 0)
      from_len = 0;
    qq->str = from;
    qq->len = from_len;
    qq->dyn = 0;
    to_end += from_len;
    from_end = from + (from_len - 1);
    if ((from >= to && from <= to_end) ||
        (from_end >= to && from_end <= to_end))
      if (from_len) {
        qq->str = _mp_malloc((size_t)from_len);
        memcpy(qq->str, from, (size_t)from_len);
        qq->dyn = 1;
#ifdef DEBUG
        printf("string %d overlaps\n", n - cnt);
        printf("mallocd %08x\n", qq->str);
#endif
        any_allocd = 1;
      }
    idx2 += from_len;
    if (idx2 >= to_len)
      break;
  }
  va_end(ap);

  qq = src_p;
  to_p = to;
  to_end = to + to_len; /* position after the end of the destination */
  for (cnt = n; cnt > 0; cnt--, qq++) {
    from = qq->str;
    for (from_len = qq->len; from_len > 0; from_len--, from++) {
      *to_p++ = *from;
      if (to_p == to_end)
        goto exit_return;
#ifdef DEBUG
      printf("from_char = %c\n", *from);
#endif
    }
  }

exit_return:
  while (to_p < to_end) { /* remember, to_end is 1 after end */
    /* blank fill to right */
    *to_p++ = ' ';
  }

  if (any_allocd) {
    idx2 = 0;
    qq = src_p;
    for (cnt = n; cnt > 0; cnt--, qq++) {
      if (qq->dyn) {
        _mp_free(qq->str);
#ifdef DEBUG
        printf("freed   %08x\n", qq->str);
#endif
      }
      idx2 += qq->len;
      if (idx2 >= to_len)
        break;
    }
  }

  if (src_p_allocd)
    _mp_free(src_p);
}

/** \brief single source, no overlap */
void
Ftn_str_cpy1_klen(char *to, _LONGLONG_T to_len, char *from, _LONGLONG_T from_len)
{
  char *to_p, *to_end;

  if (to_len <= 0) {
    return;
  }
  if (from_len < 0)
    from_len = 0;
  if (to_len <= from_len) {
    memcpy(to, from, (size_t)to_len);
    return;
  }
  memcpy(to, from, (size_t)from_len);
  /*memset(to+from_len, ' ', to_len - from_len);*/
  to_p = to + from_len;
  to_end = to + to_len;
  while (to_p < to_end) { /* remember, to_end is 1 after end */
    /* blank fill to right */
    *to_p++ = ' ';
  }
  return;
}

/* ***********************************************************************/
/** \brief
 * Implements the INDEX intrinsic; is an integer function which returns the
 * value according to the INDEX intrinsic.
 */
/* ***********************************************************************/
_LONGLONG_T Ftn_str_index_klen(a1, a2, a1_len,
                  a2_len) char *a1; /* pointer to string being searched */
char *a2;                           /* pointer to string being searched for */
_LONGLONG_T a1_len;                         /* length of a1 */
_LONGLONG_T a2_len;                         /* length of a2 */
{
  _LONGLONG_T idx1, idx2;
  int match;
  if (a1_len < 0)
    a1_len = 0;
  if (a2_len < 0)
    a2_len = 0;
  for (idx1 = 0; idx1 < a1_len; idx1++) {
    if (a2_len > (a1_len - idx1))
      return (0);
    match = TRUE;
    for (idx2 = 0; idx2 < a2_len; idx2++) {
      if (a1[idx1 + idx2] != a2[idx2]) {
        match = FALSE;
        break;
      }
    }
    if (match)
      return (idx1 + 1);
  }
  return (0);
}

/* ***********************************************************************/
/** \brief
 * Implements realational operators with string operands and the lexical
 * intrinsics. Returns integer value:
 * -  0 => strings are the same
 * - -1 => a1 lexically less than a2
 * -  1 => a1 lexically greater than a2
 * If the strings are of unequal lengths, treats shorter string as if it were
 * padded with blanks.
 */
/* ***********************************************************************/
int Ftn_strcmp_klen(a1, a2, a1_len,
               a2_len) char *a1; /* first string to be compared */
char *a2;                        /* second string to be compared */
_LONGLONG_T a1_len;                      /* length of a1 */
_LONGLONG_T a2_len;                      /* length of a2 */
{
  _LONGLONG_T idx1;
  int ret_val;
  
  if (a1_len < 0)
    a1_len = 0;
  if (a2_len < 0)
    a2_len = 0;
  if (a1_len == a2_len) {
    while (a1_len > 0) {
      if (*a1 != *a2) {
        if ((unsigned)(*a1) > (unsigned)(*a2))
          return 1;
        return -1;
      }
      ++a1;
      ++a2;
      a1_len--;
    }
    return 0;
  }
  if (a1_len > a2_len) {
    /* first compare the first a2_len characters of the strings */
    ret_val = memcmp(a1, a2, (size_t)a2_len);
    if (ret_val != 0) {
      if (ret_val < 0)
        return (-1);
      if (ret_val > 0)
        return (1);
    }
    /*
     * if the last (a1_len - a2_len) characters of a1 are blank, then the
     * strings are equal; otherwise, compare the first non-blank char. to
     * blank
     */

    for (idx1 = 0; idx1 < (a1_len - a2_len); idx1++) {
      if (a1[a2_len + idx1] != ' ') {
        if (a1[a2_len + idx1] > ' ')
          return (1);
        return (-1);
      }
    }
    return (0);
  } else {
    /* a2_len > a1_len */
    /* first compare the first a1_len characters of the strings */
    ret_val = memcmp(a1, a2, (size_t)a1_len);
    if (ret_val != 0) {
      if (ret_val < 0)
        return (-1);
      if (ret_val > 0)
        return (1);
    }
    /*
     * if the last (a2_len - a1_len) characters of a2 are blank, then the
     * strings are equal; otherwise, compare the first non-blank char. to
     * blank
     */

    for (idx1 = 0; idx1 < (a2_len - a1_len); idx1++) {
      if (a2[a1_len + idx1] != ' ') {
        if (a2[a1_len + idx1] > ' ')
          return (-1);
        return (1);
      }
    }
    return (0);
  }
}

/* ***********************************************************************/
/** \brief
 * Utility routine to allocate space for character expressions
 * whose lengths are known only at run-time.
 *
 * The compiler creates a variable to locate the list of blocks allocated
 * during a subprogram. This variable is initialized to NULL upon entry to
 * the subprogram.  When the subprogram exits, all of the blocks are freed.
 * Each block of space consists of n 'words':
 * - ++  first word        - pointer to the next allocated block,
 * - ++  remaining word(s) - space for the character data.
 *
 * \param     size - number of bytes needed,
 * \param     hdr  - pointer to the compiler-created variable locating the
 *             list of allocated blocks. Ftn_str_malloc updates this
 *             variable.
 * \return  returns a pointer to the space after the 'next pointer'.
 *
 * void  Ftn_str_free(char ***hdr)
 *      hdr  - pointer to the compiler-created variable locating the list of
 *             allocated blocks. Ftn_str_free traverses the list of allocated
 *             blocks and frees each block.
 *
 * Note that KANJI versions are unneeded since the compiler just calls
 * Ftn_str_malloc() with an adjusted length.
 */
/* ***********************************************************************/
char **
Ftn_str_malloc_klen(_LONGLONG_T size, char ***hdr)
{
  _LONGLONG_T nbytes;
  char **p, **q;

/*
 * round request to the size of a pointer & also accommodate a 'next'
 * pointer
 */
#define PTRSZ sizeof(char *)
  nbytes = ((size + PTRSZ - 1) / PTRSZ) * PTRSZ + PTRSZ;
  p = (char **)_mp_malloc((size_t)nbytes);
  if (p == NULL) {
    MP_P_STDIO;
    fprintf(__io_stderr(),
            "FTN-F-STR_MALLOC  unable to allocate area of %ld bytes\n", size);
    MP_V_STDIO;
    Ftn_exit(1);
  }
  q = *hdr;
  *p = (char *)q; /* link this block to the blocks already allocated */
  *hdr = p;       /* update the list pointer */
  return p + 1;
}
