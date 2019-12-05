/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

typedef struct {
  char *lang;    /* language */
  char *host;    /* host */
  char *vsn;     /* version number */
  char *bld;     /* build number */
  char *dvsn;    /* date-based version number */
  char *target;  /* target compiler */
  char *product; /* product designation */
  const char *copyright;
} VERSION;

extern VERSION version;

const char *get_version_string(void);

