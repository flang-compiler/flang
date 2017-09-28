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

/** \file   scan.h
    \brief  data declarations for those items which are set by
            the scanner for use by the parser or semantic analyzer.
*/

typedef struct {
  int stmtyp;
  int currlab;
  INT labno;
  LOGICAL end_program_unit; /* end of program unit seen */
  LOGICAL is_hpf;           /* true if current statement began with the
                             * '!hpf$' prefix.
                             */
  LOGICAL multiple_stmts;   /* stmts separated by ';' */
  char *directive;          /* malloc'd area containing a directive string
                             * to be passed thru as a comment string.  The
                             * string includes the the necessary prefix.
                             */
  char *options;            /* malloc'd area containing the string after
                             * 'options' in the options statement.
                             */
  struct {
    char *name;
    int avl;
    int size;
  } id;
} SCN;

/* File Records:
 *
 * Each record in the ast source file (astb.astfil) begins with a
 * 4-byte type field.  In most cases, the remaining portion of the
 * field is textual information in the form of a line (terminated by
 * '\n'.
 */
typedef enum {
  FR_SRC = -1,
  FR_B_INCL = -2,
  FR_E_INCL = -3,
  FR_END = -4,
  FR_LINENO = -5,
  FR_PRAGMA = -6,
  FR_STMT = -7,
  FR_B_HDR = -8,
  FR_E_HDR = -9,
  FR_LABEL = -98,
  FR_TOKEN = -99
} FR_TYPE;

extern SCN scn;

void scan_init(FILE *);
void scan_reset(void);
void scan_fini(void);
int get_token(INT *);
void scan_include(char *);
void scan_opt_restore(void);
int get_named_stmtyp(void);
int getCurrColumn(void);
void scan_options(void);
void fe_save_state(void);
void fe_init(void);
void fe_restart(void);
char * get_src_line(int line, char **src_file, int col, int *srcCol, 
                    int *contNo);

LOGICAL is_executable(int); /* parser.c */
void parser(void);          /* parser.c */
