/*
 * Copyright (c) 2007-2018, NVIDIA CORPORATION.  All rights reserved.
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

#ifndef CCFFINFO_H_
#define CCFFINFO_H_

/**
   \file ccffinfo.h
   \brief function prototypes and macros for ccffinfo.
   Function prototypes and macros for common compiler feedback format module
 */

#define CCFFVERSION "0.9"

#include <stdarg.h>

 void *ccff_bih_info(int msgtype, char *msgid, int bihx,
                           const char *message, ...);

 void *subccff_bih_info(void *xparent, int msgtype, char *msgid, int bihx,
                              const char *message, ...);

 void *subccff_ilt_info(void *xparent, int msgtype, char *msgid, int iltx,
                              int bihx, const char *message, ...);

 void *ccff_ilt_info(int msgtype, char *msgid, int iltx, int bihx,
                           const char *message, ...);

void ccff_init_f90(void);
void ipa_report(void); /* ipa.c */

/*
 * message type, low bit means 'neg'
 */
#define MSGINLINER 0x02
#define MSGNEGINLINER 0x03
#define MSGLOOP 0x04
#define MSGNEGLOOP 0x05
#define MSGLRE 0x06
#define MSGNEGLRE 0x07
#define MSGINTENSITY 0x08
#define MSGIPA 0x0a
#define MSGNEGIPA 0x0b
#define MSGFUSE 0x0c
#define MSGNEGFUSE 0x0d
#define MSGVECT 0x0e
#define MSGNEGVECT 0x0f
#define MSGOPENMP 0x10
#define MSGOPT 0x12
#define MSGNEGOPT 0x13
#define MSGPREFETCH 0x14
#define MSGFTN 0x16
#define MSGPAR 0x18
#define MSGNEGPAR 0x19
#define MSGHPF 0x1a
#define MSGPFO 0x1c
#define MSGNEGPFO 0x1d
#define MSGACCEL 0x1e
#define MSGNEGACCEL 0x1f
#define MSGUNIFIED 0x20
#define MSGCVECT 0x22
#define MSGNEGCVECT 0x23
#define MSGOMPACCEL 0x24

int addfile(char *filename, char *funcname, int tag, int flags, int lineno,
            int srcline, int level);

/**
   \brief ...
 */
int addinlfile(char *filename, char *funcname, int tag, int flags, int lineno, int srcline, int level, int parent);

/**
   \brief ...
 */
int subfih(int fihindex, int tag, int flags, int lineno);

/**
   \brief ...
 */
void ccff_build(char *options, char *language);

/**
   \brief ...
 */
void ccff_cleanup_children_deferred(void);

/**
   \brief ...
 */
void ccff_close_unit_deferred(void);

/**
   \brief ...
 */
void ccff_close_unit(void);

/**
   \brief ...
 */
void ccff_close(void);

/**
   \brief ...
 */
void *ccff_func_info(int msgtype, char *msgid, char *funcname,
                     const char *message, ...);

/**
   \brief ...
 */
void *ccff_info(int msgtype, char *msgid, int fihx, int lineno, const char *message, ...);

/**
   \brief ...
 */
void *_ccff_info(int msgtype, char *msgid, int fihx, int lineno,
                 const char *varname, const char *funcname,
                 const void *xparent, const char *message, va_list argptr);

/**
   \brief ...
 */
void ccff_open(char *ccff_filename, char *srcfile);

/**
   \brief ...
 */
void ccff_open_unit_deferred(void);

/**
   \brief ...
 */
void ccff_open_unit(void);

/**
   \brief ...
 */
void ccff_seq(int seq);

/**
   \brief ...
 */
void *ccff_var_info(int msgtype, char *msgid, char *varname, const char *message, ...);

/**
   \brief ...
 */
void dumpmessagelist(int nmessages);

/**
   \brief ...
 */
void fih_fini(void);

/**
   \brief ...
 */
void print_fih(void);

/**
   \brief ...
 */
void print_ifih(void);

/**
   \brief ...
 */
void restore_ccff_mark(void);

/**
   \brief ...
 */
void save_ccff_arg(char *argname, char *argvalue);

/**
   \brief ...
 */
void save_ccff_mark(void);

/**
   \brief ...
 */
void save_ccff_msg(int msgtype, char *msgid, int fihx, int lineno,
                   const char *varname, const char *funcname);

/**
   \brief ...
 */
void save_ccff_text(char *message);

/**
   \brief ...
 */
void set_allfiles(int save);

/**
   \brief ...
 */
void setfile(int f, char *funcname, int tag);

/**
   \brief ...
 */
void *subccff_info(void *xparent, int msgtype, char *msgid, int fihx,
                   int lineno, const char *message, ...);

#endif
