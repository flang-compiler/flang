/*
 * Copyright (c) 2016-2017, NVIDIA CORPORATION.  All rights reserved.
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

/** \file main.h
    \brief data definitions, macros, and prototypes for fe90/main.c
*/

#ifdef INSIDE_MAIN

/*
 * structure/constants needed for command line processing:
 */

struct cmdTable {/* command-line switch table used in main.c */
  char *cmd;
  INT caselabel;
  LOGICAL no;
};

enum cmdLineSwitches {
  SW_ASM = 1,
  SW_DCLCHK,
  SW_DEBUG,
  SW_DEPCHK,
  SW_DLINES,
  SW_EXTEND,
  SW_I4,
  SW_IDIR,
  SW_INFORM,
  SW_LINE,
  SW_LIST,
  SW_OBJECT,
  SW_ONETRIP,
  SW_OPT,
  SW_PROFILE,
  SW_Q,
  SW_SAVE,
  SW_SHOW,
  SW_STANDARD,
  SW_SYMBOL,
  SW_UPCASE,
  SW_XOFF,
  SW_XON,
  SW_NOXO,
  SW_DALIGN,
  SW_ASTYPE,
  SW_RECURS,
  SW_IEEE,
  SW_INSIZE,
  SW_VECT,
  SW_ENDIAN,
  SW_TERSE,
  SW_INLIB,
  SW_INFUNC,
  SW_EXLIB,
  SW_EXSIZE,
  SW_EXFUNC,
  SW_DOLLAR,
  SW_X,
  SW_QUAD,
  SW_REENTR,
  SW_ANNO,
  SW_ALPHA,
  SW_BETA,
  SW_Y,
  SW_FN,
  SW_QA,
  SW_ES,
  SW_P,
  SW_DEF,
  SW_UNDEF,
  SW_STDINC,
  SW_VERSION,
  SW_VH,
  SW_VN,
  SW_VT,
  SW_PGVFUNC,
  SW_PGVFILE,
  SW_HPF,
  SW_FREEFORM,
  SW_OUTPUT,
  SW_SEQUENCE,
  SW_PREPROC,
  SW_IPA,
  SW_CRAFT,
  SW_QQ,
  SW_STATIC_ANALYSIS,
  SW_NOSTATIC,
  SW_DFILE,
  SW_QFILE,
  SW_ILM,
  SW_ERRLIMIT,
  SW_FCON,
  SW_MP,
  SW_MODDIR,
  SW_IPACOMP,
  SW_IPACOMPSW,
  SW_IPAAS,
  SW_IPAASSW,
  SW_IPAIMPORT,
  SW_IPAEXPORT,
  SW_IPAFILE,
  SW_IPOFILE,
  SW_SF,
  SW_SOURCE,
  SW_IPXIMPORT,
  SW_MODEXPORT,
  SW_MODINDEX,
  SW_CCFF,
  SW_CONCUR,
  SW_CUDAROOT,
  SW_TP,
  SW_CUDAVER,
  SW_ACCEL,
  SW_STBFILE,
  SW_CMDLINE
};

#endif
