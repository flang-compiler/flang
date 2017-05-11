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
    \brief Data definitions for Fortran front-end data structures.
 */

#include "gbldefs.h"
#include "global.h"
#include "symtab.h"
#include "semant.h"
#include "soc.h"
#include "scan.h"
#include "semstk.h"
#include "flgdf.h"

GBL gbl;

SEM sem;

SST *sst = NULL;

SCN scn;

SOC soc;

SWEL *switch_base;

AUX aux;

/** \brief We only allow casting to and from TY_WORD and TY_DWORD.  Therefore,
   you
    index into the following table as follows
    cast_types[dtype][{DT_WORD or DT_DWORD} - 1][0 for from or 1 for to]
    Entries are:
    -  0  -  casting unnecessary
    -  1  -  casting necessary
    - -1  -  casting not allowed
 */
INT cast_types[NTYPE][2][2] = {
    /* DT_NONE */ {{0, 0}, {0, 0}},
    /* DT_WORD */ {{0, 0}, {1, 1}},
    /* DT_DWORD */ {{1, 1}, {0, 0}},
    /* DT_HOLL */ {{1, -1}, {1, -1}},
    /* DT_BINT */ {{1, 1}, {1, 1}},
    /* DT_SINT */ {{1, 1}, {1, 1}},
    /* DT_INT */ {{1, 1}, {1, 1}},
    /* DT_INT8 */ {{1, 1}, {1, 1}},
    /* DT_REAL */ {{1, 1}, {1, 1}},
    /* DT_DBLE */ {{1, 1}, {1, 1}},
    /* DT_QUAD */ {{-1, -1}, {-1, -1}},
    /* DT_CMPLX */ {{-1, -1}, {1, -1}},
    /* DT_DCMPLX */ {{-1, -1}, {1, -1}},
    /* DT_QCMPLX */ {{-1, -1}, {-1, -1}},
    /* DT_BLOG */ {{1, 1}, {1, 1}},
    /* DT_SLOG */ {{1, 1}, {1, 1}},
    /* DT_LOG */ {{1, 1}, {1, 1}},
    /* DT_LOG8 */ {{1, 1}, {1, 1}},
    /* DT_ADDR */ {{-1, -1}, {-1, -1}},
    /* DT_CHAR */ {{-1, -1}, {-1, -1}},
    /* DT_NCHAR */ {{-1, -1}, {-1, -1}},
};
