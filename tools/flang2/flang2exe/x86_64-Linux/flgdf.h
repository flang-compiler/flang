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
 * \brief Data definitions for FTN compiler flags
 */

FLG flg = {
    FALSE,      /* asm = -noasm */
    FALSE,      /* list = -nolist  */
    TRUE,       /* object = -object */
    FALSE,      /* xref =   -show noxref */
    FALSE,      /* code =   -show nocode */
    FALSE,      /* include = -show noinclude */
    0,          /* debug = -nodebug */
    1,          /* opt  = -opt 1    */
    TRUE,       /* depchk = -depchk on */
    FALSE,      /* depwarn = -depchk warn */
    FALSE,      /* dclchk = -nodclchk */
    FALSE,      /* locchk = -nolocchk  */
    FALSE,      /* onetrip = -noonetrip */
    FALSE,      /* save =  -nosave     */
    1,          /* inform = -inform informational */
    0xFFFFFFFF, /* xoff */
    0x00000000, /* xon  */
    FALSE,      /* ucase = -noucase */
    NULL,       /* idir == empty list */
    NULL,       /* linker_directives == empty list */
    NULL,       /* llvm_target_triple == empty ptr */
    FALSE,      /* dlines = -nodlines */
    72,         /* extend_source = -noextend_source */
    TRUE,       /* i4 = -i4 */
    FALSE,      /* line = -noline */
    FALSE,      /* symbol = -nosymbol */
    0,          /* profile = no profiling */
    FALSE,      /* standard = don't flag non-F77 uses */
    {0},        /* dbg[]  */
    TRUE,       /* align doubles on doubleword boundary */
    0,          /* astype - assembler syntax - 0-elf, 1-coff */
    FALSE,      /* recursive = -norecursive */
    0,          /* ieee: 0 == none:   num == bit value for
                        item (fdiv==1,ddiv==2) */
    0,          /* inline: 0 == none: num == max # ilms */
    0,          /* autoinline */
    0,          /* vect: 0 = none:    num == vect item */
    0,          /* little endian */
    FALSE,      /* not terse for summary, etc. */
    '_',        /* default is to change '$' to '_' */
    {0},        /*  x flags  */
    FALSE,      /*  don't quad align "unconstrained objects";
                        use natural alignment */
    FALSE,      /* anno - don't annotate asm file */
    FALSE,      /*  qa = -noqa */
    FALSE,      /* es = -noes */
    FALSE,      /* p = preprocessor does not emit # lines in its output */
    0,          /*  def ptr */
    NULL,       /*  search the standard include */
    FALSE,      /* don't allow smp directives */
    25,         /* errorlimit */
    FALSE,      /* trans_inv */
#ifdef TARGET_X86
    0,                                     /* tpcount */
    0,          0, 0, 0, 0, 0, 0, 0, 0, 0, /* tpvalue */
#endif
};
