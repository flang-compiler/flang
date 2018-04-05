/*
 * Copyright (c) 1997-2018, NVIDIA CORPORATION.  All rights reserved.
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
 * \brief (Fortran) declarations needed to use dinitutil.c module.
 */

typedef struct {/* dinit file record */
  int dtype;    /*  also sptr  */
  ISZ_T conval; /*  also offset */
} DREC;

#define DINIT_ENDFILE -96
#define DINIT_STARTARY -95
#define DINIT_ENDARY -94
#define DINIT_TYPEDEF -93
#define DINIT_ENDTYPE -92
#define DINIT_LOC -99
#define DINIT_SLOC -91
#define DINIT_REPEAT -88
#define DINIT_SECT -87 /* conval field is sptr to string with section name */
#define DINIT_DATASECT -86 /* return to data section */
#define DINIT_OFFSET -77
#define DINIT_LABEL -33
#define DINIT_ZEROES -66
#define DINIT_VPUINSTR -55 /* sparc/VPU compiler only */
#define DINIT_COMMENT                           \
  -44 /* comment string for asm file - the      \
       * DREC.conval field is an int index into \
       * the getitem_p table (salloc.c) which   \
       * contains the pointer to the string.    \
       */
#define DINIT_FILL -59
#define DINIT_MASK -60
#define DINIT_ZEROINIT -61 /* llvm : use zeroinitializer */

#define DINIT_FUNCCOUNT -31 /* gbl.func_count value */
#define DINIT_STRING -30    /* holds string initialization, length given */

void dinit_init(void);
void dinit_put(int, ISZ_T);
DREC *dinit_read(void);
void dinit_read_string(ISZ_T, char *);
void dinit_put_string(ISZ_T, char *);
void dinit_fseek(long);
void dinit_fskip(long);
void dinit_save(void);
void dinit_restore(void);
long dinit_ftell(void);
void dinit_end(void);
void do_dinit(void);
void dinit_save(void);
void dinit_restore(void);
bool dinit_ok(int);
void dmpilms(void);
void dumpilms(void);
