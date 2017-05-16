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
 *  \brief NME data structures and definitions
 */

#ifndef NME_H

typedef struct {
  char type;   /* One of the following NT_ defs. */
  char inlarr; /* nonzero if an inlined array ref; else 0 */
  char pd1;
  char pd2;
  int stl;       /* STL item pointer: rsvd for invariant */
  int hshlnk;    /* link of names w/ identical hash val */
  int nm;        /* Dependent on type. */
  int sym;       /* Dependent on type. */
  int rfptr;     /* Dependent on type. */
  int exp_loop;  /* Dependent on type. */
  int f6;        /* Dependent on type. */
  ISZ_T cnst;    /* Dependent on type. */
  int sub;       /* subscript info (0 if NA or not array) */
  int f13;       /* dependent on type */
  int base;      /* base nme */
  int pte;       /* pointer target entry (pte) index */
  int rpct_loop; /* If >0, the index of a loop which is guarded by
                  *   runtime pointer conflict tests (RPCTs).
                  *   The nme is only used within this loop. */
  union {
    struct {
      unsigned ovs : 1; /* over-subscripted -- more subscripts than rank; can
                      * occur when inlining
                      */
    } bits;
    UINT16 all; /* important flags */
  } flags;
  union {
    struct {
      unsigned m1 : 1;
      unsigned m2 : 1;
      unsigned m3 : 1;
      unsigned m4 : 1;
      unsigned m5 : 1;
      unsigned m6 : 1;
      unsigned m7 : 1;
      unsigned m8 : 1;
    } attr;
    UINT16 all;
  } mask; /* temporary attribute bits */
} NME;

typedef struct {
  int next;   /* next pointer target */
  int type;   /* pointer target type */
  int sym;    /* type-specific value */
  int val;    /* type-specific value */
  int hshlnk; /* link of PTE with name hash val */
} PTE;

/* RPCT (runtime pointer conflict test) struct:
 * The NMEs 'nme1' and 'nme2' in an RPCT record are "RPCT NMEs" that
 * are created by 'add_rpct_nme()', and the RPCT record itself is
 * created by 'add_rpct( nme1, nme2 )'.  The existence of such a
 * record indicates that the generated code contains a test which
 * proves at runtime that this pair of references do not conflict, so
 * 'conflict( nme1, nme2 )' will return NOCONFLICT.
 */
typedef struct {
  int nme1;
  int nme2;
  int hshlnk;
} RPCT;

typedef struct {
  STG_MEMBERS(NME);
  STG_DECLARE(pte, PTE);
  STG_DECLARE(rpct, RPCT);
} NMEB;

#define NME_LAST nmeb.stg_avail
#define PTE_LAST nmeb.pte.stg_avail
#define RPCT_LAST nmeb.rpct.stg_avail

#if DEBUG
#define NMECHECK(i)                                                        \
  (((i) < 0 || (i) >= nmeb.stg_avail) ? (interr("bad nme index", i, 3), i) \
                                      : (i))
#else
#define NMECHECK(i) i
#endif
#define NME_TYPE(i) nmeb.stg_base[NMECHECK(i)].type
#define NME_INLARR(i) nmeb.stg_base[NMECHECK(i)].inlarr
#define NME_SYM(i) nmeb.stg_base[NMECHECK(i)].sym
#define NME_NM(i) nmeb.stg_base[NMECHECK(i)].nm
#define NME_HSHLNK(i) nmeb.stg_base[NMECHECK(i)].hshlnk
#define NME_RFPTR(i) nmeb.stg_base[NMECHECK(i)].rfptr
#define NME_ELOOP(i) nmeb.stg_base[NMECHECK(i)].exp_loop
#define NME_RAT(i) nmeb.stg_base[NMECHECK(i)].rfptr
#define NME_DEF(i) nmeb.stg_base[NMECHECK(i)].f6
#define NME_CNST(i) nmeb.stg_base[NMECHECK(i)].cnst
#define NME_SUB(i) nmeb.stg_base[NMECHECK(i)].sub
#define NME_STL(i) nmeb.stg_base[NMECHECK(i)].stl
#define NME_CNT(i) nmeb.stg_base[NMECHECK(i)].f13
#define NME_USES(i) nmeb.stg_base[NMECHECK(i)].f13
#define NME_BASE(i) nmeb.stg_base[NMECHECK(i)].base
#define NME_PTE(i) nmeb.stg_base[NMECHECK(i)].pte
#define NME_RPCT_LOOP(i) nmeb.stg_base[NMECHECK(i)].rpct_loop

#define NME_FLAGS(i) nmeb.stg_base[NMECHECK(i)].flags.all
#define NME_OVS(i) nmeb.stg_base[NMECHECK(i)].flags.bits.ovs
#define NME_TEMP(i) nmeb.stg_base[NMECHECK(i)].mask.attr.m1
#define NME_MASK(i) nmeb.stg_base[NMECHECK(i)].mask.all

/*
 * The following two special names entries are referenced during
 * expansion.  addnme never sees NME_VOL.
 */
#define NME_UNK 0
#define NME_VOL 1

typedef enum NT_KIND {
  NT_INDARR =
      -2, /* for C/C++, only used by expand; element of a pointer deref */
  NT_ADD = -1,
  NT_UNK = 0, /* Unknown ref. e.g.  *(f()) */
  NT_IND = 1, /* Indirect ref e.g. *p      */
  NT_VAR = 2, /* Variable ref. (struct, array or scalar) */
  NT_MEM = 3, /* Structure member ref. */
  NT_ARR = 4, /* Array element ref. */
  NT_SAFE = 5 /* special names; does not conflict with preceding refs */
} NT_KIND;

/* Some files check if NT_INDARR is defined. */
#define NT_INDARR NT_INDARR

/*
 * Zero means unknown PTE information
 * may be used as PTE index or PTE type.
 */

#define PT_UNK 0
#define PTE_UNK 0
#define PTE_END -1

/*
 * PSYM: precise symbol
 * ISYM: imprecise symbol
 * ANON: anonymous variable
 * GDYN: dynamically allocated memory
 * LDYN: dynamically allocated memory allocated during this function call
 * NLOC: any nonlocal memory (from calling environment)
 */
#define PT_PSYM 1
#define PT_ISYM 2
#define PT_ANON 3
#define PT_GDYN 4
#define PT_LDYN 5
#define PT_NLOC 6

#if DEBUG
#define PTECHECK(i)                            \
  (((i) < 0 || (i) >= nmeb.pte.stg_avail)      \
       ? (interr("bad pte index", i, 3), i, i) \
       : (i))
#else
#define PTECHECK(i) i
#endif
#define PTE_NEXT(i) nmeb.pte.stg_base[PTECHECK(i)].next
#define PTE_TYPE(i) nmeb.pte.stg_base[PTECHECK(i)].type
#define PTE_SPTR(i) nmeb.pte.stg_base[PTECHECK(i)].sym
#define PTE_VAL(i) nmeb.pte.stg_base[PTECHECK(i)].val
#define PTE_HSHLNK(i) nmeb.pte.stg_base[PTECHECK(i)].hshlnk

#if DEBUG
#define RPCT_CHECK(i)                        \
  (((i) < 1 || (i) >= nmeb.rpct.stg_avail)   \
       ? (interr("bad rpct index", i, 3), i) \
       : (i))
#else
#define RPCT_CHECK(i) i
#endif
#define RPCT_NME1(i) nmeb.rpct.stg_base[RPCT_CHECK(i)].nme1
#define RPCT_NME2(i) nmeb.rpct.stg_base[RPCT_CHECK(i)].nme2
#define RPCT_HSHLNK(i) nmeb.rpct.stg_base[RPCT_CHECK(i)].hshlnk

/***** External Functions *****/

extern int add_arrnme(NT_KIND, SPTR, int, ISZ_T, int, LOGICAL);
extern int add_rpct_nme(int orig_nme, int rpct_loop);
extern int addnme(NT_KIND, int, int, ISZ_T);
extern int basesym_of(int);
extern int basenme_of(int);
extern int zbasenme_of(int);
extern void add_rpct(int rpct_nme1, int rpct_nme2);
extern LOGICAL is_presym(int);
extern void loc_of(int);
extern void loc_of_vol(int);
extern DTYPE dt_nme(int);

extern int lookupnme(NT_KIND type, int insym, int nm, ISZ_T cnst);

int print_nme(int nme);
int __print_nme(FILE *ff, int nme);
extern int build_sym_nme(int sym, int offset, LOGICAL ptr_mem_op);
extern LOGICAL basenme_is_static(int);
extern void dumpname(int);
void dmpnme(void);
int hlconflict(int nm1, int nm2);
void nme_init(void);
void nme_end(void);
int addpte(int type, SPTR sptr, int val, int next);
void ptrstore_of(int nme);

/***** External Data Declarations *****/

extern NMEB nmeb;

#define NME_H
#endif
