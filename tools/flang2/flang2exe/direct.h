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

/**
   \file
   \brief directive/pragma data structures.
 */

typedef struct {
  /* NOTES:
   * 1.  all members must be int
   * 2.  any additions/modifications imply load_dirset() and store_dirset()
   *     in direct.c, and set_flg() in pragma.c, must be modified.
   * 3.  set_flg() cares about the order in which the members occur.
   * 4.  the member x must be the last member in this structure.
   *     DIRSET_XFLAG is x's offset (in units of ints) from the beginning
   *     of the structure.
   */
  int opt;
  int vect;
  int depchk;
  int fcon;   /* C-only, but always declared */
  int single; /* C-only, but always declared */
  int tpvalue[TPNVERSION]; /* target processor(s), for unified binary */
  int x[sizeof(flg.x) / sizeof(int)]; /* same as flg.x[...] */
} DIRSET;

#define DIRSET_XFLAG 15

typedef struct lpprg_ {/* pragma information for loops */
  int beg_line;        /* beginning line # of loop */
  int end_line;        /* ending line # of loop */
  DIRSET dirset;       /* dirset for the loop */
} LPPRG;

typedef struct {/* loop pragma stack */
  int dirx;     /* index into lpg of the loop's dirset */
} LPG_STK;

/** \brief Directive structure
 */
typedef struct {
  DIRSET gbl;        /**< Holding area for global-scoped pragmas */
  DIRSET rou;        /**< Holding area for routine-scoped pragmas */
  DIRSET loop;       /**< Holding area for loop-scoped pragmas */
  DIRSET rou_begin;  /**< Pragmas which apply to the beginning of a routine.
                       *  For C, this structure must be saved away for each
                       *  function appearing in the source file.
                       */
  LOGICAL loop_flag; /**< Seen pragma with loop scope */
  LOGICAL in_loop;   /**< Currently in loop with pragmas */
  LOGICAL carry_fwd; /**< If global/routine pragma seen, must be carried
                      * forward to all outer loops which follow in the
                      * routine.
                      */
  /**
   * for C, need to allocate a DIRSET for each function -- is located
   * by the function's ENTRY aux structure and is assigned by dir_rou_end().
   *
   * for C & Fortran, need to allocate a DIRSET for a loop which has
   * pragmas associated with it.
   */
  DIRSET *stgb;
  int size;
  int avail;
  /**
   * for C & Fortran, each function is associated with a set of
   * loop pragma information. The set is organized as a table
   * and will be ordered according to occurrence of loops (with
   * associated pragmas) in the function.
   */
  struct {
    LPPRG *stgb;
    int size;
    int avail;
  } lpg;
  struct {
    LPG_STK *stgb;
    int size;
    int top;
  } lpg_stk;
} DIRECT;

extern DIRECT direct;

void direct_init(void); /* direct.c */
int direct_import(FILE *);
void direct_rou_end(void);
void direct_loop_enter(void);
void direct_loop_end(int, int);
void direct_rou_load(int);
void direct_rou_setopt(int, int);
void load_dirset(DIRSET *);  /* utility to load a dirset into flg */
void store_dirset(DIRSET *); /* store flg into a dirset */
void dirset_options(LOGICAL);

void push_lpprg(int);

void ili_lpprg_init(void); /* ilidir.c */
void open_pragma(int);
void close_pragma(void);
void push_pragma(int);
void pop_pragma(void);
void direct_fini(void);
void direct_export(FILE *ff);
void direct_xf(char *fn, int x, int v);
void direct_yf(char *fn, int x, int v);
