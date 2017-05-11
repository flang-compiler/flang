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
 * \brief Hammer Code Generator - AILI Scheduling module.
 *
 * Define datatypes, functions, macros shared by cgsched.c and the actual
 * Scheduler: sched-dag.c.
 */

typedef struct {            /* aili in the current segment to be scheduled */
  AILI *ai;                 /* pointer to the actual aili */
  struct pritem *pred_list; /* predecessor list for this aili */
  struct pritem *succ_list; /* successor list */
  int next_scheduled_aili;  /* links scheduled aili */
  int sequence; /* indicates whether this ANODE is a member of a sequence:
                  == 0 - not a member of a sequence
                   > 0 - this is the head of a sequence(num elements in
                  sequence)
                   < 0 - distance to head of sequence (-1, -2, -3,...)
               */

  /* -------------- fields local to the Scheduler: --------------- */
  void *pInst; /* Pointer to schedulers representation */
  int delay;   /* Delay from the start of the block */
  int early, late;

} ANODE;

typedef enum DEPTYPE {
  DT_REGUSE = 1,
  DT_LDST = 2,
  DT_CC = 3,
  DT_DOM = 4,
  DT_FENCE = 5
} DEPTYPE;

typedef struct pritem {/* item in a predecessor or successor list */
  int ainum;           /* index into table of ANODES */
  DEPTYPE deptype;     /* type of dependecy: */
  struct pritem *next; /* next item in pred or succ list */

  /* -------------- fields local to the Scheduler: --------------- */
} PRITEM;

typedef struct {
  int bih;                  /* BIH to which the AILI segment is associated */
  ANODE *ailis;             /* table of ANODES for the current segment of AILI
                               to be scheduled */
  int aili_count;           /* number of aili in the current segment */
  int first_scheduled_aili; /*  Index into the ANODE table.  Set by Scheduler
      to indicate the first aili in the new, scheduled order.  */
} SCH;

extern int schedule_dag(SCH *pSch); /* sched-dag.c */
