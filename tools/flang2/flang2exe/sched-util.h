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
 */
#ifndef _SCHED_UTIL_H_
#define _SCHED_UTIL_H_

/** Schedule states */
typedef enum { SS_None, SS_Sched = 1, SS_Ready, SS_Cand } STATE;

typedef struct tagINST {

  int cyc;
  int ainum; /* Index into table of ANODES */
  int rcnt;  /* Count of successors in Cands queue */
  STATE state;
  ANODE *pAnode;

  struct tagINST *prev;
  struct tagINST *next;
} INST;

typedef struct tagINST_QUEUE {
  INST *pInstrs;
  INST *pTail;
  int count; /* Count of elements in queue */
  int size;  /* Memory size of queue */
} INST_QUEUE;

extern int is_pred_leaf(ANODE *);
extern int is_succ_leaf(ANODE *);

void iq_insert(INST *pInst, INST_QUEUE *pIQ);

/* Instruction management */
extern INST *new_inst(int, SCH *);
extern void free_inst(INST *);

/* Instruction queue management */
extern INST_QUEUE *iq_new();
extern void iq_free(INST_QUEUE *);
extern void iq_cons(INST *, INST_QUEUE *);
extern void iq_append(INST *, INST_QUEUE *);
extern int iq_isempty(INST_QUEUE *);
extern INST *iq_head(INST_QUEUE *);
extern void iq_remove(INST *, INST_QUEUE *);

#endif
