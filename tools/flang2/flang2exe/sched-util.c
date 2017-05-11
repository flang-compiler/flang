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
#include "gbldefs.h"
#include "global.h"
#include "symtab.h"
#include "ili.h"
#include "machreg.h"
#include "cg.h"
#include "cgsched.h"
#include "sched-util.h"

int
is_pred_leaf(ANODE *pAnode)
{
  PRITEM *pred;
  int flag;

  flag = TRUE;
  pred = pAnode->pred_list;
  while (pred) {
    if (pred->ainum != 0) {
      flag = FALSE;
    }
    pred = pred->next;
  }
  return flag;
}

int
is_succ_leaf(ANODE *pAnode)
{
  PRITEM *succ;
  int flag;

  flag = TRUE;
  succ = pAnode->succ_list;
  while (succ) {
    if (succ->ainum != 0) {
      flag = FALSE;
    }
    succ = succ->next;
  }
  return flag;
}

int
is_load(INST *pInst)
{
  int opc;
  AILI *pAi;
  OPRND *op1, *op2, *dest;

  pAi = pInst->pAnode->ai;
  opc = pAi->opc;
  op1 = pAi->src1;
  op2 = pAi->src2;
  dest = pAi->dest;

  return op1 && (op1->opkind == OP_ADDR || op1->opkind == OP_SPTR) &&
         op2 == NULL && dest &&
         (dest->opkind == OP_REG || dest->opkind == OP_REGPAIR ||
          dest->opkind == OP_X87);
}

int
is_store(INST *pInst)
{
  int opc;
  AILI *pAi;
  OPRND *op1, *op2, *dest;

  pAi = pInst->pAnode->ai;
  opc = pAi->opc;
  op1 = pAi->src1;
  op2 = pAi->src2;
  dest = pAi->dest;

  return dest && (dest->opkind == OP_SPTR || dest->opkind == OP_ADDR) &&
         op2 == NULL && op1 &&
         (op1->opkind == OP_REG || op1->opkind == OP_REGPAIR ||
          op1->opkind == OP_X87);
}

int
is_rm(INST *pInst)
{
  int opc;
  AILI *pAi;
  OPRND *op1, *op2, *dest;

  pAi = pInst->pAnode->ai;
  opc = pAi->opc;
  op1 = pAi->src1;
  op2 = pAi->src2;
  dest = pAi->dest;

  if ((op1 != NULL && op2 != NULL) &&
      (((op1->opkind == OP_REG || op1->opkind == OP_REGPAIR) &&
        (op2->opkind == OP_ADDR)) ||
       ((op2->opkind == OP_REG || op2->opkind == OP_REGPAIR) &&
        (op1->opkind == OP_ADDR)))) {
    return TRUE;
  }
  return FALSE;
}

/* Heaps */

INST *
new_inst(int ainum, SCH *pSch)
{
  INST *pNew;

  NEW(pNew, INST, 1);
  BZERO(pNew, INST, 1);

  pNew->ainum = ainum;
  pNew->pAnode = &(pSch->ailis[ainum]);
  pNew->rcnt = 0;

  pSch->ailis[ainum].pInst = pNew;

  return pNew;
}

void
free_inst(INST *pInst)
{
}

INST_QUEUE *
iq_new()
{
  INST_QUEUE *pNew;

  NEW(pNew, INST_QUEUE, 1);

  pNew->count = 0;
  pNew->size = 0;

  return pNew;
}

void
iq_free(INST_QUEUE *pQueue)
{
  FREE(pQueue->pInstrs);
  pQueue->count = 0;
  pQueue->size = 0;
}

int
iq_count(INST_QUEUE *pQ)
{
  return pQ->count;
}

int
iq_isempty(INST_QUEUE *pIQ)
{
  if (pIQ->pInstrs == NULL) {
    return 1;
  }
  return 0;
}

void
iq_cons(INST *pInst, INST_QUEUE *pIQ)
{
  if (pIQ->pInstrs != NULL) {
    pIQ->pInstrs->prev = pInst;
    pInst->prev = NULL;
    pInst->next = pIQ->pInstrs;
  }
  pIQ->pInstrs = pInst;
  pIQ->count += 1;
}

/**
   \brief Insert pInst into instruction queue.
 */
void
iq_insert(INST *pInst, INST_QUEUE *pIQ)
{
  INST *pTmp;

  if (pIQ->pInstrs == NULL) {
    pIQ->pInstrs = pInst;
  } else {
    pTmp = pIQ->pInstrs;
    while (pTmp->next && (pInst->cyc >= pTmp->cyc)) {
      pTmp = pTmp->next;
    }
    pInst->next = pTmp->next;
    pInst->prev = pTmp;
    pTmp->next = pInst;
    if (pInst->next)
      pInst->next->prev = pInst;
  }
  pIQ->count += 1;
}

void
iq_append(INST *pInst, INST_QUEUE *pIQ)
{
  INST *pTmp;

  if (pIQ->pInstrs == NULL) {
    pInst->next = NULL;
    pIQ->pInstrs = pInst;
  } else {
    pTmp = pIQ->pInstrs;
    while (pTmp->next) {
      pTmp = pTmp->next;
    }
    pInst->next = NULL;
    pInst->prev = pTmp;
    pTmp->next = pInst;
  }

  pIQ->count += 1;
}

INST *
iq_head(INST_QUEUE *pIQ)
{
  return pIQ->pInstrs;
}

/**
   \brief Remove inst from pIQ.
 */
void
iq_remove(INST *pInst, INST_QUEUE *pIQ)
{
  INST *pNext, *pPrev;

  pNext = pInst->next;
  pPrev = pInst->prev;

  pInst->next = NULL;
  pInst->prev = NULL;
  if (pIQ->pInstrs == pInst) {
    pIQ->pInstrs = pNext;
    if (pNext)
      pNext->prev = NULL;
  } else {
    if (pPrev)
      pPrev->next = pNext;
    if (pNext)
      pNext->prev = pPrev;
  }
  pIQ->count -= 1;
}

#define inst_gt(i, j) (((i).cyc > (j).cyc))
#define inst_lt(i, j) (((i).cyc < (j).cyc))
#define inst_eq(i, j) (((i).cyc == (j).cyc))
