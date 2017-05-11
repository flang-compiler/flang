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
   \brief x86-64 and x86-32 code generator

   Performs basic list scheduling over a DAG of AILI.  The DAG is built by
   cgsched.c.  Most of the interesting code is in the selection heuristics,
   found in selectInst().
 */

#include "gbldefs.h"
#include "error.h"
#include "global.h"
#include "symtab.h"
#include "ili.h"
#include "machreg.h"
#include "cg.h"
#include "cgsched.h"
#include "sched-util.h"
#include "schinfo.h"
#include "x86.h"

typedef enum { DT_Direct, DT_Double, DT_Vector } DECODE_TYPE;

/* FPU pipes */
#define FPT_FADD 0x2
#define FPT_FMUL 0x4
#define FPT_FSTO 0x8

/* Some instruction characteristic information */
typedef struct tagINST_CHAR {
  DECODE_TYPE dec_type;
  short fpu_pipe;
  short latency; /* Static latency */
} INST_CHAR;

typedef struct tagMACH_QUEUES {
  INST_QUEUE *int_alu;

  INST_QUEUE *fadd;
  INST_QUEUE *fmul;
  INST_QUEUE *fst;
} MACH_QUEUES;

static int recent_fp;

static int getLat(INST *, SCH *);
#if DEBUG
static void dump_segment(SCH *);
#endif

/**
   \brief Return delay time associated with executing the instruction
 */
static int
delay_time(ANODE *pAnode)
{
  AILI *pAi;
  ILI_OP opc;

  pAi = pAnode->ai;
  opc = pAi->opc;

  switch (opc) {
  case IL_PDMUL:
  case IL_PDMULX:
  case IL_DMUL:
    return 5;
  case IL_URSHIFT:
  case IL_ARSHIFT:
  case IL_KLSHIFT:
  case IL_KURSHIFT:
    return 2;
  default:
    return 1; /* Dummy ExecTime function for now */
  }
}

static int
latency(ANODE *pAnode1, int c1, ANODE *pAnode2, int c2)
{
  AILI *pAi1;
  ILI_OP opc1;

  pAi1 = pAnode1->ai;
  opc1 = pAi1->opc;
  /*
      pAi2 = pAnode2->ai;
      opc2 = pAi2->opc;
  */
  switch (opc1) {
  case IL_PDMUL:
  case IL_PDMULX:
    return 5;
  default:
    return 1; /* Dummy for now */
  }
}

static void
buildDelays(SCH *pSch)
{
  int i;
  int delay, m_delay;
  ANODE *pAnode, *pAnode2;
  PRITEM *pSucc;

  for (i = 1; i < pSch->aili_count; i++) {
    pAnode = &(pSch->ailis[i]);
    if (is_succ_leaf(pAnode)) {
      delay = delay_time(pAnode);
      delay = getLat(pAnode->pInst, pSch);
    } else {
      delay = 0;
      pSucc = pAnode->succ_list;
      while (pSucc != NULL) {
        pAnode2 = &(pSch->ailis[pSucc->ainum]);
        m_delay = latency(pAnode, 2, pAnode2, 1) + delay_time(pAnode2);
        delay = (delay >= m_delay) ? delay : m_delay;
        pSucc = pSucc->next;
      }
    }
    pAnode->delay = delay;
    /*printf ("Delay(%d): %d\n", pAnode->ai->number, pAnode->delay); */
  }
}

static void
buildDelays2(SCH *pSch)
{
  int i, j;
  int lat, lat2;
  int delay, m_delay;
  int early, max_early, late, max_late;
  ANODE *pAnode, *pAnode2;
  PRITEM *pred, *succ;

  for (i = 1; i < pSch->aili_count; i++) {
    pAnode = &(pSch->ailis[i]);

    early = 0;
    for (pred = pAnode->pred_list; pred != NULL; pred = pred->next) {
      pAnode2 = &(pSch->ailis[pred->ainum]);
      lat2 = getLat(pAnode2->pInst, pSch);
      if ((pAnode2->early + lat2) > early) {
        early = pAnode2->early + lat2;
      }
    }
    pAnode->early = early;
  }

  for (i = pSch->aili_count - 1; i >= 1; i--) {
    pAnode = &(pSch->ailis[i]);

    lat = getLat(pAnode->pInst, pSch);
    late = 0;
    if (pAnode->succ_list == NULL)
      late = pAnode->early;
    for (succ = pAnode->succ_list; succ != NULL; succ = succ->next) {
      pAnode2 = &(pSch->ailis[succ->ainum]);
      if (late == 0 || (pAnode2->late - lat) < late) {
        late = pAnode2->late - lat;
      }
    }
    pAnode->late = late;
  }
}

static int
aili_in_state(int ainum, SCH *pSch, STATE State)
{
  INST *pInst;
  ANODE *pAnode;

  pAnode = &(pSch->ailis[ainum]);

  asrt(pAnode != NULL);
  asrt(pAnode->pInst != NULL);

  if (((INST *)pAnode->pInst)->state == State)
    return TRUE;

  return FALSE;
}

static int
aili_in_sched(int ainum, SCH *pSch, INST_QUEUE *pSched)
{
  INST *pInst;

  pInst = pSched->pInstrs;
  while (pInst) {
    /*if (pInst->pAnode->ai->number == ainum)*/
    if (pInst->ainum == ainum)
      return TRUE;
    pInst = pInst->next;
  }
  return FALSE;
}

static int
aili_cyc(int ainum, SCH *pSch)
{
  INST *pInst;
  ANODE *pAnode;

  pAnode = &(pSch->ailis[ainum]);

  asrt(pAnode != NULL);
  asrt(pAnode->pInst != NULL);

  return (((INST *)(pAnode->pInst))->cyc);
}

/* Scheduling selection stuff */

static int
is_shift(AILI *pAi)
{
  ILI_OP opc;

  opc = pAi->opc;
  switch (opc) {
  case IL_ARSHIFT:
  case IL_KARSHIFT:
  case IL_URSHIFT:
  case IL_KURSHIFT:
    return TRUE;
  default:
    return FALSE;
  }
}

static int
isPrefetch(ILI_OP opc)
{
  switch (opc) {
  case IL_PREFETCH:
  case IL_PREFETCHT0:
  case IL_PREFETCHW:
    return 1;
  default:
    return 0;
  }
}

#define RVEC_LEN 8

static int ResVec[RVEC_LEN];

static void
initSelect(void)
{
  ResVec[0] = 3;
  ResVec[P_FADD] = 1;
  ResVec[P_FMUL] = 1;
  ResVec[P_FMUL | P_FADD] = 2;
  ResVec[P_FST] = 1;
  ResVec[P_FST | P_FADD] = 2;
  ResVec[P_FST | P_FMUL] = 2;
  ResVec[P_FST | P_FMUL | P_FADD] = 3;
}

static int
isLoad(AILI *pAi)
{
  ILI_OP opc;
  OPRND *op1, *op2, *dest;

  opc = pAi->opc;
  op1 = pAi->src1;
  op2 = pAi->src2;
  dest = pAi->dest;

  return op1 && op1->opkind == OP_ADDR && op2 == NULL && dest &&
         (dest->opkind == OP_REG || dest->opkind == OP_REGPAIR ||
          dest->opkind == OP_X87);
}

static int
isStore(AILI *pAi)
{
  ILI_OP opc;
  OPRND *op1, *op2, *dest;

  opc = pAi->opc;
  op1 = pAi->src1;
  op2 = pAi->src2;
  dest = pAi->dest;

  return op1 && (op1->opkind == OP_REG || op1->opkind == OP_REGPAIR ||
                 op1->opkind == OP_X87) &&
         op2 == NULL && dest && dest->opkind == OP_ADDR;
}

static int
isRM(AILI *pAI)
{
  if (!XBIT(92, 0x8))
    return TRUE;
  if (pAI->src1) {
    if (pAI->src1->opkind == OP_ADDR)
      return TRUE;
#if DEBUG
    if ((unsigned int)(pAI->src1->opkind) > 6)
      interr("bad isRM src1 value", pAI->src1->opkind, 1);
#endif
  }
  if (pAI->src2) {
    if (pAI->src2->opkind == OP_ADDR)
      return TRUE;
#if DEBUG
    if ((unsigned int)(pAI->src2->opkind) > 6)
      interr("bad isRM src2 value", pAI->src2->opkind, 1);
#endif
  }
  if (pAI->dest) {
    if (pAI->dest->opkind == OP_ADDR)
      return TRUE;
#if DEBUG
    if ((unsigned int)(pAI->dest->opkind) > 6)
      interr("bad isRM dest value", pAI->dest->opkind, 1);
#endif
  }
  return FALSE;
}

static int
getFPRes(INST *pInst, SCH *pSch)
{
  ILI_OP opc;
  int attr;
  int mask = (P_FST | P_FMUL | P_FADD);

  if (pInst == NULL)
    return 0;

  opc = (pSch->ailis[pInst->ainum]).ai->opc;

  attr = SCH_ATTR(opc);

  if (((attr >> LD_SHIFT) & mask) && isLoad(pInst->pAnode->ai))
    attr = attr >> LD_SHIFT & 0xff;
  else if (((attr >> ST_SHIFT) & mask) && isStore(pInst->pAnode->ai))
    attr = attr >> ST_SHIFT & 0xff;

  attr = attr & mask;

  return attr;
}

static int
getLat(INST *pInst, SCH *pSch)
{
  AILI *ai;
  ILI_OP opc;
  int attr;
  int base_lat, rr_lat, rm_lat, lat;

  if (pInst == NULL)
    return 0;

  ai = (pSch->ailis[pInst->ainum]).ai;
  opc = (pSch->ailis[pInst->ainum]).ai->opc;
  base_lat = SCH_LAT(opc);

  if (base_lat == 0)
    return 1;

  rr_lat = (base_lat >> RR_SHIFT) & 0xff;
  rm_lat = (base_lat >> RM_SHIFT) & 0xff;

  if (isRM(ai)) {
    lat = (rm_lat != 0) ? rm_lat : rr_lat;
  } else { /* rr */
    lat = rr_lat;
  }
  if (lat == 0)
    lat = 1;
  return lat;
}

/**
   \brief Called after an instruction has been selected for
   scheduling.  Updates the resource vector to remove any consumed
   resources.
 */
static void
updateResources(INST *pInst, SCH *pSch)
{
  ILI_OP opc;
  int attr;

  opc = (pSch->ailis[pInst->ainum]).ai->opc;

  attr = SCH_ATTR(opc);
  attr = attr & (P_FADD | P_FMUL | P_FST);

#ifdef DEW
/* printf("Inst attr(%s): %#x\n", IL_NAME(opc), attr); */
#endif
  if (attr == (P_FADD | P_FMUL | P_FST)) {
    ResVec[P_FST | P_FMUL | P_FADD] -= 1;
  } else if (attr == (P_FST | P_FMUL)) {
    ResVec[P_FST | P_FMUL] -= 1;
  } else if (attr == (P_FST | P_FADD)) {
    ResVec[P_FST | P_FADD] -= 1;
  } else if (attr == (P_FMUL | P_FADD)) {
    ResVec[P_FMUL | P_FADD] -= 1;
  } else if (attr == P_FST) {
    ResVec[P_FST | P_FMUL | P_FADD] -= 1;
    ResVec[P_FST | P_FMUL] -= 1;
    ResVec[P_FST | P_FADD] -= 1;
    ResVec[P_FST] -= 1;
  } else if (attr == P_FMUL) {
    ResVec[P_FST | P_FMUL | P_FADD] -= 1;
    ResVec[P_FMUL | P_FST] -= 1;
    ResVec[P_FMUL | P_FADD] -= 1;
    ResVec[P_FMUL] -= 1;
  } else if (attr == P_FADD) {
    ResVec[P_FST | P_FMUL | P_FADD] -= 1;
    ResVec[P_FADD | P_FST] -= 1;
    ResVec[P_FADD | P_FMUL] -= 1;
    ResVec[P_FADD] -= 1;
  }
}

#define SLACK(ip) ((ip)->pAnode->late - (ip)->pAnode->early)

static AILI *pLastMovsd; /* EM64T */
static AILI *pLastMinMax;

static int
loadIsSameBase(AILI *pAi1, AILI *pAi2)
{
  ADDROP *aop1, *aop2;

  if (!(isLoad(pAi1) && isLoad(pAi2)))
    return FALSE;

  aop1 = (ADDROP *)&(pAi1->src1->u);
  aop2 = (ADDROP *)&(pAi2->src1->u);
  if (aop1->scale != aop2->scale)
    return FALSE;
  if (aop1->scale == 1) {
    /* base and index can be commuted */
    if (((aop1->basereg == aop2->basereg) &&
         (aop1->indexreg == aop2->indexreg)) ||
        ((aop1->basereg == aop2->indexreg) &&
         (aop1->indexreg == aop2->basereg))) {
      return TRUE;
    }
  } else {
    if (((aop1->basereg == aop2->basereg) &&
         (aop1->indexreg == aop2->indexreg))) {
      return TRUE;
    }
  }
  return FALSE;
}

/**
   \brief Returns boolean, value in d is valid iff true.
 */
static int
loadDistance(AILI *pAi1, AILI *pAi2, int *d)
{
  int b1, b2;
  if (loadIsSameBase(pAi1, pAi2)) {

    if (pAi1->src1->u.addr.ksptr == pAi2->src1->u.addr.ksptr) {
      *d = 0;
      return TRUE;
    }
    b1 = 0;
    b2 = 0;
    if (pAi1->src1->u.addr.ksptr) {
      if (DTYPEG(pAi1->src1->u.addr.ksptr) == DT_INT)
        b1 = CONVAL2G(pAi1->src1->u.addr.ksptr);
      else
        return FALSE;
    }

    if (pAi2->src1->u.addr.ksptr) {
      if (DTYPEG(pAi2->src1->u.addr.ksptr) == DT_INT)
        b2 = CONVAL2G(pAi2->src1->u.addr.ksptr);
      else
        return FALSE;
    }

    *d = b2 - b1;
    return TRUE;
  }
  return FALSE;
}

static INST *
selectInst(INST_QUEUE *pReady, SCH *pSch, int nCurCyc)
{
  INST *pInst, *pBest, *pRet, *pIntel;
  ANODE *pAnode;
  PRITEM *pSucc;
  AILI *pAi;
  ILI_OP opc;
  int aili;
  int regSucc;
  int fp_res, best_fp_res;

  pBest = NULL;
  pIntel = NULL;
  pRet = iq_head(pReady);
  pInst = pRet;
  pAi = (pSch->ailis[pRet->ainum]).ai;
  opc = (pSch->ailis[pRet->ainum]).ai->opc;

  /* for each inst in Ready */
  regSucc = 0;
  while (pInst) {
    pAnode = pInst->pAnode;

    asrt(pAnode);
    if (!XBIT(92, 1)) {
      /* New selection heuristics */
      fp_res = getFPRes(pInst, pSch);
      best_fp_res = getFPRes(pBest, pSch);

      /* No best instruction recorded yet */
      /*1*/ if (pBest == NULL) {
        pBest = pInst;
      } else if (pLastMinMax != NULL) {
        pAi = pSch->ailis[pInst->ainum].ai;
        if (pAi->src1 && pAi->src1->opkind == OP_ADDR) {
          pBest = pInst;
        }

      }
      /*2*/ else if (pBest->pAnode->early > nCurCyc &&
                     pInst->pAnode->early <= nCurCyc) {
        pBest = pInst;
      }
      /*3*/ else if (pBest->pAnode->early <= nCurCyc &&
                     pInst->pAnode->early > nCurCyc) {
        pBest = pBest;
      } else if (XBIT(92, 0x4) &&
                 isPrefetch(pSch->ailis[pBest->ainum].ai->opc) &&
                 !isPrefetch(pSch->ailis[pInst->ainum].ai->opc)) {
        pBest = pInst;
      }
      /*4*/ else if (fp_res != 0 && ((fp_res & recent_fp) == 0) &&
                     best_fp_res == 0) {
        pBest = pInst;
      }
      /*5*/ else if (best_fp_res != 0 && ((best_fp_res & recent_fp) == 0) &&
                     fp_res == 0) {
        pBest = pBest;
      }
      /*6*/ else if (fp_res != 0 && ((fp_res & recent_fp) == 0) &&
                     best_fp_res != 0 && ((best_fp_res & recent_fp) != 0)) {
        pBest = pInst;
      }
      /*7*/ else if ((best_fp_res != 0 && ((best_fp_res & recent_fp) == 0)) &&
                     (fp_res != 0 && ((fp_res & recent_fp) != 0))) {
        pBest = pBest;
      }
      /*8*/ else if (SLACK(pInst) < SLACK(pBest)) {
        pBest = pInst;
      }
      /*9*/ else if (SLACK(pBest) < SLACK(pInst)) {
        pBest = pBest;
      }
      /*10*/ else if (getLat(pInst, pSch) > getLat(pBest, pSch)) {
        pBest = pInst; /* Prefer longer-latency inst's */
      }
      /* Craig's EM64T craziness */
      else if (XBIT(72, 0x2) && pLastMovsd != NULL) {
        pAi = pSch->ailis[pInst->ainum].ai;
        if (pAi->opc == IL_PDLD_HIGHH && isLoad(pAi) &&
            pAi->dest == pLastMovsd->dest) {
          pBest = pInst;
          pIntel = pInst;
        }
      } else if (XBIT(72, 0x4) && pLastMovsd != NULL) {
        int dist;
        pAi = pSch->ailis[pInst->ainum].ai;
        if (pAi->opc == IL_PDLD_HIGHH && isLoad(pAi) &&
            loadDistance(pAi, pLastMovsd, &dist) && abs(dist) < 64) {
          pBest = pInst;
          pIntel = pInst;
        }
      }
    } else {
      if (pBest == NULL)
        pBest = pInst;
      /* for each s in succ(inst) */
      pSucc = pAnode->succ_list;
      while (pSucc) {
        if (pSucc->deptype == DT_REGUSE) {
          regSucc = 1;
          break;
        }
        pSucc = pSucc->next;
      }

      /* prefer register successors */
      if (regSucc) {
        pBest = pInst;
        break;
      }
    } /* else */
    pInst = pInst->next;
  }
  pRet = pBest;
  if (pBest == pIntel) {
    /* we chose an instruction because of the Intel heuristic,
     * which we wouldn't have chosen if it hadn't been an Intel */
    ++mach_count.type[MACH_INTEL_PENTIUM4];
  }

  recent_fp = getFPRes(pRet, pSch);

  updateResources(pRet, pSch);

  /* EM64T Heuristic a la Craig */
  if (mach.type[MACH_INTEL_PENTIUM4]) {
    pAi = (pSch->ailis[pRet->ainum]).ai;
    if (pAi->opc == IL_MOVDP && isLoad(pAi)) {
      pLastMovsd = pAi;
    } else {
      pLastMovsd = NULL;
    }
  }

  /* hide latency of cmov's.  Falls out of 456.hmmer analysis.
   */
  /*if (mach.type[MACH_AMD_HAMMER]) */
  if (1) {
    pAi = (pSch->ailis[pRet->ainum]).ai;
    if (pAi->opc == IL_IMIN || pAi->opc == IL_IMAX || pAi->opc == IL_KMIN ||
        pAi->opc == IL_KMAX) {
      pLastMinMax = pAi;
    } else {
      pLastMinMax = NULL;
    }
  }

  return pRet;
}

static void
dump_q(char *txt, INST_QUEUE *pQ, SCH *pSch)
{
  INST *pInst;
  int i;

  printf("%s Queue = {", txt);
  i = 0;
  pInst = pQ->pInstrs;
  while (pInst) {
    printf("%d [%d], ", (pSch->ailis[pInst->ainum]).ai->number, pInst->cyc);
    if (!(++i % 16))
      printf("\n               ");
    pInst = pInst->next;
  }
  printf("}\n");
}

int
schedule_dag(SCH *pSch)
{
  int i, j, flag;
  int meta_root;
  int last_sch_aili;
  int nLat, nMaxLat;
  int nCycle;
  int top, bottom;
  int cnt; /* Modulo counter for determining reset of
              instruction selection parameters */

  int seq_start, seq_cnt, seq_nxt;

  ANODE *pAnode;
  PRITEM *pred, *succ;
  INST *pNewInst;
  INST *pInst, *pInstTmp;

  INST_QUEUE Sched;
  INST_QUEUE Ready;
  INST_QUEUE Cands;

  pLastMovsd = NULL;
  pLastMinMax = NULL;

  top = 0;
  bottom = pSch->aili_count - 1;

  /* Sched = [] */
  Sched.size = 0;
  Sched.count = 0;
  Sched.pInstrs = NULL;

  /* Cands = [] */
  Cands.size = 0;
  Cands.count = 0;
  Cands.pInstrs = NULL;

  Ready.size = 0;
  Ready.count = 0;
  Ready.pInstrs = NULL;

  /* Build up Cands */
  for (i = 1; i < pSch->aili_count - 1; i++) {
    pAnode = &(pSch->ailis[i]);
    if (pAnode->sequence < 0)
      continue;
    pNewInst = new_inst(i, pSch);
    iq_cons(pNewInst, &Cands);
  }

  nCycle = 1;

  /* Ready = Roots(nodes) */
  pInst = Cands.pInstrs;
  while (pInst) {
    pAnode = &(pSch->ailis[pInst->ainum]);
    if (is_pred_leaf(pAnode)) {
      pInstTmp = pInst;
      pInstTmp->cyc = nCycle;
      pInst = pInst->next;

      iq_remove(pInstTmp, &Cands);
      iq_cons(pInstTmp, &Ready);
      pInstTmp->state = SS_Ready;
    } else {
      pInst = pInst->next;
    }
  }

  buildDelays(pSch);
  if (!XBIT(92, 0x1)) {
    buildDelays2(pSch);
#if DEBUG
    if (DBGBIT(16, 0x200))
      dump_segment(pSch);
#endif
  }

  recent_fp = 0;
  cnt = 0;
  while (!iq_isempty(&Ready)) {

    if (cnt == 0) {
      nCycle += 1;
      initSelect();
    }
    /* Select inst from Cands */
    pInst = selectInst(&Ready, pSch, nCycle);

    /* Remove Inst from Ready */
    iq_remove(pInst, &Ready);

    if (cnt == 0) {
      nCycle = (nCycle > pInst->pAnode->early) ? nCycle : pInst->pAnode->early;
    }
    /* Set Inst to be scheduled this cycle */
    pInst->cyc = nCycle;

    /* Add Inst to Sched */
    iq_append(pInst, &Sched);
    pInst->state = SS_Sched;

    /*nCycle += 1; */

    /* For each successor of Inst */
    succ = pSch->ailis[pInst->ainum].succ_list;
    while (succ) {
      if (succ->ainum == bottom) {
        succ = succ->next;
        continue;
      }

      /* Update the early field */
      if (succ->deptype == DT_REGUSE) {
        (pSch->ailis[succ->ainum]).early = nCycle + getLat(pInst, pSch);
      } else {
        (pSch->ailis[succ->ainum]).early = nCycle + 1;
      }

      if (aili_in_state(succ->ainum, pSch, SS_Ready)) {
        succ = succ->next;
        continue;
      }

      /* If all preds(i) are in Sched */
      pred = (pSch->ailis[succ->ainum]).pred_list;
      flag = TRUE;
      nMaxLat = 0;
      while (pred) {
        if (pred->ainum != 0) {
          if (!aili_in_state(pred->ainum, pSch, SS_Sched)) {
            flag = FALSE;
            break;
          } else {
            pAnode = &(pSch->ailis[pred->ainum]);
            pInst = pAnode->pInst;
            nLat = aili_cyc(pred->ainum, pSch) +
                   latency(pAnode, 1, pInst->pAnode, 1);
            if (nLat > nMaxLat)
              nMaxLat = nLat;
          }
        }
        pred = pred->next;
      }

      /* Add i to Ready */
      pAnode = &(pSch->ailis[succ->ainum]);
      pInst = pAnode->pInst;
      if (flag) {
        pInst->cyc = nMaxLat;
        iq_remove(pInst, &Cands);

        /*	iq_append(pInst, &Ready);  */ /* Implement FIFO policy */
        /*	iq_cons(pInst, &Ready);    */ /* Implement LIFO policy */
        iq_insert(pInst, &Ready);
        pInst->state = SS_Ready;
      }

      succ = succ->next;
      /* pInst = pInstTmp; */
    } /* for each i in Succ(inst) */

    cnt = (cnt + 1) % 3;
  }

  /* Loop over Sched, fixing up the Sch structure */
  pInst = Sched.pInstrs;
  if (pInst) {
    pSch->first_scheduled_aili = pInst->ainum;
    last_sch_aili = pInst->ainum;
    pInst = pInst->next;
    while (pInst) {
      (pSch->ailis[last_sch_aili]).next_scheduled_aili = pInst->ainum;
      last_sch_aili = pInst->ainum;

      seq_cnt = pSch->ailis[last_sch_aili].sequence;
      pInst = pInst->next;
    }
    (pSch->ailis[last_sch_aili]).next_scheduled_aili = -1;

    while (Sched.pInstrs) {
      pInst = Sched.pInstrs;
      Sched.pInstrs = Sched.pInstrs->next;
      FREE(pInst);
    }
  }

  return 1;
}

#if DEBUG
static void
dump_segment(SCH *pSch)
{
  int i;
  ANODE *p;

  fprintf(gbl.dbgfil, "\n");
  for (i = 1; i < pSch->aili_count; i++) {
    p = &(pSch->ailis[i]);
    fprintf(gbl.dbgfil, " [%02d] (%4d) %s  early: %d  late: %d\n", i,
            p->ai->number, IL_NAME(p->ai->opc), p->early, p->late);
  }
}
#endif
