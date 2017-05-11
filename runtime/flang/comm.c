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

/* clang-format off */

/* comm.c -- compiler interfaces to communication routines */

#include "stdioInterf.h"
#include "fioMacros.h"

#include "fort_vars.h"

/* simple communication schedule structure */

typedef struct {
  sked sked;
  chdr *channel;
} comm_sked;

/* ENTFTN(comm_start) function: adjust base addresses and call doit */

static void I8(comm_sked_start)(comm_sked *sk, char *rb, char *sb, F90_Desc *rd,
                                F90_Desc *sd)
{
#if defined(DEBUG)
  if (F90_KIND_G(rd) != F90_KIND_G(sd) || F90_LEN_G(rd) != F90_LEN_G(sd))
    __fort_abort("COMM_START: mismatched array types");
#endif
  rb += DIST_SCOFF_G(rd) * F90_LEN_G(rd);
  sb += DIST_SCOFF_G(sd) * F90_LEN_G(sd);
  __fort_adjbase(sk->channel, sb, rb, F90_KIND_G(rd), F90_LEN_G(rd));
  __fort_doit(sk->channel);
}

/* comm_free function: free channel and schedule structures */

static void
comm_sked_free(comm_sked *sk)
{
  __fort_frechn(sk->channel);
  __fort_free(sk);
}

/* create a simple communication schedule */

sked *I8(__fort_comm_sked)(chdr *ch, char *rb, char *sb, dtype kind, int len)
{
  comm_sked *sk;

  __fort_setbase(ch, sb, rb, kind, len);
  sk = (comm_sked *)__fort_malloc(sizeof(comm_sked));
  sk->sked.tag = __SKED;
  sk->sked.start = I8(comm_sked_start);
  sk->sked.free = comm_sked_free;
  sk->sked.arg = sk;
  sk->channel = ch;
  return &sk->sked;
}

void *ENTFTN(COMM_START, comm_start)(sked **skp, void *rb, F90_Desc *rd,
                                     void *sb, F90_Desc *sd)
{
  sked *sk;

  sk = *skp;
  if (sk != NULL) {
#if defined(DEBUG)
    if (sk->tag != __SKED)
      __fort_abort("COMM_START: invalid schedule");
#endif
    sk->start(sk->arg, rb, sb, rd, sd);
  }
  return NULL;
}

void ENTFTN(COMM_FINISH, comm_finish)(void *xp) {}

/* user-callable schedule executor; combines comm_start and comm_finish */

void ENTFTN(COMM_EXECUTE, comm_execute)(sked **skp, void *rb, void *sb,
                                        F90_Desc *skpd, F90_Desc *rd,
                                        F90_Desc *sd)
{
  sked *sk;

  if (!ISSCALAR(skpd) ||
      GET_DIST_SIZE_OF(F90_TAG_G(skpd)) != sizeof(__POINT_T))
    __fort_abort("COMM_EXECUTE: invalid schedule pointer");
  sk = *skp;
  if (sk != NULL) {
    if (sk->tag != __SKED)
      __fort_abort("COMM_EXECUTE: invalid schedule");
    sk->start(sk->arg, rb, sb, rd, sd);
  }
  /* call comm_finish here if it isn't a no-op. */
}

void ENTFTN(COMM_FREE, comm_free)(__INT_T *ns, ...)
{
  sked *sk;
  va_list va;
  int n;

  va_start(va, ns);
  for (n = *ns; n > 0; --n) {
    sk = *va_arg(va, sked **);
    if (sk == NULL)
      continue;
#if defined(DEBUG)
    if (sk->tag != __SKED)
      __fort_abort("COMM_FREE: invalid schedule");
#endif
    sk->free(sk->arg);
  }
  va_end(va);
}
