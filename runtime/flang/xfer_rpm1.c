/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

#include "stdioInterf.h"
#include "fioMacros.h"

extern void *__fort_malloc();

int __fort_minxfer = 0;

/* receive data items */

void __fort_erecv(cpu, e) int cpu;
struct ents *e;
{
  __fort_abort("__fort_erecv: not implemented");
}

/* send data items */

void __fort_esend(cpu, e) int cpu;
struct ents *e;
{
  __fort_abort("__fort_esend: not implemented");
}

/* bcopy data items */

void __fort_ebcopys(ed, es) struct ents *ed;
struct ents *es;
{
  struct ent *p;
  struct ent *q;

  p = es->beg;
  q = ed->beg;
#if defined(DEBUG)
  if (ed->avl - q != es->avl - p)
    __fort_abort("ebcopys: unmatched send/recv");
#endif
  while (q < ed->avl) {
#if defined(DEBUG)
    if (p->cnt != q->cnt)
      __fort_abort("ebcopys: inconsistent counts");
    if (p->ilen != q->ilen)
      __fort_abort("ebcopys: inconsistent item length");
#endif
    __DIST_ENTRY_COPY(p->len);
    __fort_bcopysl(q->adr, p->adr, q->cnt, q->str, p->str, q->ilen);
    __DIST_ENTRY_COPY_DONE();
    p++;
    q++;
  }
}

/* execute structure */

void __fort_doit(c) struct chdr *c;
{
  struct ccpu *cp;
  int n;

  while (c != (struct chdr *)0) {
    for (n = 0; n < c->cn; n++) {
      cp = &(c->cp[n]);
      switch (cp->op) {
      case CPU_COPY:
        __fort_ebcopys(cp->rp, cp->sp);
        break;
      default:
        __fort_abort("__fort_doit: invalid operation\n");
        break;
      }
    }
    c = c->next;
  }
}
