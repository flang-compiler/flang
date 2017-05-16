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
 *  \file
 *  \brief Fortran module responsible for generating Cross Reference Listing.
 */

#include "gbldefs.h"
#include "error.h"
#include "global.h"
#include "symtab.h"

struct memitem {
  int type; /* 0 - xref, 1 - par xref */
  int lineno;
  int use;
  int link;
};

static FILE *fd;
static int xrefcnt;
static struct memitem *baseptr;

/* ---------------------------------------------------------------------- */

void
xrefinit(void)
{
  xrefcnt = 0;
  baseptr = NULL;

  /* create temporary file and open it for writing */
  if ((fd = tmpf("b")) == NULL)
    errfatal(5);
}

/* ---------------------------------------------------------------------- */

/**
   \brief write one reference record to Reference File.
*/
void
xrefput(int symptr, int usage)
{
  struct memitem item;

  xrefcnt++;
  item.type = 0;
  item.lineno = gbl.lineno;
  item.use = usage;
  item.link = symptr;

  if ((fwrite((char *)(&item), sizeof(struct memitem), 1, fd)) == 0)
    interr("xrefput fwrite failed", xrefcnt, 4);
}

/* ---------------------------------------------------------------------- */

/**
   \brief write two lines of info for symbol elem to listing.
*/
static void
putentry(int elem)
{
  char buf[200];
  extern char *parmprint();
  char *ptr, *sc_p;
  int stype, dtype;
  char hyp;
  static char *scs[] = {"n/a",    "local", "static",  "dummy", "common",
                        "extern", "based", "private", "eight"};

  stype = STYPEG(elem);

  ptr = SYMNAME(elem);
  if (stype == ST_LABEL)
    ptr += 2; /* delete ".L" from front of label name */

  dtype = 0;
  if (stype != ST_LABEL && stype != ST_CMBLK) {
    dtype = DTYPEG(elem);
  }

  sprintf(buf, "%-16.16s ", ptr);
  getdtype(dtype, &buf[17]);
  strcat(buf, " ");
  strcat(buf, stb.stypes[stype]);
  list_line(buf);

  if ((int)strlen(ptr) > 16) {
    ptr += 16;
    hyp = '-';
  } else {
    ptr = "";
    hyp = ' ';
  }

  switch (stype) {
  case ST_LABEL:
    sprintf(buf, "%c%-15.15s   addr: %08" ISZ_PF "x", hyp, ptr, ADDRESSG(elem));
    break;
  case ST_PARAM:
/* Changed to display ALL PARAMETER data types LDE */
    sprintf(buf, "%c%-15.15s  value: %s", hyp, ptr, parmprint(elem));
    break;
  case ST_ENTRY:
    sprintf(buf, "%c%-15.15s   addr: %08" ISZ_PF "x", hyp, ptr, ADDRESSG(elem));
    break;
  case ST_CMBLK:
    sprintf(buf, "%c%-15.15s   size: %" ISZ_PF "d bytes", hyp, ptr,
            SIZEG(elem));
    break;

  case ST_VAR:
  case ST_IDENT:
  case ST_ARRAY:
  case ST_STRUCT:
  case ST_UNION:
    if (SCG(elem) == SC_CMBLK)
      sc_p = SYMNAME(MIDNUMG(elem));
    else
      sc_p = scs[SCG(elem)];
    sprintf(buf, "%c%-15.15s  sc: %s   addr: %08" ISZ_PF "x", hyp, ptr, sc_p,
            ADDRESSG(elem));
    break;

  case ST_PROC:
  case ST_STFUNC:
  case ST_GENERIC:
  case ST_INTRIN:
  default:
    if (hyp == '-')
      sprintf(buf, "%c%-15.15s", hyp, ptr);
    else
      hyp = 0;
  }

  if (hyp != 0)
    list_line(buf);

}

/* ---------------------------------------------------------------------- */

static void
printxref(int liststart)
{
  int sptr;
  int refptr;
  int i;
  char buf[200];

  /* print header */
  list_line("Cross Reference Listing:");
  list_line("");

  /* for each ref'd element of symbol table with a name */
  for (sptr = liststart; sptr != 0; sptr = HASHLKG(sptr)) {
    assert((NMPTRG(sptr) != 0), "printxref bad record name", sptr, 3);
    assert((XREFLKG(sptr) != 0), "printxref bad record link", sptr, 3);

    /* print stuff about symbol table entry */
    putentry(sptr);

    refptr = XREFLKG(sptr);

#define ENDOFLINE 80
    /* put references info */
    i = 999; /* i is char position in line being printed */
    do {
      if (i > (ENDOFLINE - 5)) {
        if (i != 999)
          list_line(buf);
        sprintf(buf, "                ");
        i = 16;
      }
      sprintf(&buf[i], "%5d%c", (baseptr + refptr)->lineno,
              (baseptr + refptr)->use);
      refptr = (baseptr + refptr)->link;
      i += 6;
    } while (refptr != 0);
    list_line(buf);
  }

  /* print trailer */
  list_line("---------------------------------");
  list_page();
}

/* ---------------------------------------------------------------------- */

static int
insertsrt(int q, int r)
{
  int i, last, next;
  char *ptr;

  HASHLKP(0, 0);

  for (i = q; i <= r; i++) {
    /* skip over those records without ref lists */
    if (XREFLKG(i) == 0)
      continue;
    ptr = SYMNAME(i);
    last = 0;
    next = HASHLKG(0);
    while ((next != 0) && (strcmp(ptr, SYMNAME(next)) >= 0)) {
      last = next;
      next = HASHLKG(next);
    }
    HASHLKP(i, next);
    HASHLKP(last, i);
  }
  return (HASHLKG(0));
}

/* ---------------------------------------------------------------------- */

static int
merge(int q, int r)
{
  int i, j, k;

  k = 0;
  i = q;
  j = r;
  while ((i != 0) && (j != 0)) {
    if (strcmp(SYMNAME(i), SYMNAME(j)) <= 0) {
      HASHLKP(k, i);
      k = i;
      i = HASHLKG(i);
    } else {
      HASHLKP(k, j);
      k = j;
      j = HASHLKG(j);
    }
  }
  if (i == 0)
    HASHLKP(k, j);
  else
    HASHLKP(k, i);

  return (HASHLKG(0));
}

/* ---------------------------------------------------------------------- */

static int
mergesrt(int low, int high)
{
  int mid;
  int p, q, r;
#define SORTLIM 16

  if ((high - low) < SORTLIM)
    p = insertsrt(low, high);
  else {
    mid = (low + high) / 2;
    q = mergesrt(low, mid);
    r = mergesrt(mid + 1, high);
    p = merge(q, r);
  }
  return (p);
}

/* ---------------------------------------------------------------------- */

void
xref(void)
{
  struct memitem *iptr;
  int sptr;
  int index;
  int i;
  int liststart, nsyms;

  if (!xrefcnt)
    return;

  /* rewind file */
  if (fseek(fd, 0L, 0) == EOF)
    interr("xref's fseek failed", 0, 4);

  /* allocate space for ref lists */
  NEW(baseptr, struct memitem, xrefcnt + 1);
  if (baseptr == NULL)
    interr("xref couldn't alloc enough space", 0, 4);

  /* fill symbol table's hash field with NULLs (sort links)    */
  /* fill w7 field in symbol table with NULLs  (ref list head) */
  nsyms = stb.symavl;
  for (i = 1; i <= nsyms; i++) {
    HASHLKP(i, 0);
    XREFLKP(i, 0);
  }

  /* read ref info into memory - 0 pos is wasted */
  if (fread((char *)(baseptr + 1), sizeof(struct memitem), xrefcnt, fd) == 0)
    interr("xref fread failed", xrefcnt, 4);

  /* process in reverse order so lists are in correct order */
  iptr = baseptr + xrefcnt;
  index = xrefcnt;
  while (iptr > baseptr) {
    if (iptr->type == 0) {
      sptr = iptr->link;
      iptr->link = XREFLKG(sptr);
      XREFLKP(sptr, index);
    }
    index--;
    iptr--;
  }

  /* sort symbol table on SYMNAMES  */
  liststart = mergesrt(1, nsyms);

  printxref(liststart);
  if (!XBIT(0, 0x200)) {
    FREE(baseptr);
    fclose(fd);
  }

}

/* ---------------------------------------------------------------------- */

void
par_xref_put(int lineno, int sym, int sc)
{
  struct memitem item;

  xrefcnt++;
  item.type = 1;
  item.lineno = lineno;
  item.use = sc;
  item.link = sym;

  if (fwrite((char *)(&item), sizeof(struct memitem), 1, fd) == 0)
    interr("xrefput fwrite failed", xrefcnt, 4);
}

/* data collection per each parallel region - in sorted order by lineno */
typedef struct {
  int lineno;
  int shared;
  int private;
  int prev;
  int next;
} RGN;
static RGN *rgn_base;
static int rgn_avl;
static int rgn_size;
static int rgn_cur;

static int
find_rgn(int lineno)
{
  int ll;
  int i, here;

  ll = rgn_base[rgn_cur].lineno;
  if (lineno == ll)
    return rgn_cur;
  if (lineno > ll)
    i = rgn_cur;
  else
    i = 0;
  here = i;
  for (here = i; (i = rgn_base[i].next); here = i) {
    if (lineno < rgn_base[i].lineno) {
      break;
    }
    if (lineno == rgn_base[i].lineno) {
      rgn_cur = i;
      return i;
    }
  }
  /*  Insert after 'here' */
  i = rgn_avl++;
  ll = rgn_base[here].next;
  NEED(rgn_avl + 1, rgn_base, RGN, rgn_size, rgn_size + 16);
  rgn_base[i].lineno = lineno;
  rgn_base[i].shared = rgn_base[i].private = 0;
  rgn_base[i].prev = here;
  rgn_base[i].next = ll;
  rgn_base[here].next = i;
  rgn_base[ll].prev = i;

  rgn_cur = i;
  return i;
}

static void
add_to_list(int *list, int index)
{
  int s;
  int prev;

  prev = *list;
  for (s = prev; s; prev = s, s = baseptr[s].use) {
    if (baseptr[s].link == baseptr[index].link)
      return;
  }
  if (prev) {
    baseptr[index].use = baseptr[prev].use;
    baseptr[prev].use = index;
  } else {
    *list = index;
    baseptr[index].use = 0;
  }
}

static void
par_body(char *hdr, int *list)
{
  char *nm;
  char buf[200];
  int s;
  int cnt;
#undef WIDTH
#define WIDTH 8

  list_line(hdr);
  cnt = 0;
  for (s = *list; s; s = baseptr[s].use) {
    nm = SYMNAME(baseptr[s].link);
    if (strlen(nm) + cnt > 80) {
      list_line(buf);
      cnt = 0;
    }
    sprintf(buf + cnt, "    %-*s", WIDTH, nm);
    cnt += 4;
    if (strlen(nm) <= WIDTH)
      cnt += WIDTH;
    else
      cnt += strlen(nm);
  }
  if (cnt)
    list_line(buf);
  list_line("");
}

static void
print_par_xref(void)
{
  int i;
  int s;
  char *nm;
  char buf[200];
  int cnt;

  for (i = rgn_base[0].next; i; i = rgn_base[i].next) {
    sprintf(buf, "Parallel region beginning at line number %d:",
            rgn_base[i].lineno);
    list_line(buf);
    par_body("  Shared variables:", &rgn_base[i].shared);
    par_body("  Private variables:", &rgn_base[i].private);
  }
  list_line("---------------------------------");
  list_page();
}

void
par_xref(void)
{
  struct memitem *iptr;
  int sptr;
  int index;
  int i;
  int nsyms;

  if (!xrefcnt)
    return;

  /* rewind file */
  if (fseek(fd, 0L, 0) == EOF)
    interr("par_xref's fseek failed", 0, 4);

  if (!flg.xref) {
    /* allocate space for ref lists */
    NEW(baseptr, struct memitem, xrefcnt + 1);
    if (baseptr == NULL)
      interr("par_xref couldn't alloc enough space", 0, 4);
  }

  /* read ref info into memory - 0 pos is wasted */
  if (fread((char *)(baseptr + 1), sizeof(struct memitem), xrefcnt, fd) == 0)
    interr("par_xref fread failed", xrefcnt, 4);

  rgn_size = 16;
  rgn_avl = 1;
  NEW(rgn_base, RGN, rgn_size);
  rgn_base[0].lineno = -1;
  rgn_base[0].prev = rgn_base[0].next = 0;
  rgn_base[0].shared = rgn_base[0].private = 0;
  rgn_cur = 0;

  /* process in forward order - in most cases, line numbers increase */
  iptr = baseptr + 1;
  for (index = 1; index <= xrefcnt; index++, iptr++) {
    if (iptr->type == 1) {
      i = find_rgn(iptr->lineno);
      sptr = iptr->link;
      if (iptr->use == SC_PRIVATE)
        add_to_list(&rgn_base[i].private, index);
      else
        add_to_list(&rgn_base[i].shared, index);
    }
  }

  print_par_xref();

  FREE(baseptr);
  FREE(rgn_base);
  fclose(fd);

}
