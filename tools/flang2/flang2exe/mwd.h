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
 * \brief Header for mw's dump routines
 */

#if DEBUG
extern char *printname(int sptr);
extern void putdtype(DTYPE dtype);
extern void ds(int sptr);
extern void dsa(void);
extern void dss(int l, int u);
extern void dsym(int sptr);
extern void dsyms(int l, int u);

extern void ddtype(int dtype);
extern void ddtypes(void);
extern void dumpdtype(int dtype);
extern void dumpdtypes(void);

extern void dili(int ilix);
extern void dilitre(int ilix);
extern void dilt(int ilt);
extern void dumpilt(int ilt);

extern void db(int block);
extern void dbih(void);
extern void dbihonly(void);
extern void dumpblock(int block);
extern void dumptblock(char *title, int block);
extern void dumpblocks(char *title);
extern void dumpblocksonly(void);
extern void dumpfnode(int v);
extern void dumpfgraph(void);
extern void printnme(int n);
extern void dumpnmetree(int n);
extern void dumpnme(int n);
extern void dumpnnme(int n);
extern void _dumpnme(int n, LOGICAL dumpdefsuses);
extern void dumpnmes(void);
extern void dumpdefnmes(void);
extern void dumpdef(int def);
extern void _dumpdef(int def, int dumpuses);
extern void _dumpuse(int use, int dumpdefs);
extern void dumpdefs(void);
extern void dumpuse(int use);
extern void dumpuses(void);
extern void dumploop(int l);
extern void dumploops(void);
extern void dumploopsbv(int bvlen);
extern void dumplooptree(void);
extern void dumplili(int li);
extern void dumpalllili(void);
extern void dumptlili(char *title);

extern void dumpauses(void);
extern void dumpreducdefs(void);
extern void dumpuseddefs(void);
extern void dumpliveoutdefs(void);
extern void dumpliveinuses(void);
extern void dumpuseduses(void);
extern void dumpprivatelist(void);
extern void dumpivlist(void);

extern void dumplong(void);
extern void dumpdiff(void);
extern void dumpddiff(int);
extern void dumpdfs(void);

extern void putmwline(void);

extern void dumpregion(int r);
extern void dumpregionnest(int r);
void simpleprintregionnest(char *msg, int r);
extern void dumpregions(void);
void printregions(void);
extern void dumprnest(void);
extern void dumpscalar(int s);
extern void dumpscalarlist(int ss);
extern void dumpscalars(void);
extern void dumpstmt(int s);
extern void dumpstmts(void);
extern void dumpvloop(int l);
extern void dumpvloops(void);
extern void dumpsub(int s);
extern void dumpsubs(void);
extern void putsubs(int s1, int n1);
extern void _putsubs(char *name, int s1, int n1);
extern void dumpmr(int m);
extern void dumpmemref(int m);
extern void dumpmemrefs(void);
void dumpinds(void);
extern void dumpvind(int i);
extern void dumpvinds(void);
extern void dumpvindl(int i1, int n);
extern void printili(int i);
void checkfgraph(char *s);
extern void printblocks(void);
extern void printblock(int block);
extern void printfgraph(void);
extern void printfnode(int v);
extern void printilt(int i);
extern void dumparefs(void);
extern void dumparef(int arefx);
extern void putnme(char *s, int nme);
void printregionnest(char *msg, int r);
void dgbl(void);
void dumpdvls(void);
void stackcheck(void);
void stackvars(void);
void dumpilts(void);
void dflg(void);
void dumpaccrout(void);
#endif
