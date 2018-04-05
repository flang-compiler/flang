/*
 * Copyright (c) 1994-2018, NVIDIA CORPORATION.  All rights reserved.
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
 *  \brief symbol table for Fortran backend
 */

#include <stdarg.h>

/* clang-format off */
.OC

.ST
/* the following macro depends on stype ordering */
#define ST_ISVAR(s) ((s) >= ST_VAR && (s) <= ST_UNION)

.TY

#define DT_FLOAT DT_REAL
#define TY_FLOAT TY_REAL
#define DT_CPTR DT_ADDR

#define DTY(dt) (stb.dt_base[dt])

/* for fast DT checking -- define table indexed by TY_ */
extern short dttypes[TY_MAX+1];
.TA

#define DDTG(dt) (DTY(dt) == TY_ARRAY ? DTY(dt+1) : dt)
#define DTYG(dt) (DTY(dt) == TY_ARRAY ? DTY(DTY(dt+1)) : DTY(dt))

#define DT_ISINT(dt)	(dttypes[DTY(dt)]&_TY_INT)
#define DT_ISREAL(dt)	(dttypes[DTY(dt)]&_TY_REAL)
#define DT_ISCMPLX(dt)	(dttypes[DTY(dt)]&_TY_CMPLX)
#define DT_ISNUMERIC(dt) (dttypes[DTY(dt)]&(_TY_INT|_TY_REAL|_TY_CMPLX))
#define DT_ISBASIC(dt)	(dttypes[DTY(dt)]&_TY_BASIC)
#define DT_ISUNSIGNED(dt) (dttypes[DTY(dt)]&_TY_UNSIGNED)
#define DT_ISSCALAR(dt)	(dttypes[DTY(dt)]&_TY_SCALAR)
#define DT_ISVEC(dt)	(dttypes[DTY(dt)]&_TY_VEC)
#define DT_ISLOG(dt)	(dttypes[DTY(dt)]&_TY_LOG)
#define DT_ISWORD(dt)	(dttypes[DTY(dt)]&_TY_WORD)
#define DT_ISDWORD(dt)	(dttypes[DTY(dt)]&_TY_DWORD)
#define DT_ISVECT(dt)   (dttypes[DTY(dt)]&_TY_VECT)

#define TY_ISINT(t)	(dttypes[t]&_TY_INT)
#define TY_ISREAL(t)	(dttypes[t]&_TY_REAL)
#define TY_ISCMPLX(t)	(dttypes[t]&_TY_CMPLX)
#define TY_ISNUMERIC(t)	(dttypes[t]&(_TY_INT|_TY_REAL|_TY_CMPLX))
#define TY_ISBASIC(t)	(dttypes[t]&_TY_BASIC)
#define TY_ISUNSIGNED(t) (dttypes[t]&_TY_UNSIGNED)
#define TY_ISSCALAR(t)	(dttypes[t]&_TY_SCALAR)
#define TY_ISLOG(t)	(dttypes[t]&_TY_LOG)
#define TY_ISVEC(t)	(dttypes[t]&_TY_VEC)
#define TY_ISWORD(t)	(dttypes[t]&_TY_WORD)
#define TY_ISDWORD(t)	(dttypes[t]&_TY_DWORD)
#define TY_ISVECT(t)    (dttypes[t]&_TY_VECT)

#define TY_VECT_MAXLEN 16

#define ALIGN(addr, a) ((addr + a) & ~(a))
#define ALIGN_AUTO(addr, a) ((addr) & ~(a))

.Sc

#define SC_AUTO SC_LOCAL
#define SC_ISCMBLK(p)  (p == SC_CMBLK)

.SE

/* redo & add a few macros when BIGOBJects are allowed.
 * The value of the CONVAL2 field is 'assumed' to be a 32-bit;
 * the offset of an address constant is 64-bit.
 */

#undef CONVAL2G
#undef CONVAL2P
#undef CMEMLG
#undef CMEMLP
#undef DEFLABG
#undef DEFLABP
#undef ENDLINEG
#undef ENDLINEP
#undef FUNCLINEG
#undef FUNCLINEP
#undef GSAMEG
#undef GSAMEP
#undef ILMG
#undef ILMP
#define CONVAL2G(s)   (INT)(( stb.stg_base)[s].w14)
#define CONVAL2P(s,v) (INT)(( stb.stg_base)[s].w14 = (v))
#define CMEMLG(s)   (INT)(( stb.stg_base)[s].w14)
#define CMEMLP(s,v) (INT)(( stb.stg_base)[s].w14 = (v))
#define DEFLABG(s)   (INT)(( stb.stg_base)[s].w14)
#define DEFLABP(s,v) (INT)(( stb.stg_base)[s].w14 = (v))
#define ENDLINEG(s)   (INT)(( stb.stg_base)[s].w14)
#define ENDLINEP(s,v) (INT)(( stb.stg_base)[s].w14 = (v))
#define FUNCLINEG(s)   (INT)(( stb.stg_base)[s].w14)
#define FUNCLINEP(s,v) (INT)(( stb.stg_base)[s].w14 = (v))
#define GSAMEG(s)   (INT)(( stb.stg_base)[s].w14)
#define GSAMEP(s,v) (INT)(( stb.stg_base)[s].w14 = (v))
#define ILMG(s)   (INT)(( stb.stg_base)[s].w14)
#define ILMP(s,v) (INT)(( stb.stg_base)[s].w14 = (v))

#undef ORIGDUMMYG
#undef ORIGDUMMYP
#define ORIGDUMMYG(s)   (INT)(( stb.stg_base)[s].w32)
#define ORIGDUMMYP(s,v) (INT)(( stb.stg_base)[s].w32 = (v))

#undef GREALG
#undef GREALP
#undef SFDSCG
#undef SFDSCP
#define GREALG(s)   (INT)(( stb.stg_base)[s].w10)
#define GREALP(s,v) (INT)(( stb.stg_base)[s].w10 = (v))
#define SFDSCG(s)   (INT)(( stb.stg_base)[s].w10)
#define SFDSCP(s,v) (INT)(( stb.stg_base)[s].w10 = (v))

/* overloaded macros accessing shared fields */

#define ACONOFFG(s)   (( stb.stg_base)[s].w14)
#define ACONOFFP(s,v) (( stb.stg_base)[s].w14 = (v))
#define PARAMVALG(s)   (( stb.stg_base)[s].w15)
#define PARAMVALP(s,v) (( stb.stg_base)[s].w15 = (v))
#define DLLG(s)       b3G(s)
#define DLLP(s,v)     b3P(s,v)
#define PDALN_EXPLICIT_0 0xf
#define PDALNG(s)     ((b4G(s)&0x0f) == PDALN_EXPLICIT_0 ? 0 : (b4G(s)&0x0f))
#define PDALNP(s,v)   b4P(s, (b4G(s)&0xf0) | ((v) == 0 ? PDALN_EXPLICIT_0 : (v)))
#define PDALN_IS_DEFAULT(s) ((b4G(s)&0x0f) == 0)
#ifdef PGF90
#define CUDAG(s)      b4G(s)
#define CUDAP(s,v)    b4P(s,v)
#define CUDA_HOST		0x01
#define CUDA_DEVICE		0x02
#define CUDA_GLOBAL		0x04
#define CUDA_BUILTIN		0x08
#define CUDA_GRID		0x10
#define CUDA_CONSTRUCTOR	0x20
#define CUDA_STUB		0x40
 /* b4G and b4P can only be up to 0xFF */
#endif

#define SYMNAME(p)        (stb.n_base + NMPTRG(p))
#define SYMNAMEG(p, buff, len)    len = NMLENG(p); strncpy(buff,SYMNAME(p),len)
#define LOCAL_SYMNAME(p) local_sname(SYMNAME(p))
#define RFCNTI(s) (++RFCNTG(s))
#define RFCNTD(s) (--RFCNTG(s))
#define RETADJG(s)      (( stb.stg_base)[s].w10)
#define RETADJP(s,v)    (( stb.stg_base)[s].w10 = (v))
#define XREFLKG(s)      (( stb.stg_base)[s].w16)
#define XREFLKP(s,v)    (( stb.stg_base)[s].w16 = (v))
#define NOSYM 1

typedef enum{
    ETLS_PROCESS,
    ETLS_TASK,
    ETLS_THREAD,
    ETLS_OMP,
    /* Insert HLS here ?*/
    ETLS_NUM_LEVELS
} etls_levels;

#define IS_TLS(s) (TLSG(s) || ETLSG(s))
#define IS_THREAD_TLS(s) (THREADG(s) &&   IS_TLS(s))
#define IS_THREAD_TP(s)  (THREADG(s) && (!IS_TLS(s)))
#define IS_TLS_WRAPPER(sptr) (0)
#define IS_TLS_GETTER(sptr) IS_TLS(sptr)

#define CMPLXFUNC_C XBIT(49, 0x40000000)

#define DLL_NONE   0x0
#define DLL_EXPORT 0x1
#define DLL_IMPORT 0x2

typedef struct {
    int    numdim;
    int    scheck;
    int    zbase;
    int    sdsc;
    ILM_T  *ilmp;
    struct {
        int mlpyr;
        int lwbd;
        int upbd;
    } b[1];
} ADSC;

#define AD_DPTR(dtype) ((ADSC *)(aux.arrdsc_base+DTY((dtype)+2)))
#define AD_PTR(sptr) ((ADSC *) (aux.arrdsc_base + DTY(DTYPEG(sptr)+2)))
#define AD_NUMDIM(p)  ((p)->numdim)
#define AD_SCHECK(p) ((p)->scheck)
#define AD_ZBASE(p)  ((p)->zbase)
#define AD_SDSC(p)   ((p)->sdsc)
#define AD_ILMP(p)   ((p)->ilmp)
#define AD_MLPYR(p, i) ((p)->b[i].mlpyr)
#define AD_LWBD(p, i)  ((p)->b[i].lwbd)
#define AD_UPBD(p, i)  ((p)->b[i].upbd)
#define AD_NUMELM(p)  ((p)->b[AD_NUMDIM(p)].mlpyr)

typedef struct {
    ISZ_T           stack_addr; /* available address on run-time stack  */
    int             ent_save;	/* sptr:
    				 * o  n10 - to cc array to hold saved ar's and
   				 *    excstat
				 * o  x86 - to cc scalar if multiple entries.
				 */
    short           first_dr;	/* first data reg used as global  */
    short           first_ar;	/* first address reg used as global  */
    short           first_sp;	/* first float reg used as global  */
    short           first_dp;	/* first double reg used as global  */
    int             auto_array; /* static array used for auto vars, else 0 */
    int             ret_var;   	/* sym of return value if passed as arg */
    int             memarg_ptr; /* sym where memarg ptr is saved upon entry */
    int             gr_area;    /* sym of where to save global regs */
    INT             flags;	/* misc. target dependent flags */
    char           *arasgn;	/* local ar (base pointer) ARASGN records */
    char           *regset;	/* target dependent register set info */
    char           *argset;	/* target dependent register set info */
    int             display;    /* sptr to an internal procedure's display
				 * (i.e., the host procedure's stack frame).
				 */
    int             uplevel;    /* sptr to an outlined function contains
                                 * addresses of uplevel variables /
                                 */
    int             cgr;	/* index into the simplfied call graph info */
} ENTRY;

typedef struct {
    int   sptr;
    int   next;
    int   lineno;
} NMLDSC;

#define NML_SPTR(i)   aux.nml_base[i].sptr
#define NML_NEXT(i)   aux.nml_base[i].next
#define NML_LINENO(i) aux.nml_base[i].lineno

/*****  Symbol List Item  *****/

typedef struct {
    int   sptr;
    int   next;
} SYMI;

#define SYMI_SPTR(i) aux.symi_base[i].sptr
#define SYMI_NEXT(i) aux.symi_base[i].next


typedef struct {
    int    sptr;
    INT    conval;
} DVL;

#define DVL_SPTR(i)   aux.dvl_base[i].sptr
#define DVL_CONVAL(i) aux.dvl_base[i].conval

typedef struct {
   int    *dpdsc_base;
   int     dpdsc_size;
   int     dpdsc_avl;
   int    *arrdsc_base;
   int     arrdsc_size;
   int     arrdsc_avl;
   ENTRY  *entry_base;
   int     entry_size;
   int     entry_avail;
   ENTRY  *curr_entry;
   int     strdesc;
   NMLDSC *nml_base;
   int     nml_size;
   int     nml_avl;
   DVL    *dvl_base;
   int     dvl_size;
   int     dvl_avl;
   SYMI   *symi_base;
   int     symi_size;
   int     symi_avl;
   INT    *vcon_base;
   int     vcon_size;
   int     vcon_avl;
   int     parregs;      /* Number of parallel regions  */
   INT    *parsyms_base; /* Symbols in parallel regions */
   int     parsyms_size;
   int     parsyms_avl;
   int     vtypes[TY_MAX+1][TY_VECT_MAXLEN];
} AUX;

#define VCON_CONVAL(i) aux.vcon_base[i]

#include "symacc.h"

/*   symbol table data declarations:  */

extern AUX aux;

/* pointer-sized integer */

#define __POINT_T DT_INT8

/*  declarations required to access switch statement or computed goto lists: */

typedef struct {
    INT  val;
    SPTR clabel;
    int  next;
} SWEL;

extern SWEL *switch_base;

/*   declare external functions from symtab.c and dtypeutil.c:  */

extern void     sym_init (void);
extern void     implicit_int (int);
extern SPTR     getsym(const char *, int);
extern SPTR     getsymbol (const char *);
extern SPTR     getcon (INT *, DTYPE);
extern SPTR     get_acon (SPTR, ISZ_T);
extern SPTR     get_acon3 (SPTR, ISZ_T, DTYPE);
extern int      get_vcon (INT *, int);
extern int      get_vcon0 (int);
extern int      get_vcon1 (int);
extern int      get_vconm0 (int);
extern int      get_vcon_scalar (INT, int);
extern ISZ_T    get_isz_cval(int);
extern INT      sign_extend(INT, int);
extern int      getstring (char *, int);
extern void     newimplicit (int, int, int);
extern void     setimplicit (int);
extern void     reapply_implicit (void);
extern char    *parmprint (int);
extern char    *getprint (int);
extern void     symdentry (FILE *, int);
extern void     symdmp (FILE *, int);
extern void     dmp_socs (int, FILE *);
extern int      adddupsym(int);
extern int      getnewccsym (int, int, int);
extern int      getccsym (int, int, SYMTYPE);
extern int      getccsym_sc (int, int, int, int);
extern int      getcctemp_sc (char *, int, int);
extern int      getccssym (char *, int, int);
extern int      getccssym_sc (char *, int, int, int);
extern int	getccsym_copy(int);
extern int      insert_sym (int);
extern int      insert_sym_first(int);
extern int      getlab (void);
extern int      get_entry_item(void);
extern void     pop_scope (void);
extern void     pop_sym (int);
extern SPTR     mkfunc (const char *);
extern SPTR     mkfunc_cncall(const char *);
/* Private symbol adjustment, works differently (has two separate versions) for
 * native and LLVM backends */
extern void     fix_private_sym(int);

int mk_prototype(char *, char *, DTYPE, int, ...);
int mk_prototype_llvm(char *, char *, DTYPE, int, ...);

extern int      add_symitem(int , int);
extern int      dbg_symdentry (int);
extern int      get_semaphore(void);
extern char    *getsname(int);		/*****  defined in assem.c  *****/
extern char    *getsname2(int);		/*****  defined in assem.c  *****/
extern void     sym_is_refd(int);	/*****  defined in assem.c  *****/

extern ISZ_T    size_of (DTYPE);
extern ISZ_T    size_of_sym (SPTR);
extern int      alignment (DTYPE);
extern int      align_unconstrained(DTYPE);
extern int      alignment_sym (SPTR);
extern void     init_chartab (void);
extern DTYPE    get_type (int, TY_KIND, int);
extern DTYPE    get_array_dtype (int, DTYPE);
extern DTYPE    get_vector_dtype (DTYPE, int);
extern int      cmpat_func (int, int);
extern void     getdtype (DTYPE, char *);
extern ISZ_T    extent_of (DTYPE);
extern ISZ_T    ad_val_of(int);
extern int      get_bnd_con(ISZ_T);
extern ISZ_T    get_bnd_cval(int con);
extern void     dmp_dtype (void);
extern int      dmp_dent (int);
extern int      scale_of (int, INT *);
extern int      Scale_Of (int, ISZ_T *);
extern int      fval_of (int);
extern int      kanji_len (unsigned char *, int);
extern int      kanji_char (unsigned char *, int, int *);
extern int      kanji_prefix (unsigned char *, int, int);
extern int      addnewsym(char*);

/* dtypeutl.c */
LOGICAL is_empty_typedef(DTYPE dtype);
ISZ_T zsize_of(DTYPE dtype);
int align_of(int dtype);
LOGICAL no_data_components(DTYPE dtype);
void Save_Chartab(FILE *);
void Restore_Chartab(FILE *);
DTYPE array_element_dtype(DTYPE);

/* xref.c */
void xrefinit(void);
void xref(void);
void xrefput(int symptr, int usage);

/* llassem.h - should this be moved? */
int runtime_alignment(int syma);

int mk_swtab(INT n, SWEL *swhdr, int deflab, int doinit);
int mk_swtab_ll(INT n, SWEL *swhdr, int deflab, int doinit);
int mk_swlist(INT n, SWEL *swhdr, int doinit);
