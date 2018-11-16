/*
 * Copyright (c) 2005-2018, NVIDIA CORPORATION.  All rights reserved.
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

/*  define structures and data for keyword processing: */

typedef struct {
  char *keytext;       /* keyword text in lower case */
  int toktyp;          /* token id (as used in parse tables) */
  LOGICAL nonstandard; /* TRUE if nonstandard (extension to f90) */
} KWORD;

typedef struct {
  int kcount;  /* number of keywords in this table */
  KWORD *kwds; /* pointer to first in array of KWORD */
               /* the following members are filled in by init_ktable() to record
                * the indices of the first and last keywords beginning with a
                * certain letter.   If first[] is zero, there does not exist a
                * keyword which begins with the respective letter.  A nonzero
                * value is the index into the keyword table.
                */
  short first[26]; /* indexed by ('a' ... 'z') - 'a' */
  short last[26];  /* indexed by ('a' ... 'z') - 'a' */
} KTABLE;

/* NOTE:  When entering keywords in the tables, two or more consecutive tokens
 * that can be seen in the grammar should be combined into a single token.
 * This is because the keyword search routine is not designed to extract two
 * or more tokens from a single identifier. For example, consider the
 * statement  DO WHILE ( <expr> ).  One way of looking at the statement is
 * that it begins with the two keywords DO and WHILE.  The routine alpha
 * sees the name DOWHILE; relevant tokens are "do", "doublecomplex", and
 * "doubleprecision".  When the keyword search routine looks at "dowhile",
 * it determines that it should look AFTER "doubleprecision" --- the character
 * 'w' is greater than 'u'.  Consequently, no keyword, in particular DO, is
 * found.   For the statement, DO 10 WHILE ( <expr> ), the keyword DO is
 * found in "do10while";  it is determined that it must look BEFORE
 * "doublecomplex" ('1' < 'u').  DO statements of the form,
 *     DO <var> =
 * are not a problem because of the '='; the keyword DO is found as a special
 * case in routine alpha since there is an "exposed" equals sign.
 *
 * If a keyword is a prefix of another keyword, it is possible that the
 * keyword search routine will not find the 'shorter' prefix of an identifier.
 * E.g.,  real and realign are keywords
 *        The identifier realpi will not be found since 'realpi' > 'realign';
 *        the search routine searches past realign instead of before.
 * For cases like this, the keyword table must only contain the 'shorter'
 * prefix; it's up to alpha() to determine look for the additional keywords;
 * e.g., 'realign' is 'real' followed by 'ign'.
 *
 */

/*
 * When the input is freeform, certain keywords may contain blanks.
 * For those whose 'first' ident is also a keyword, the processing in
 * alpha() will search ahead to determine if what follows is an ident
 * which can be combined with 'first' to form a keyword; e.g.,
 *     end if, end program, etc.
 * There are a few whose 'first' ident is not a keyword.  For these,
 * define special macros (TKF_ ...) and initialize in the keyword
 * table and let alpha() do the rest.  (NOTE that 0 is returned by
 * keyword() if a name is not found in a keyword table).
 */
#define TKF_ARRAY -1
#define TKF_ATOMIC -2
#define TKF_CANCELLATION -3
#define TKF_DISTPAR -4
#define TKF_DOCONCURRENT -5
#define TKF_DOUBLE -6
#define TKF_DOWHILE -7
#define TKF_ENDDISTPAR -8
#define TKF_GO -9
#define TKF_NO -10
#define TKF_SELECT -11
#define TKF_TARGETENTER -12
#define TKF_TARGETEXIT -13
#define TKF_BLOCK -14
#define TKF_ENDBLOCK -15

static KWORD t1[] = {         /* normal keyword table */
                     {"", 0}, /* a keyword index must be nonzero */
                     {"abstract", TK_ABSTRACT},
                     {"accept", TK_ACCEPT},
                     {"align", TK_ALIGN},
                     {"allocatable", TK_ALLOCATABLE},
                     {"allocate", TK_ALLOCATE},
                     {"array", TKF_ARRAY},
                     {"assign", TK_ASSIGN},
                     {"associate", TK_ASSOCIATE},
                     {"asynchronous", TK_ASYNCHRONOUS},
                     {"backspace", TK_BACKSPACE},
                     {"bind", TK_BIND},
                     {"block", TKF_BLOCK},
                     {"blockdata", TK_BLOCKDATA},
                     {"byte", TK_BYTE},
                     {"call", TK_CALL},
                     {"case", TK_CASE},
                     {"casedefault", TK_CASEDEFAULT},
                     {"character", TK_CHARACTER},
                     {"class", TK_CLASS},
                     {"close", TK_CLOSE},
                     {"common", TK_COMMON},
                     {"complex", TK_COMPLEX},
                     {"concurrent", TK_CONCURRENT},
                     {"contains", TK_CONTAINS},
                     {"contiguous", TK_CONTIGUOUS},
                     {"continue", TK_CONTINUE},
                     {"cycle", TK_CYCLE},
                     {"data", TK_DATA},
                     {"deallocate", TK_DEALLOCATE},
                     {"decode", TK_DECODE},
                     {"default", TK_DEFAULT},
                     {"dimension", TK_DIMENSION},
                     {"do", TK_DO},
                     {"doconcurrent", TKF_DOCONCURRENT},
                     {"double", TKF_DOUBLE},
                     {"doublecomplex", TK_DBLECMPLX},
                     {"doubleprecision", TK_DBLEPREC},
                     {"dowhile", TKF_DOWHILE},
                     {"elemental", TK_ELEMENTAL},
                     {"else", TK_ELSE},
                     {"elseforall", TK_FORALL},
                     {"elseif", TK_ELSEIF},
                     {"elsewhere", TK_ELSEWHERE},
                     {"encode", TK_ENCODE},
                     {"end", TK_ENDSTMT},
                     {"endassociate", TK_ENDASSOCIATE},
                     {"endblock", TKF_ENDBLOCK},
                     {"endblockdata", TK_ENDBLOCKDATA},
                     {"enddo", TK_ENDDO},
                     {"endenum", TK_ENDENUM},
                     {"endfile", TK_ENDFILE},
                     {"endforall", TK_ENDFORALL},
                     {"endfunction", TK_ENDFUNCTION},
                     {"endif", TK_ENDIF},
                     {"endinterface", TK_ENDINTERFACE},
                     {"endmap", TK_ENDMAP},
                     {"endmodule", TK_ENDMODULE},
                     {"endprocedure", TK_ENDPROCEDURE},
                     {"endprogram", TK_ENDPROGRAM},
                     {"endselect", TK_ENDSELECT},
                     {"endstructure", TK_ENDSTRUCTURE},
                     {"endsubmodule", TK_ENDSUBMODULE},
                     {"endsubroutine", TK_ENDSUBROUTINE},
                     {"endtype", TK_ENDTYPE},
                     {"endunion", TK_ENDUNION},
                     {"endwhere", TK_ENDWHERE},
                     {"entry", TK_ENTRY},
                     {"enum", TK_ENUM},
                     {"enumerator", TK_ENUMERATOR},
                     {"equivalence", TK_EQUIV},
                     {"exit", TK_EXIT},
                     {"extends", TK_EXTENDS},
                     {"external", TK_EXTERNAL},
                     {"final", TK_FINAL},
                     {"flush", TK_FLUSH},
                     {"forall", TK_FORALL},
                     {"format", TK_FORMAT},
                     {"function", TK_FUNCTION},
                     {"generic", TK_GENERIC},
                     {"go", TKF_GO},
                     {"goto", TK_GOTO},
                     {"if", TK_IF},
                     {"implicit", TK_IMPLICIT},
                     {"import", TK_IMPORT},
                     {"impure", TK_IMPURE},
                     {"include", TK_INCLUDE},
                     {"independent", TK_INDEPENDENT},
                     {"inquire", TK_INQUIRE},
                     {"integer", TK_INTEGER},
                     {"intent", TK_INTENT},
                     {"interface", TK_INTERFACE},
                     {"intrinsic", TK_INTRINSIC},
                     {"local", TK_LOCAL},
                     {"local_init", TK_LOCAL_INIT},
                     {"logical", TK_LOGICAL},
                     {"map", TK_MAP},
                     {"module", TK_MODULE},
                     {"namelist", TK_NAMELIST},
                     {"ncharacter", TK_NCHARACTER},
                     {"no", TKF_NO},
                     {"non_intrinsic", TK_NON_INTRINSIC},
                     {"none", TK_NONE},
                     {"nopass", TK_NOPASS},
                     {"nosequence", TK_NOSEQUENCE},
                     {"nullify", TK_NULLIFY},
                     {"open", TK_OPEN},
                     {"optional", TK_OPTIONAL},
                     {"options", TK_OPTIONS},
                     {"parameter", TK_PARAMETER},
                     {"pass", TK_PASS},
                     {"pause", TK_PAUSE},
                     {"pointer", TK_POINTER},
                     {"print", TK_PRINT},
                     {"private", TK_PRIVATE},
                     {"procedure", TK_PROCEDURE},
                     {"program", TK_PROGRAM},
                     {"protected", TK_PROTECTED},
                     {"public", TK_PUBLIC},
                     {"pure", TK_PURE},
                     {"read", TK_READ},
                     {"real", TK_REAL},
                     {"record", TK_RECORD},
                     {"recursive", TK_RECURSIVE},
                     {"return", TK_RETURN},
                     {"rewind", TK_REWIND},
                     {"save", TK_SAVE},
                     {"select", TKF_SELECT},
                     {"selectcase", TK_SELECTCASE},
                     {"selecttype", TK_SELECTTYPE},
                     {"sequence", TK_SEQUENCE},
                     {"shared", TK_SHARED},
                     {"stop", TK_STOP},
                     {"structure", TK_STRUCTURE},
                     {"submodule", TK_SUBMODULE},
                     {"subroutine", TK_SUBROUTINE},
                     {"target", TK_TARGET},
                     {"then", TK_THEN},
                     {"type", TK_TYPE},
                     {"union", TK_UNION},
                     {"use", TK_USE},
                     {"value", TK_VALUE},
                     {"volatile", TK_VOLATILE},
                     {"wait", TK_WAIT},
                     {"where", TK_WHERE},
                     {"while", TK_WHILE},
                     {"write", TK_WRITE}};

static KWORD t2[] = {         /* logical keywords */
                     {"", 0}, /* a keyword index must be nonzero */
                     {"a", TK_AND, TRUE},
                     {"and", TK_AND, FALSE},
                     {"eq", TK_EQ, FALSE},
                     {"eqv", TK_EQV, FALSE},
                     {"f", TK_LOGCONST, TRUE},
                     {"false", TK_LOGCONST, FALSE},
                     {"ge", TK_GE, FALSE},
                     {"gt", TK_GT, FALSE},
                     {"le", TK_LE, FALSE},
                     {"lt", TK_LT, FALSE},
                     {"n", TK_NOTX, TRUE},
                     {"ne", TK_NE, FALSE},
                     {"neqv", TK_NEQV, FALSE},
                     {"not", TK_NOT, FALSE},
                     {"o", TK_ORX, TRUE},
                     {"or", TK_OR, FALSE},
                     {"t", TK_LOGCONST, TRUE},
                     {"true", TK_LOGCONST, FALSE},
                     {"x", TK_XORX, TRUE},
                     {"xor", TK_XOR, TRUE}};

static KWORD t3[] = {
    /* I/O keywords and ALLOCATE keywords */
    {"", 0}, /* a keyword index must be nonzero */
    {"access", TK_ACCESS},
    {"action", TK_ACTION},
    {"advance", TK_ADVANCE},
    {"align", TK_ALIGN}, /* ... used in ALLOCATE stmt */
    {"asynchronous", TK_ASYNCHRONOUS},
    {"blank", TK_BLANK},
    {"convert", TK_CONVERT},
    {"decimal", TK_DECIMAL},
    {"delim", TK_DELIM},
    {"direct", TK_DIRECT},
    {"disp", TK_DISPOSE},
    {"dispose", TK_DISPOSE},
    {"encoding", TK_ENCODING},
    {"end", TK_END},
    {"eor", TK_EOR},
    {"err", TK_ERR},
    {"errmsg", TK_ERRMSG}, /* ... used in ALLOCATE stmt */
    {"exist", TK_EXIST},
    {"file", TK_FILE},
    {"fmt", TK_FMT},
    {"form", TK_FORM},
    {"formatted", TK_FMTTD},
    {"id", TK_ID},
    {"iolength", TK_IOLENGTH},
    {"iomsg", TK_IOMSG},
    {"iostat", TK_IOSTAT},
    {"mold", TK_MOLD},
    {"name", TK_NAME},
    {"named", TK_NAMED},
    {"newunit", TK_NEWUNIT},
    {"nextrec", TK_NEXTREC},
    {"nml", TK_NML},
    {"number", TK_NUMBER},
    {"opened", TK_OPENED},
    {"pad", TK_PAD},
    {"pending", TK_PENDING},
    {"pinned", TK_PINNED}, /* ... used in ALLOCATE stmt */
    {"pos", TK_POS},
    {"position", TK_POSITION},
    {"read", TK_READ},
    {"readonly", TK_READONLY},
    {"readwrite", TK_READWRITE},
    {"rec", TK_REC},
    {"recl", TK_RECL},
    {"recordsize", TK_RECL}, /* ... identical to RECL    */
    {"round", TK_ROUND},
    {"sequential", TK_SEQUENTIAL},
    {"shared", TK_SHARED},
    {"sign", TK_SIGN},
    {"size", TK_SIZE},
    {"source", TK_SOURCE}, /* ... used in ALLOCATE stmt */
    {"stat", TK_STAT},     /* ... used in ALLOCATE and DEALLOCATE stmts */
    {"status", TK_STATUS},
    {"stream", TK_STREAM},
    {"type", TK_STATUS}, /* ... identical to STATUS  */
    {"unformatted", TK_UNFORMATTED},
    {"unit", TK_UNIT},
    {"write", TK_WRITE},
};

static KWORD t4[] = {         /* keywords appearing within a FORMAT stmt: */
                     {"", 0}, /* a keyword index must be nonzero */
                     /* {"$", TK_DOLLAR}, special case in alpha() */
                     {"a", TK_A},
                     {"b", TK_B},
                     {"bn", TK_BN},
                     {"bz", TK_BZ},
                     {"d", TK_D},
                     {"dc", TK_DC},
                     {"dp", TK_DP},
                     {"dt", TK_DT},
                     {"e", TK_E},
                     {"en", TK_EN},
                     {"es", TK_ES},
                     {"f", TK_F},
                     {"g", TK_G},
                     {"i", TK_I},
                     {"l", TK_L},
                     {"n", TK_N},
                     {"o", TK_O},
                     {"p", TK_P},
                     {"q", TK_Q},
                     {"s", TK_S},
                     {"rc", TK_RC},
                     {"rd", TK_RD},
                     {"rn", TK_RN},
                     {"rp", TK_RP},
                     {"ru", TK_RU},
                     {"rz", TK_RZ},
                     {"sp", TK_SP},
                     {"ss", TK_SS},
                     {"t", TK_T},
                     {"tl", TK_TL},
                     {"tr", TK_TR},
                     {"x", TK_X},
                     {"z", TK_Z}};

static KWORD t5[] = {
    /* keywords appearing within PARALLEL directives */
    {"", 0}, /* a keyword index must be nonzero */
    {"aligned", TK_ALIGNED},
    {"capture", TK_CAPTURE},
    {"chunk", TK_CHUNK},
    {"collapse", TK_COLLAPSE},
    {"compare", TK_COMPARE},
    {"copyin", TK_COPYIN},
    {"copyprivate", TK_COPYPRIVATE},
    {"default", TK_DEFAULT},
    {"defaultmap", TK_DEFAULTMAP},
    {"depend", TK_DEPEND},
    {"device", TK_DEVICE},
    {"dist_schedule", TK_DIST_SCHEDULE},
    {"final", TK_FINAL},
    {"firstprivate", TK_FIRSTPRIVATE},
    {"from", TK_FROM},
    {"grainsize", TK_GRAINSIZE},
    {"if", TK_IF},
    {"inbranch", TK_INBRANCH},
    {"is_device_ptr", TK_IS_DEVICE_PTR},
    {"lastlocal", TK_LASTPRIVATE},
    {"lastprivate", TK_LASTPRIVATE},
    {"linear", TK_LINEAR},
    {"link", TK_LINK},
    {"local", TK_PRIVATE},
    {"map", TK_MP_MAP},
    {"mergeable", TK_MERGEABLE},
    {"mp_schedtype", TK_MP_SCHEDTYPE},
    {"nogroup", TK_NOGROUP},
    {"notinbranch", TK_NOTINBRANCH},
    {"nowait", TK_NOWAIT},
    {"num_tasks", TK_NUM_TASKS},
    {"num_teams", TK_NUM_TEAMS},
    {"num_threads", TK_NUM_THREADS},
    {"ordered", TK_ORDERED},
    {"priority", TK_PRIORITY},
    {"private", TK_PRIVATE},
    {"proc_bind", TK_PROC_BIND},
    {"read", TK_READ},
    {"reduction", TK_REDUCTION},
    {"safelen", TK_SAFELEN},
    {"schedule", TK_SCHEDULE},
    {"share", TK_SHARED},
    {"shared", TK_SHARED},
    {"simd", TK_SIMD},
    {"simdlen", TK_SIMDLEN},
    {"thread_limit", TK_THREAD_LIMIT},
    {"threads", TK_THREADS},
    {"to", TK_TO},
    {"uniform", TK_UNIFORM},
    {"untied", TK_UNTIED},
    {"update", TK_UPDATE},
    {"write", TK_WRITE},
};

static KWORD t6[] = {
    /* keywords beginning OpenMP/PARALLEL directives */
    {"", 0}, /* a keyword index must be nonzero */
    {"atomic", TK_MP_ATOMIC},
    {"barrier", TK_MP_BARRIER},
    {"cancel", TK_MP_CANCEL},
    {"cancellation", TKF_CANCELLATION},
    {"cancellationpoint", TK_MP_CANCELLATIONPOINT},
    {"critical", TK_MP_CRITICAL},
    {"declare", TK_DECLARE},
    {"declarereduction", TK_MP_DECLAREREDUCTION},
    {"declaresimd", TK_MP_DECLARESIMD},
    {"declaretarget", TK_MP_DECLARETARGET},
    {"distribute", TK_MP_DISTRIBUTE},
    {"distributeparallel", TKF_DISTPAR},
    {"distributeparalleldo", TK_MP_DISTPARDO},
    {"distributeparalleldosimd", TK_MP_DISTPARDOSIMD},
    {"do", TK_MP_PDO},
    {"doacross", TK_MP_DOACROSS},
    {"dosimd", TK_MP_DOSIMD},
    {"end", TK_ENDSTMT},
    {"endatomic", TK_MP_ENDATOMIC},
    {"endcritical", TK_MP_ENDCRITICAL},
    {"enddistribute", TK_MP_ENDDISTRIBUTE},
    {"enddistributeparallel", TKF_ENDDISTPAR},
    {"enddistributeparalleldo", TK_MP_ENDDISTPARDO},
    {"enddistributeparalleldosimd", TK_MP_ENDDISTPARDOSIMD},
    {"enddo", TK_MP_ENDPDO},
    {"enddosimd", TK_MP_ENDDOSIMD},
    {"endmaster", TK_MP_ENDMASTER},
    {"endordered", TK_MP_ENDORDERED},
    {"endparallel", TK_MP_ENDPARALLEL},
    {"endparalleldo", TK_MP_ENDPARDO},
    {"endparalleldosimd", TK_MP_ENDPARDOSIMD},
    {"endparallelsections", TK_MP_ENDPARSECTIONS},
    {"endparallelworkshare", TK_MP_ENDPARWORKSHR},
    {"endsections", TK_MP_ENDSECTIONS},
    {"endsimd", TK_MP_ENDSIMD},
    {"endsingle", TK_MP_ENDSINGLE},
    {"endtarget", TK_MP_ENDTARGET},
    {"endtargetdata", TK_MP_ENDTARGETDATA},
    {"endtargetparallel", TK_MP_ENDTARGPAR},
    {"endtargetparalleldo", TK_MP_ENDTARGPARDO},
    {"endtargetparalleldosimd", TK_MP_ENDTARGPARDOSIMD},
    {"endtargetsimd", TK_MP_ENDTARGSIMD},
    {"endtargetteams", TK_MP_ENDTARGTEAMS},
    {"endtargetteamsdistribute", TK_MP_ENDTARGTEAMSDIST},
    {"endtargetteamsdistributeparalleldo", TK_MP_ENDTARGTEAMSDISTPARDO},
    {"endtargetteamsdistributeparalleldosimd", TK_MP_ENDTARGTEAMSDISTPARDOSIMD},
    {"endtargetteamsdistributesimd", TK_MP_ENDTARGTEAMSDISTSIMD},
    {"endtask", TK_MP_ENDTASK},
    {"endtaskgroup", TK_MP_ENDTASKGROUP},
    {"endtaskloop", TK_MP_ENDTASKLOOP},
    {"endtaskloopsimd", TK_MP_ENDTASKLOOPSIMD},
    {"endteams", TK_MP_ENDTEAMS},
    {"endteamsdistribute", TK_MP_ENDTEAMSDIST},
    {"endteamsdistributeparalleldo", TK_MP_ENDTEAMSDISTPARDO},
    {"endteamsdistributeparalleldosimd", TK_MP_ENDTEAMSDISTPARDOSIMD},
    {"endteamsdistributesimd", TK_MP_ENDTEAMSDISTSIMD},
    {"endworkshare", TK_MP_ENDWORKSHARE},
    {"flush", TK_MP_FLUSH},
    {"master", TK_MP_MASTER},
    {"ordered", TK_MP_ORDERED},
    {"parallel", TK_MP_PARALLEL},
    {"paralleldo", TK_MP_PARDO},
    {"paralleldosimd", TK_MP_PARDOSIMD},
    {"parallelsections", TK_MP_PARSECTIONS},
    {"parallelworkshare", TK_MP_PARWORKSHR},
    {"section", TK_MP_SECTION},
    {"sections", TK_MP_SECTIONS},
    {"simd", TK_MP_SIMD},
    {"single", TK_MP_SINGLE},
    {"target", TK_MP_TARGET},
    {"targetdata", TK_MP_TARGETDATA},
    {"targetenter", TKF_TARGETENTER},
    {"targetenterdata", TK_MP_TARGETENTERDATA},
    {"targetexit", TKF_TARGETEXIT},
    {"targetexitdata", TK_MP_TARGETEXITDATA},
    {"targetparallel", TK_MP_TARGPAR},
    {"targetparalleldo", TK_MP_TARGPARDO},
    {"targetparalleldosimd", TK_MP_TARGPARDOSIMD},
    {"targetsimd", TK_MP_TARGSIMD},
    {"targetteams", TK_MP_TARGTEAMS},
    {"targetteamsdistribute", TK_MP_TARGTEAMSDIST},
    {"targetteamsdistributeparalleldo", TK_MP_TARGTEAMSDISTPARDO},
    {"targetteamsdistributeparalleldosimd", TK_MP_TARGTEAMSDISTPARDOSIMD},
    {"targetteamsdistributesimd", TK_MP_TARGTEAMSDISTSIMD},
    {"targetupdate", TK_MP_TARGETUPDATE},
    {"task", TK_MP_TASK},
    {"taskgroup", TK_MP_TASKGROUP},
    {"taskloop", TK_MP_TASKLOOP},
    {"taskloopsimd", TK_MP_TASKLOOPSIMD},
    {"taskwait", TK_MP_TASKWAIT},
    {"taskyield", TK_MP_TASKYIELD},
    {"teams", TK_MP_TEAMS},
    {"teamsdistribute", TK_MP_TEAMSDIST},
    {"teamsdistributeparalleldo", TK_MP_TEAMSDISTPARDO},
    {"teamsdistributeparalleldosimd", TK_MP_TEAMSDISTPARDOSIMD},
    {"teamsdistributesimd", TK_MP_TEAMSDISTSIMD},
    {"threadprivate", TK_MP_THREADPRIVATE},
    {"workshare", TK_MP_WORKSHARE},
};

static KWORD t7[] = {
    /* keywords which begin a 'cdec$' directive */
    {"", 0}, /* a keyword index must be nonzero */
    {"alias", TK_ALIAS},
    {"attributes", TK_ATTRIBUTES},
    {"craydistributepoint", TK_DISTRIBUTEPOINT},
    {"distribute", TK_DISTRIBUTE},
    {"distributepoint", TK_DISTRIBUTEPOINT},
};

static KWORD t8[] = {
    /* keywords which begin other directives */
    {"", 0}, /* a keyword index must be nonzero */
    {"local", TK_LOCAL},
    {"prefetch", TK_PREFETCH},
};

static KWORD t9[] = {
    /* keywords for parsed PGI pragmas */
    {"", 0}, /* a keyword index must be nonzero */
    {"defaultkind", TK_DFLT},
    {"ignore_tkr", TK_IGNORE_TKR},
    {"movedesc", TK_MOVEDESC},
    {"prefetch", TK_PREFETCH},
};

static KWORD t11[] = {
    /* keywords for kernel directives */
    {"", 0}, /* a keyword index must be nonzero */
    {"do", TK_DO},
    {"kernel", TK_KERNEL},
    {"nowait", TK_NOWAIT},
};

static KWORD t12[] = {
    {"", 0}, /* a keyword index must be nonzero */
    {"compare", TK_PGICOMPARE},
};

/* ****  NOTE -- each of these must appear in a call to init_ktable() in
 *               scan_init().
 */
static KTABLE normalkw = {sizeof(t1) / sizeof(KWORD), &t1[0]};
static KTABLE logicalkw = {sizeof(t2) / sizeof(KWORD), &t2[0]};
static KTABLE iokw = {sizeof(t3) / sizeof(KWORD), &t3[0]};
static KTABLE formatkw = {sizeof(t4) / sizeof(KWORD), &t4[0]};
static KTABLE parallelkw = {sizeof(t5) / sizeof(KWORD), &t5[0]};
static KTABLE parbegkw = {sizeof(t6) / sizeof(KWORD), &t6[0]};
static KTABLE deckw = {sizeof(t7) / sizeof(KWORD), &t7[0]};
static KTABLE pragma_kw = {sizeof(t8) / sizeof(KWORD), &t8[0]};
static KTABLE ppragma_kw = {sizeof(t9) / sizeof(KWORD), &t9[0]};
static KTABLE kernel_kw = {sizeof(t11) / sizeof(KWORD), &t11[0]};
static KTABLE pgi_kw = {sizeof(t12) / sizeof(KWORD), &t12[0]};

/* char classification macros */

#undef _CS
#undef _DI
#undef _BL
#undef _HD
#undef _HO

#define _CS 1  /* alpha symbol */
#define _DI 2  /* digit */
#define _BL 4  /* blank */
#define _HD 8  /* hex digit */
#define _HO 16 /* Hollerith constant indicator */

#undef iscsym
#define iscsym(c) (ctable[c] & _CS)
#undef isblank
#define isblank(c) (ctable[c] & _BL)
#undef iswhite
#define iswhite(c) ((c) <= ' ')
#define ishex(c) (ctable[c] & (_HD | _DI))
#define isident(c) (ctable[c] & (_CS | _DI))
#define isdig(c) (ctable[c] & _DI)
#define isodigit(c) ((c) >= '0' && (c) <= '7')
#define isholl(c) (ctable[c] & _HO)

static char ctable[256] = {
    0,         /* nul */
    0,         /* soh */
    0,         /* stx */
    0,         /* etx */
    0,         /* eot */
    0,         /* enq */
    0,         /* ack */
    0,         /* bel */
    0,         /* bs  */
    _BL,       /* ht  */
    0,         /* nl  */
    _BL,       /* vt  */
    _BL,       /* np  */
    _BL,       /* cr  */
    0,         /* so  */
    0,         /* si  */
    0,         /* dle */
    0,         /* dc1 */
    0,         /* dc2 */
    0,         /* dc3 */
    0,         /* dc4 */
    0,         /* nak */
    0,         /* syn */
    0,         /* etb */
    0,         /* can */
    0,         /* em  */
    0,         /* sub */
    0,         /* esc */
    0,         /* fs  */
    0,         /* gs  */
    0,         /* rs  */
    0,         /* us  */
    _BL,       /* sp  */
    0,         /* !  */
    0,         /* "  */
    0,         /* #  */
    _CS,       /* $  */
    0,         /* %  */
    0,         /* &  */
    0,         /* '  */
    0,         /* (  */
    0,         /* )  */
    0,         /* *  */
    0,         /* +  */
    0,         /* ,  */
    0,         /* -  */
    0,         /* .  */
    0,         /* /  */
    _DI,       /* 0  */
    _DI,       /* 1  */
    _DI,       /* 2  */
    _DI,       /* 3  */
    _DI,       /* 4  */
    _DI,       /* 5  */
    _DI,       /* 6  */
    _DI,       /* 7  */
    _DI,       /* 8  */
    _DI,       /* 9  */
    0,         /* :  */
    0,         /* ;  */
    0,         /* <  */
    0,         /* =  */
    0,         /* >  */
    0,         /* ?  */
    0,         /* @  */
    _CS | _HD, /* A  */
    _CS | _HD, /* B  */
    _CS | _HD, /* C  */
    _CS | _HD, /* D  */
    _CS | _HD, /* E  */
    _CS | _HD, /* F  */
    _CS,       /* G  */
    _CS | _HO, /* H  */
    _CS,       /* I  */
    _CS,       /* J  */
    _CS,       /* K  */
    _CS,       /* L  */
    _CS,       /* M  */
    _CS,       /* N  */
    _CS,       /* O  */
    _CS,       /* P  */
    _CS,       /* Q  */
    _CS,       /* R  */
    _CS,       /* S  */
    _CS,       /* T  */
    _CS,       /* U  */
    _CS,       /* V  */
    _CS,       /* W  */
    _CS,       /* X  */
    _CS,       /* Y  */
    _CS,       /* Z  */
    0,         /* [  */
    0,         /* \  */
    0,         /* ]  */
    0,         /* ^  */
    _CS,       /* _  */
    0,         /* `  */
    _CS | _HD, /* a  */
    _CS | _HD, /* b  */
    _CS | _HD, /* c  */
    _CS | _HD, /* d  */
    _CS | _HD, /* e  */
    _CS | _HD, /* f  */
    _CS,       /* g  */
    _CS | _HO, /* h  */
    _CS,       /* i  */
    _CS,       /* j  */
    _CS,       /* k  */
    _CS,       /* l  */
    _CS,       /* m  */
    _CS,       /* n  */
    _CS,       /* o  */
    _CS,       /* p  */
    _CS,       /* q  */
    _CS,       /* r  */
    _CS,       /* s  */
    _CS,       /* t  */
    _CS,       /* u  */
    _CS,       /* v  */
    _CS,       /* w  */
    _CS,       /* x  */
    _CS,       /* y  */
    _CS,       /* z  */
    0,         /* {  */
    0,         /* |  */
    0,         /* }  */
    0,         /* ~  */
    0,         /* del */
};
