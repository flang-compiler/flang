/*
 * Copyright (c) 2017-2018, NVIDIA CORPORATION.  All rights reserved.
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

#ifndef FORTRAN_SYMFUN_H_
#define FORTRAN_SYMFUN_H_

#include "gbldefs.h"
#include "symtab.h"
#ifdef FE90
#undef HAVE_ILI
#else
#include "ili.h"
#define HAVE_ILI
#endif

#ifdef __cplusplus
/* clang-format off */
#ifdef CHECK_PORT
inline void Precond(bool P) {
  assert(P, "precondition failed", 0, ERR_Fatal);
}
#else // !CHECK_PORT
#define Precond(P)
#endif // CHECK_PORT

// ===========
// DTY getters

/// \brief Check if \p dtype is in legal range
inline bool DTyValidRange(DTYPE dtype) {
  return (dtype != DT_NONE) &&
    (static_cast<unsigned>(dtype) < stb.dt.stg_avail);
}

/// \brief Warning: do not use. Use DTY() instead.
inline ISZ_T unsafeDTY(int index) {
  return stb.dt.stg_base[index];
}

#undef DTY
inline TY_KIND DTY(DTYPE dtype) {
  Precond(DTyValidRange(dtype));
  return static_cast<TY_KIND>(unsafeDTY(static_cast<int>(dtype)));
}

inline ISZ_T DTyCharLength(DTYPE dtype) {
  Precond(DTY(dtype) == TY_CHAR || DTY(dtype) == TY_NCHAR);
  Precond(DTyValidRange(dtype));
  return unsafeDTY(static_cast<int>(dtype) + 1);
}

inline bool DTySeqTyValid(DTYPE dtype) {
#ifdef TY_VECT
  if (DTY(dtype) == TY_VECT)
    return true;
#endif
  return (DTY(dtype) == TY_PTR) || (DTY(dtype) == TY_ARRAY);
}

inline DTYPE DTySeqTyElement(DTYPE dtype) {
  Precond(DTySeqTyValid(dtype));
  Precond(DTyValidRange(dtype));
  return static_cast<DTYPE>(unsafeDTY(static_cast<int>(dtype) + 1));
}

inline ISZ_T DTyArrayDesc(DTYPE dtype) {
  Precond(DTY(dtype) == TY_ARRAY);
  Precond(DTyValidRange(dtype));
  return unsafeDTY(static_cast<int>(dtype) + 2);
}

inline void DTySetArrayDesc(DTYPE dtype, ISZ_T desc) {
  Precond(DTY(dtype) == TY_ARRAY);
  Precond(DTyValidRange(dtype));
  stb.dt.stg_base[static_cast<int>(dtype) + 2] = desc;
}

inline SPTR DTyAlgTyMember(DTYPE dtype) {
  Precond(DTY(dtype) == TY_STRUCT || DTY(dtype) == TY_UNION);
  Precond(DTyValidRange(dtype));
  return static_cast<SPTR>(unsafeDTY(static_cast<int>(dtype) + 1));
}

inline ISZ_T DTyAlgTySize(DTYPE dtype) {
  Precond(DTY(dtype) == TY_STRUCT || DTY(dtype) == TY_UNION);
  Precond(DTyValidRange(dtype));
  return unsafeDTY(static_cast<int>(dtype) + 2);
}

inline SPTR unsafeDTyAlgTyTag(DTYPE dtype) {
  Precond(DTyValidRange(dtype));
  return static_cast<SPTR>(unsafeDTY(static_cast<int>(dtype) + 3));
}

inline SPTR DTyAlgTyTag(DTYPE dtype) {
  Precond(DTY(dtype) == TY_STRUCT || DTY(dtype) == TY_UNION);
  return unsafeDTyAlgTyTag(dtype);
}

inline SPTR DTyAlgTyTagNeg(DTYPE dtype) {
  Precond(DTY(dtype) == -TY_STRUCT || DTY(dtype) == -TY_UNION);
  return unsafeDTyAlgTyTag(dtype);
}

inline ISZ_T DTyAlgTyAlign(DTYPE dtype) {
  Precond(DTY(dtype) == TY_STRUCT || DTY(dtype) == TY_UNION);
  Precond(DTyValidRange(dtype));
  return unsafeDTY(static_cast<int>(dtype) + 4);
}

inline int DTyAlgTyInitCon(DTYPE dtype) {
  Precond(DTY(dtype) == TY_STRUCT || DTY(dtype) == TY_UNION);
  Precond(DTyValidRange(dtype));
  return static_cast<int>(unsafeDTY(static_cast<int>(dtype) + 5));
}

inline DTYPE DTyReturnType(DTYPE dtype) {
  Precond(DTY(dtype) == TY_PROC || DTY(dtype) == TY_PFUNC);
  Precond(DTyValidRange(dtype));
  return static_cast<DTYPE>(unsafeDTY(static_cast<int>(dtype) + 1));
}

inline SPTR DTyInterface(DTYPE dtype) {
  Precond(DTY(dtype) == TY_PROC);
  Precond(DTyValidRange(dtype));
  return static_cast<SPTR>(unsafeDTY(static_cast<int>(dtype) + 2));
}

inline int DTyParamCount(DTYPE dtype) {
  Precond(DTY(dtype) == TY_PROC);
  Precond(DTyValidRange(dtype));
  return static_cast<int>(unsafeDTY(static_cast<int>(dtype) + 3));
}

inline int DTyParamDesc(DTYPE dtype) {
  Precond(DTY(dtype) == TY_PROC);
  Precond(DTyValidRange(dtype));
  return static_cast<int>(unsafeDTY(static_cast<int>(dtype) + 4));
}

inline SPTR DTyFuncVal(DTYPE dtype) {
  Precond(DTY(dtype) == TY_PROC);
  Precond(DTyValidRange(dtype));
  return static_cast<SPTR>(unsafeDTY(static_cast<int>(dtype) + 5));
}

inline DTYPE DTyParamList(DTYPE dtype) {
  Precond(DTY(dtype) == TY_PFUNC);
  Precond(DTyValidRange(dtype));
  return static_cast<DTYPE>(unsafeDTY(static_cast<int>(dtype) + 2));
}

inline DTYPE DTyArgType(DTYPE dtype) {
  Precond(DTY(dtype) == TY_PARAM);
  Precond(DTyValidRange(dtype));
  return static_cast<DTYPE>(unsafeDTY(static_cast<int>(dtype) + 1));
}

inline SPTR DTyArgSym(DTYPE dtype) {
  Precond(DTY(dtype) == TY_PARAM);
  Precond(DTyValidRange(dtype));
  return static_cast<SPTR>(unsafeDTY(static_cast<int>(dtype) + 2));
}

inline DTYPE DTyArgNext(DTYPE dtype) {
  Precond(DTY(dtype) == TY_PARAM);
  Precond(DTyValidRange(dtype));
  return static_cast<DTYPE>(unsafeDTY(static_cast<int>(dtype) + 3));
}

#ifdef TY_VECT
inline ISZ_T DTyVecLength(DTYPE dtype) {
  Precond(DTY(dtype) == TY_VECT);
  Precond(DTyValidRange(dtype));
  return unsafeDTY(static_cast<int>(dtype) + 2);
}

inline void DTySetVecLength(DTYPE dtype, ISZ_T length) {
  Precond(DTY(dtype) == TY_VECT);
  Precond(DTyValidRange(dtype));
  stb.dt.stg_base[static_cast<int>(dtype) + 2] = length;
}
#endif

#undef DDTG
inline DTYPE DDTG(DTYPE dtype) {
  Precond(DTyValidRange(dtype));
  return (DTY(dtype) == TY_ARRAY) ? DTySeqTyElement(dtype) : dtype;
}

#undef DTYG
inline TY_KIND DTYG(DTYPE dtype) {
  Precond(DTyValidRange(dtype));
  return DTY((DTY(dtype) == TY_ARRAY) ? DTySeqTyElement(dtype) : dtype);
}

#undef AD_DPTR
inline ADSC *AD_DPTR(DTYPE dtype) {
  return reinterpret_cast<ADSC*>(&aux.arrdsc_base[DTyArrayDesc(dtype)]);
}

inline bool SptrValidRange(SPTR sptr) {
  return (sptr > NOSYM) && (sptr < static_cast<SPTR>(stb.stg_avail));
}

#undef AD_PTR
inline ADSC *AD_PTR(SPTR sptr) {
  Precond(SptrValidRange(sptr));
  return AD_DPTR(DTYPEG(sptr));
}

/// \brief Unchecked setting of element at dtype+0 in the dtype table
inline void DTySet(DTYPE dtype, ISZ_T val) {
  stb.dt.stg_base[static_cast<int>(dtype)] = val;
}

/// \brief Unchecked setting of element at dtype+1 in the dtype table
inline void DTySetFst(DTYPE dtype, ISZ_T val) {
  DTySet(static_cast<DTYPE>(static_cast<int>(dtype) + 1), val);
}

inline void DTySetAlgTySize(DTYPE dtype, ISZ_T val) {
  DTySet(static_cast<DTYPE>(static_cast<int>(dtype) + 2), val);
}

inline void DTySetAlgTyTag(DTYPE dtype, SPTR tag) {
  DTySet(static_cast<DTYPE>(static_cast<int>(dtype) + 3), tag);
}

inline void DTySetAlgTyAlign(DTYPE dtype, ISZ_T val) {
  DTySet(static_cast<DTYPE>(static_cast<int>(dtype) + 4), val);
}

inline void DTySetAlgTyICT(DTYPE dtype, ISZ_T val) {
  DTySet(static_cast<DTYPE>(static_cast<int>(dtype) + 5), val);
}

/// \brief Warning: do not use! Use DTySetAlgTy() instead.
inline void unsafeSetAlgTy(DTYPE dtype, SPTR member, ISZ_T size, SPTR tag,
                           ISZ_T align) {
  DTySetFst(dtype, member);
  DTySet(static_cast<DTYPE>(static_cast<int>(dtype) + 2), size);
  DTySet(static_cast<DTYPE>(static_cast<int>(dtype) + 3), tag);
  DTySet(static_cast<DTYPE>(static_cast<int>(dtype) + 4), align);
}

/// \brief Initialize an algebraic type
inline void DTySetAlgTy(DTYPE dtype, SPTR member, ISZ_T size, SPTR tag,
                        ISZ_T align, ISZ_T ict) {
  Precond(DTY(dtype) == TY_STRUCT || DTY(dtype) == TY_UNION);
  unsafeSetAlgTy(dtype, member, size, tag, align);
  DTySet(static_cast<DTYPE>(static_cast<int>(dtype) + 5), ict);
}

inline void DTySetProcTy(DTYPE dtype, DTYPE subty, SPTR iface, ISZ_T numParams,
                         ISZ_T dpdsc, SPTR fval) {
  Precond(DTY(dtype) == TY_PROC);
  DTySetFst(dtype, subty);
  DTySet(static_cast<DTYPE>(static_cast<int>(dtype) + 2), iface);
  DTySet(static_cast<DTYPE>(static_cast<int>(dtype) + 3), numParams);
  DTySet(static_cast<DTYPE>(static_cast<int>(dtype) + 4), dpdsc);
  DTySet(static_cast<DTYPE>(static_cast<int>(dtype) + 5), fval);
}

inline void DTySetInterface(DTYPE dtype, SPTR iface) {
  DTySet(static_cast<DTYPE>(static_cast<int>(dtype) + 2), iface);
}

inline void DTySetParamDesc(DTYPE dtype, ISZ_T dpdsc) {
  DTySet(static_cast<DTYPE>(static_cast<int>(dtype) + 4), dpdsc);
}

inline void DTySetFuncVal(DTYPE dtype, SPTR fval) {
  DTySet(static_cast<DTYPE>(static_cast<int>(dtype) + 5), fval);
}

inline void DTySetParamList(DTYPE dtype, DTYPE param) {
  Precond(DTY(dtype) == TY_PFUNC);
  Precond(DTyValidRange(dtype));
  stb.dt.stg_base[static_cast<int>(dtype) + 2] = param;
}

// ===========
// ILI getters

#ifdef HAVE_ILI

inline bool ILIIsValid(int ilix) {
  return true; // FIXME
}

inline ILI_OP ILIOpcode(int ilix) {
  Precond(ILIIsValid(ilix));
  return ILI_OPC(ilix);
}

inline bool ILIIsConstant(int ilix) {
  Precond(ILIIsValid(ilix));
  return IL_TYPE(ILIOpcode(ilix)) == ILTY_CONS;
}

inline SPTR ILIConstantSymbol(int ilix) {
  Precond(ILIIsConstant(ilix));
  return static_cast<SPTR>(ILI_OPND(ilix, 1));
}

#endif // HAVE_ILI

// ===========
// STB getters

#define ST_GetterInstance(OrigMacro, ReturnType, Suffix)        \
  inline ReturnType STGet ## Suffix (int index) {               \
    return static_cast<ReturnType>(OrigMacro(index));           \
  }

ST_GetterInstance(ALTNAMEG, SPTR, AlternateName)
#undef ALTNAMEG
#define ALTNAMEG(X) STGetAlternateName(X)

ST_GetterInstance(ARGTYPG, DTYPE, ArgumentType)
#undef ARGTYPG
#define ARGTYPG(X) STGetArgumentType(X)

ST_GetterInstance(BASESYMG, SPTR, BaseSymbol)
#undef BASESYMG
#define BASESYMG(X) STGetBaseSymbol(X)

ST_GetterInstance(BEGINSCOPELABG, SPTR, BeginScopeLabel)
#undef BEGINSCOPELABG
#define BEGINSCOPELABG(X) STGetBeginScopeLabel(X)

ST_GetterInstance(CLENG, SPTR, CLength)
#undef CLENG
#define CLENG(X) STGetCLength(X)

ST_GetterInstance(CMEMFG, SPTR, CMemF)
#undef CMEMFG
#define CMEMFG(X) STGetCMemF(X)

ST_GetterInstance(CMEMLG, SPTR, CMemL)
#undef CMEMLG
#define CMEMLG(X) STGetCMemL(X)

ST_GetterInstance(CONVAL1G, SPTR, Pointee)

ST_GetterInstance(DEFLABG, SPTR, DefLab)
#undef DEFLABG
#define DEFLABG(X) STGetDefLab(X)

ST_GetterInstance(DEVCOPYG, SPTR, DeviceCopy)
#undef DEVCOPYG
#define DEVCOPYG(X) STGetDeviceCopy(X)

ST_GetterInstance(ENCLFUNCG, SPTR, EnclosingFunction)
#undef ENCLFUNCG
#define ENCLFUNCG(X) STGetEnclosingFunction(X)

ST_GetterInstance(ENDLABG, SPTR, EndLabel)
#undef ENDLABG
#define ENDLABG(X) STGetEndLabel(X)

ST_GetterInstance(ENDSCOPELABG, SPTR, EndScopeLabel)
#undef ENDSCOPELABG
#define ENDSCOPELABG(X) STGetEndScopeLabel(X)

ST_GetterInstance(FMTPTG, SPTR, Fmtpt)
#undef FMTPTG
#define FMTPTG(X) STGetFmtpt(X)

ST_GetterInstance(FVALG, SPTR, FValue)
#undef FVALG
#define FVALG(X) STGetFValue(X)

ST_GetterInstance(GCMPLXG, SPTR, GComplex)
#undef GCMPLXG
#define GCMPLXG(X) STGetGComplex(X)

ST_GetterInstance(GDBLEG, SPTR, GDouble)
#undef GDBLEG
#define GDBLEG(X) STGetGDouble(X)

ST_GetterInstance(GDCMPLXG, SPTR, GDoubleComplex)
#undef GDCMPLXG
#define GDCMPLXG(X) STGetGDoubleComplex(X)

ST_GetterInstance(GINTG, SPTR, GInteger)
#undef GINTG
#define GINTG(X) STGetGInteger(X)

ST_GetterInstance(GINT8G, SPTR, GIntegerEight)
#undef GINT8G
#define GINT8G(X) STGetGIntegerEight(X)

ST_GetterInstance(GREALG, SPTR, GReal)
#undef GREALG
#define GREALG(X) STGetGReal(X)

ST_GetterInstance(GSAMEG, SPTR, GSame)
#undef GSAMEG
#define GSAMEG(X) STGetGSame(X)

ST_GetterInstance(GSINTG, SPTR, GSInt)
#undef GSINTG
#define GSINTG(X) STGetGSInt(X)

ST_GetterInstance(INMODULEG, SPTR, InModule)
#undef INMODULEG
#define INMODULEG(X) STGetInModule(X)

ST_GetterInstance(INTTYPG, DTYPE, IntType)
#undef INTTYPG
#define INTTYPG(X) STGetIntType(X)

ST_GetterInstance(MIDNUMG, SPTR, MidNum)
#undef MIDNUMG
#define MIDNUMG(X) STGetMidNum(X)

ST_GetterInstance(ORIGDUMMYG, SPTR, OrigDummy)
#undef ORIGDUMMYG
#define ORIGDUMMYG(X) STGetOrigDummy(X)

ST_GetterInstance(PSMEMG, SPTR, PsMem)
#undef PSMEMG
#define PSMEMG(X) STGetPsMem(X)

ST_GetterInstance(SCOPEG, SPTR, Scope)
#undef SCOPEG
#define SCOPEG(X) STGetScope(X)

ST_GetterInstance(SDSCG, SPTR, SDSC)
#undef SDSCG
#define SDSCG(X) STGetSDSC(X)

ST_GetterInstance(STARTLABG, SPTR, StartLabel)
#undef STARTLABG
#define STARTLABG(X) STGetStartLabel(X)

ST_GetterInstance(TASKDUPG, SPTR, TaskDup)
#undef TASKDUPG
#define TASKDUPG(X) STGetTaskDup(X)

ST_GetterInstance(TDLNKG, SPTR, TdLink)
#undef TDLNKG
#define TDLNKG(X) STGetTdLink(X)

ST_GetterInstance(TPLNKG, SPTR, TpLink)
#undef TPLNKG
#define TPLNKG(X) STGetTpLink(X)

ST_GetterInstance(THPRVTOPTG, SPTR, ThreadPrivate)
#undef THPRVTOPTG
#define THPRVTOPTG(X) STGetThreadPrivate(X)

ST_GetterInstance(TYPDEF_INITG, SPTR, TypedefInit)
#undef TYPDEF_INITG
#define TYPDEF_INITG(X) STGetTypedefInit(X)

ST_GetterInstance(VARIANTG, SPTR, Variant)
#undef VARIANTG
#define VARIANTG(X) STGetVariant(X)

ST_GetterInstance(VTABLEG, SPTR, VTable)
#undef VTABLEG
#define VTABLEG(X) STGetVTable(X)

ST_GetterInstance(XREFLKG, ISZ_T, DsrtInit)
ST_GetterInstance(XREFLKG, SPTR, CrossRefLink)
#undef XREFLKG
#define XREFLKG(X) STGetCrossRefLink(X)

#else // !__cplusplus

#define DTyValidRange(D) (((D) > DT_NONE) && ((unsigned)(D) < stb.dt.stg_avail))
#define DTyCharLength(D)     DTY((D) + 1)
#define DTySeqTyElement(D)   DTY((D) + 1)
#define DTyArrayDesc(D)      DTY((D) + 2)
#define DTySetArrayDesc(D,E) (DTY((D) + 2) = (E))
#define DTyVecLength(D)      DTY((D) + 2)
#define DTySetVecLength(D,E) (DTY((D) + 2) = (E))
#define DTyAlgTyMember(D)    DTY((D) + 1)
#define DTyAlgTySize(D)      DTY((D) + 2)
#define DTyAlgTyTag(D)       DTY((D) + 3)
#define DTyAlgTyAlign(D)     DTY((D) + 4)
#define DTyAlgTyInitCon(D)   DTY((D) + 5)
#define DTyAlgTyTagNeg(D)    DTyAlgTyTag(D)
#define DTyArgType(D)        DTY((D) + 1)
#define DTyArgSym(D)         DTY((D) + 2)
#define DTyArgNext(D)        DTY((D) + 3)
#define DTyReturnType(D)     DTY((D) + 1)
#define DTyInterface(D)      DTY((D) + 2)
#define DTyParamCount(D)     DTY((D) + 3)
#define DTyParamDesc(D)      DTY((D) + 4)
#define DTyFuncVal(D)        DTY((D) + 5)
#define DTyParamList(D)      DTY((D) + 2)
#define DTySetParamList(D,E) (DTY((D) + 2) = (E))

#define DTySet(D,E)           (DTY(D) = (E))
#define DTySetFst(D,E)        (DTY((D) + 1) = (E))
#define DTySetAlgTySize(D,E)  (DTY((D) + 2) = (E))
#define DTySetAlgTyTag(D,E)   (DTY((D) + 3) = (E))
#define DTySetAlgTyAlign(D,E) (DTY((D) + 4) = (E))
#define DTySetAlgTyICT(D,E)   (DTY((D) + 5) = (E))
#define DTySetInterface(D,E)  (DTY((D) + 2) = (E))
#define DTySetParamDesc(D,E)  (DTY((D) + 4) = (E))
#define DTySetFuncVal(D,E)    (DTY((D) + 5) = (E))

#define DTySetAlgTy(D,M,S,T,A,F)                \
  { DTY((D) + 1) = (M);                         \
    DTY((D) + 2) = (S);                         \
    DTY((D) + 3) = (T);                         \
    DTY((D) + 4) = (A);                         \
    DTY((D) + 5) = (F); } 

#define DTySetProcTy(D,S,I,C,E,F) \
  { DTY((D) + 1) = (S);           \
    DTY((D) + 2) = (I);           \
    DTY((D) + 3) = (C);           \
    DTY((D) + 4) = (E);           \
    DTY((D) + 5) = (F); } 

#define SptrValidRange(S)    (((S) > NOSYM) && ((unsigned)(S) < stb.stg_avail))

#ifdef HAVE_ILI

#define ILIOpcode(I)         ILI_OPC(I)
#define ILIConstantSymbol(I) ILI_OPND(I, 1)

#endif // HAVE_ILI

#define STGetPointee(S)      CONVAL1G(S)

#endif // __cplusplus

#define IM_null ((ILM_OP)0)

#endif // FORTRAN_SYMFUN_H_
