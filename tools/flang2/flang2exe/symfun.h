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

#ifdef __cplusplus

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
  if (DTY(dtype) == TY_VECT)
    return true;
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

inline int DTyParamList(DTYPE dtype) {
  Precond(DTY(dtype) == TY_PFUNC);
  Precond(DTyValidRange(dtype));
  return static_cast<int>(unsafeDTY(static_cast<int>(dtype) + 2));
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
#define DTySet(D,E)          (DTY(D) = (E))
#define DTySetFst(D,E)       (DTY((D) + 1) = (E))

#define SptrValidRange(S)    (((S) > NOSYM) && ((unsigned)(S) < stb.stg_avail))

#endif // __cplusplus

#define IM_null ((ILM_OP)0)

#endif // FORTRAN_SYMFUN_H_
