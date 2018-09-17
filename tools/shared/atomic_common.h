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


#ifndef ATOMIC_COMMON_H_
#define ATOMIC_COMMON_H_

/**
 * \brief Memory reference size/type codes.
 *
 * Legacy assumptions observed:
 *   -  (code & 3) < 2 if and only if the type size is 1 or 2 bytes.
 *   -  (code & 3) == log2(type size) if the type size is 1, 2, 4, or 8 bytes.
 */
typedef enum MSZ {
  MSZ_SBYTE = 0x00,  /* signed byte */
  MSZ_SHWORD = 0x01, /* signed 16-bit short */
  MSZ_UBYTE = 0x04,  /* unsigned byte */
  MSZ_UHWORD = 0x05, /* unsigned 16-bit short */

  /* Codes for types larger than two bytes. These are all distinct values
   * suitable for use as case labels in switches.  The holes in this sequence
   * of code values avoid violating the first legacy assumption described above.
   */
  MSZ_SWORD = 0x02,  /* signed 32-bit int */
  MSZ_SLWORD = 0x03, /* signed 64-bit long */
  MSZ_UWORD = 0x06,  /* unsigned 32-bit int */
  MSZ_ULWORD = 0x07, /* unsigned 64-bit long */
  MSZ_FWORD = 0x0a,  /* 32-bit single precision float */
  MSZ_FLWORD = 0x0b, /* 64-bit double precision float */
  MSZ_I8 = 0x0f,     /* distinct 64-bit integer type */
  MSZ_PTR = 0x13,    /* distinct 64-bit pointer type */
  MSZ_F10 = 0x16,    /* X87 FPU 80-bit extended precision */
  MSZ_F16 = 0x17,    /* 128-bit quad precision float */
  MSZ_F32 = 0x1a,    /* 256-bit float */
  MSZ_F8x2 = 0x1b,   /* 128-bit double-double float */

  MSZ_UNDEF = 0xff, /* undefined MSZ code */
} MSZ;

/** Specifies memory order of an atomic operation.
    Values corresponding to C11/C++11 memory orders are guaranteed
    to match those of the target's C11/C++11 header. */
typedef enum MEMORY_ORDER {
  MO_RELAXED,
  MO_CONSUME,
  MO_ACQUIRE,
  MO_RELEASE,
  MO_ACQ_REL,
  MO_SEQ_CST,
  MO_MAX_DEF = MO_SEQ_CST, /**< maximum value with defined meaning */
  MO_UNDEF = 0xFF          /**< denotes "undefined" */
} MEMORY_ORDER;

/** Specifies scope an atomic operation. */
typedef enum SYNC_SCOPE {
  SS_SINGLETHREAD, ///< Synchronize only within a thread (e.g. a signal fence)
  SS_PROCESS       ///< Synchronize with other threads
} SYNC_SCOPE;

/** Specifies source of an atomic operation. */
typedef enum ATOMIC_ORIGIN {
  AORG_CPLUS,                 /**< C++11 or C11 atomic operation */
  AORG_OPENMP,                /**< OpenMP */
  AORG_OPENACC,               /**< OpenACC */
  AORG_MAX_DEF = AORG_OPENACC /**< maximum value with defined meaning */
} ATOMIC_ORIGIN;

/** Specifies a read-modify-write operation. */
typedef enum ATOMIC_RMW_OP {
  AOP_XCHG,
  AOP_ADD,
  AOP_SUB,
  AOP_AND,
  AOP_OR,
  AOP_MIN,
  AOP_MAX,
  AOP_XOR,
  AOP_MUL,
  AOP_DIV,
  AOP_SHR,
  AOP_SHL,
  AOP_EQV,
  AOP_NEQV,
  AOP_MAX_DEF = AOP_XOR, /**< maximum value with defined meaning */
  AOP_UNDEF = 0xFF
} ATOMIC_RMW_OP;

typedef struct CMPXCHG_MEMORY_ORDER {
  MEMORY_ORDER success;
  MEMORY_ORDER failure;
} CMPXCHG_MEMORY_ORDER;

/** Information about an atomic operation. */
typedef struct ATOMIC_INFO {
  MSZ msz; ///< size of memory operand
  ATOMIC_RMW_OP op; ///< AOP_UNDEF except for ATOMICRMWx instructions
  ATOMIC_ORIGIN origin;
  SYNC_SCOPE scope;
} ATOMIC_INFO;

/** True if MEMORY_ORDER m performs an acquire operation.
    MO_CONSUME is considered to perform an acquire. */
#define MO_HAS_ACQUIRE(m) ((m) != MO_RELAXED && (m) != MO_RELEASE)

/** True if MEMORY_ORDER m performs a release operation */
#define MO_HAS_RELEASE(m) ((m) >= MO_RELEASE)

#endif


