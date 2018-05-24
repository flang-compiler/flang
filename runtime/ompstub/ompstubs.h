//===----------------------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.txt for details.
//
//===----------------------------------------------------------------------===//

#ifndef OMP_50
#define OMP_50
#endif

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Lifted from theinal Intel kmp.h */

typedef int32_t kmp_int32;
typedef int64_t kmp_int64;

#if defined(TARGET_X8632) || defined(TARGET_LINUX_ARM32)
typedef int32_t kmp_critical_name[8];  /* must be 32 bytes */
#else
typedef int64_t kmp_critical_name[4]; /* must be 32 bytes and  8-byte aligned */
#endif

/*
 * The ident structure that describes a source location.
 */
typedef struct ident {
  kmp_int32 reserved_1; /**<  might be used in Fortran; see above  */
  kmp_int32 flags;      /**<  also f.flags; KMP_IDENT_xxx flags;
                          KMP_IDENT_KMPC identifies this union member  */
  kmp_int32 reserved_2; /**<  not really used in Fortran any more;
                          see above */
  kmp_int32 reserved_3; /**< source[4] in Fortran, do not use for C++  */
  char const *psource; /**< String describing the source location.
                         The string is composed of semi-colon separated fields
                         which describe the source file, the function and a pair
                         of line numbers that delimit the construct. */
} ident_t;

#include <omp.h>

#ifdef __cplusplus
} /* extern "C" */
#endif

