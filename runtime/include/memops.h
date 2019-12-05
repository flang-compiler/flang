/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 */

/** \file
 * \brief Various memory operations
 */

#ifdef __cplusplus
extern "C" {
#endif

#if defined(INLINE_MEMOPS)
#include <sys/types.h>

static inline void
__attribute__((always_inline))
__c_mzero1(char *dest, long cnt)
{
  (void) __builtin_memset(dest, 0, (size_t) cnt);
}

static inline void
__attribute__((always_inline))
__c_mzero2(short *dest, long cnt)
{
  (void) __builtin_memset(dest, 0, (size_t) cnt * sizeof(short));
}

static inline void
__attribute__((always_inline))
__c_mzero4(int *dest, long cnt)
{
  (void) __builtin_memset(dest, 0, (size_t) cnt * sizeof(int));
}

static inline void
__attribute__((always_inline))
__c_mzero8(long long *dest, long cnt)
{
  (void) __builtin_memset(dest, 0, (size_t) cnt * sizeof(long long));
}

static inline void
__attribute__((always_inline))
__c_mcopy1(char *dest, char *src, long cnt)
{
  (void) __builtin_memcpy(dest, src, (size_t) cnt);
}

static inline void
__attribute__((always_inline))
__c_mcopy2(short *dest, short *src, long cnt)
{
  (void) __builtin_memcpy(dest, src, (size_t) cnt * sizeof(short));
}

static inline void
__attribute__((always_inline))
__c_mcopy4(int *dest, int *src, long cnt)
{
  (void) __builtin_memcpy(dest, src, (size_t) cnt * sizeof(int));
}

static inline void
__attribute__((always_inline))
__c_mcopy8(long long *dest, long long *src, long cnt)
{
  (void) __builtin_memcpy(dest, src, (size_t) cnt * sizeof(long long));
}

static inline void
__attribute__((always_inline))
__c_mset1(char *dest, int value, long cnt)
{
  ssize_t i;
  for (i = 0; i < cnt; ++i)
    dest[i] = (char) value;
}

static inline void
__attribute__((always_inline))
__c_mset2(short *dest, int value, long cnt)
{
  ssize_t i;
  for (i = 0; i < cnt; ++i)
    dest[i] = (short) value;
}

static inline void
__attribute__((always_inline))
__c_mset4(int *dest, int value, long cnt)
{
  ssize_t i;
  for (i = 0; i < cnt; ++i)
    dest[i] = value;
}

static inline void
__attribute__((always_inline))
__c_mset8(long long *dest, long long value, long cnt)
{
  ssize_t i;
  for (i = 0; i < cnt; ++i)
    dest[i] = (long long) value;
}
#else
void __c_mcopy1(char *dest, char *src, long cnt);
void __c_mcopy2(short *dest, short *src, long cnt);
void __c_mcopy4(int *dest, int *src, long cnt);
void __c_mcopy8(long long *dest, long long *src, long cnt);

void __c_mset1(char *dest, int value, long cnt);
void __c_mset2(short *dest, int value, long cnt);
void __c_mset4(int *dest, int value, long cnt);
void __c_mset8(long long *dest, long long value, long cnt);

void __c_mzero1(char *dest, long cnt);
void __c_mzero2(short *dest, long cnt);
void __c_mzero4(int *dest, long cnt);
void __c_mzero8(long long *dest, long cnt);
#endif

#ifdef __cplusplus
} /* extern "C" */
#endif

