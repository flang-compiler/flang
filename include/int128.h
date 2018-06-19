/*
 * Copyright (c) 2016-2018, NVIDIA CORPORATION.  All rights reserved.
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
 * \brief Portable 128-bit integer operations
 *
 *  128-bit integer operations implemented in portable code.
 */

#ifndef INT128_H_
#define INT128_H_
#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <stdint.h>

#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ > 8)
typedef __int128 int128_t;
typedef unsigned __int128 uint128_t;
#else
typedef struct int128_t {
  uint32_t part[4]; /* little-endian order */
} int128_t;
#endif

void int128_from_uint64(int128_t *result, uint64_t x);
void int128_from_int64(int128_t *result, int64_t x);

/* Returns true on overflow. */
bool int128_to_uint64(uint64_t *result, const int128_t *x);
bool int128_to_int64(int64_t *result, const int128_t *x);

/* Returns the usual (-1, 0, 1) for (LT, EQ, GT) */
int int128_unsigned_compare(const int128_t *x, const int128_t *y);
int int128_signed_compare(const int128_t *x, const int128_t *y);

int int128_count_leading_zeros(const int128_t *x);

/*
 *  The functions that can overflow return true when it happens.
 */

void int128_ones_complement(int128_t *result, const int128_t *x);
bool int128_twos_complement(int128_t *result, const int128_t *x);

void int128_and(int128_t *result, const int128_t *x, const int128_t *y);
void int128_or(int128_t *result, const int128_t *x, const int128_t *y);
void int128_xor(int128_t *result, const int128_t *x, const int128_t *y);
void int128_shift_left(int128_t *result, const int128_t *x, int count);
void int128_shift_right_logical(int128_t *result, const int128_t *x, int count);

bool int128_unsigned_add(int128_t *result, const int128_t *x,
                         const int128_t *y);
bool int128_signed_add(int128_t *result, const int128_t *x,
                       const int128_t *y);
bool int128_signed_subtract(int128_t *result, const int128_t *x,
                            const int128_t *y);
void int128_unsigned_multiply(int128_t *high, int128_t *low,
                              const int128_t *x, const int128_t *y);
void int128_signed_multiply(int128_t *high, int128_t *low,
                            const int128_t *x, const int128_t *y);
bool int128_unsigned_divide(int128_t *quotient, int128_t *remainder,
                            const int128_t *dividend, const int128_t *divisor);
bool int128_signed_divide(int128_t *quotient, int128_t *remainder,
                          const int128_t *dividend, const int128_t *divisor);

#ifdef __cplusplus
}
#endif
#endif /* INT128_H_ */
