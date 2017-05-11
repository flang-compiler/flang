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

#include <stdbool.h>

/** \file
 * \brief Definitions for x-flags handling routines
 *
 * X-flags are an array of integers, some of which are bitvectors and some are
 * just plain values that are used to control compiler's behavior. XBIT macros
 * scattered around the source read those values.
 */

/** \brief Query whether x flag index corresponds to a bit vector
 *
 * \return      true is it is, false if it corresponds to a plain value
 * \param index index into x flag array
 */
bool is_xflag_bitvector(int index);

/** \brief Set x flag value
 *
 * XOR passed value if with existing one if the index corresponds to a
 * bitvector, otherwise just assign it.
 *
 * \param xflags pointer to x flags array
 * \param index  index of the element to modify
 * \param value  mask to apply (or value to set)
 */
void set_xflag_value(int *xflags, int index, int value);

/** \brief Unset x flag value
 *
 * Bitwise AND passed value if with existing one if the index corresponds to a
 * bitvector, otherwise just set it to zero.
 *
 * \param xflags pointer to x flags array
 * \param index  index of the element to modify
 * \param value  mask to unset (if value is a bit vector)
 */
void unset_xflag_value(int *xflags, int index, int value);
