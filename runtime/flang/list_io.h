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
 *  Function declarations for Fortran list IO (files ldread.c/ldwrite.c).
 */

/** \brief
 * list-directed external file read initialization (defined in ldread.c) 
 *
 * \param type data  type (as defined in pghpft.h) 
 * \param length  # items of type to read
 * \param stride   distance in bytes between items
 * \param item   where to transfer data to
 * \param itemlen
 */
int __f90io_ldr(int type, long length, int stride, char *item, __CLEN_T itemlen);

/** \brief
 *  list-directed external file write initializations (defined in ldwrite.c) 
 *
 * \param type     data type (as defined in pghpft.h)
 * \param length  # items of type to write. May be <= 0 
 * \param stride   distance in bytes between items 
 * \param item   where to transfer data from 
 * \param item_length
 */
int
__f90io_ldw(int type, long length, int stride, char *item, __CLEN_T item_length);
