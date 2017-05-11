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

/* clang-format off */

/** \file
 * \brief External declarations for memory management routines defined in alloc.c.
 */

void ENTF90(PTR_ALLOC03,
            ptr_alloc03)(__INT_T *nelem, __INT_T *kind, __INT_T *len,
                         __STAT_T *stat, char **pointer, __POINT_T *offset,
                         __INT_T *firsttime, DCHAR(errmsg) DCLEN(errmsg));

void ENTF90(PTR_SRC_ALLOC03,
            ptr_src_alloc03)(F90_Desc *sd, __INT_T *nelem, __INT_T *kind,
                             __INT_T *len, __STAT_T *stat, char **pointer,
                             __POINT_T *offset, __INT_T *firsttime,
                             DCHAR(errmsg) DCLEN(errmsg));

void ENTF90(DEALLOC03, dealloc03)(__STAT_T *stat, char *area,
                                  __INT_T *firsttime,
                                  DCHAR(errmsg) DCLEN(errmsg));

void ENTF90(DEALLOC_MBR03, dealloc_mbr03)(__STAT_T *stat, char *area,
                                          __INT_T *firsttime,
                                          DCHAR(errmsg) DCLEN(errmsg));
