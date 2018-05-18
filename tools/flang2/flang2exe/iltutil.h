/*
 * Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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

#ifndef ILTUTIL_H_
#define ILTUTIL_H_

#include "gbldefs.h"
#include <stdio.h>

/**
   \brief ...
 */
int addilt(int after, int ilix);

/**
   \brief ...
 */
int reduce_ilt(int iltx, int ilix);

/**
   \brief ...
 */
void *ccff_ilt_info(int msgtype, char *msgid, int iltx, int bihx, const char *message, ...);

/**
   \brief ...
 */
void delilt(int iltx);

/**
   \brief ...
 */
void dmpilt(int bihx);

/**
   \brief ...
 */
void dump_ilt(FILE *ff, int bihx);

/**
   \brief ...
 */
void ilt_cleanup(void);

/**
   \brief ...
 */
void ilt_init(void);

/**
   \brief ...
 */
void moveilt(int iltx, int before);

/**
   \brief ...
 */
void rdilts(int bihx);

/**
   \brief ...
 */
void relnkilt(int iltx, int bihx);

/**
   \brief ...
 */
void *subccff_ilt_info(void *xparent, int msgtype, char *msgid, int iltx, int bihx, const char *message, ...);

/**
   \brief ...
 */
void unlnkilt(int iltx, int bihx, bool reuse);

/**
   \brief ...
 */
void wrilts(int bihx);

#endif // ILTUTIL_H_
