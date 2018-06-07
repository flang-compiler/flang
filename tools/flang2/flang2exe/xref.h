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

#ifndef XREF_H_
#define XREF_H_

#include "gbldefs.h"
#include "global.h"

/**
   \brief ...
 */
void par_xref_put(int lineno, SPTR sym, int sc);

/**
   \brief ...
 */
void par_xref(void);

/**
   \brief ...
 */
void xrefinit(void);

/**
   \brief ...
 */
void xrefput(SPTR symptr, int usage);

/**
   \brief ...
 */
void xref(void);

#endif // XREF_H_
