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

#ifndef DINITUTL_H_
#define DINITUTL_H_

#include "gbldefs.h"
#include "error.h"
#include "global.h"
#include "dinit.h"

/**
   \brief ...
 */
bool df_is_open(void);

/**
   \brief ...
 */
DREC *dinit_read(void);

/**
   \brief ...
 */
long dinit_ftell(void);

/**
   \brief ...
 */
void dinit_end(void);

/**
   \brief ...
 */
void dinit_fseek(long off);

/**
   \brief ...
 */
void dinit_fskip(long off);

/**
   \brief ...
 */
void dinit_init(void);

/**
   \brief ...
 */
void dinit_put(int dtype, ISZ_T conval);

/**
   \brief ...
 */
void dinit_put_string(ISZ_T len, char *str);

/**
   \brief ...
 */
void dinit_read_string(ISZ_T len, char *str);

/**
   \brief ...
 */
void dinit_restore(void);

/**
   \brief ...
 */
void dinit_save(void);

#endif
