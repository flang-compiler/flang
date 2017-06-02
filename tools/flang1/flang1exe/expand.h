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

/** \file
 * \brief various definitions for the expand module
 */

#include <stdint.h>

/*  DEBUG-controlled -q stuff  */

#define EXPDBG(x, y) (DEBUG && DBGBIT(x, y))

/* storage allocation macros  */

#define EXP_ALLOC(stgb, dt, sz) \
  {                             \
    NEW(stgb.stg_base, dt, sz); \
    stgb.stg_size = sz;         \
  }

#define EXP_MORE(stb, dt, nsz)                                              \
  {                                                                         \
    stb.stg_base =                                                          \
        (dt *)sccrelal((char *)stb.stg_base, ((UINT)((nsz) * sizeof(dt)))); \
    stb.stg_size = nsz;                                                     \
  }

#define EXP_NEED(stb, dt, nsz)      \
  if (stb.stg_avail > stb.stg_size) \
    EXP_MORE(stb, dt, nsz);

#define EXP_FREE(stb) FREE(stb.stg_base)

int is_passbyval_dummy(int);

