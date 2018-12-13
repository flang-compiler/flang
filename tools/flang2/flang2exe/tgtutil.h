/*
 * Copyright (c) 2017-2018, NVIDIA CORPORATION.  All rights reserved.
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
 * \brief Various definitions for the libomptarget runtime
 */

#ifndef TGT_RUNTIME_H__
#define TGT_RUNTIME_H__

#include "ompaccel.h"

#define OMPACCEL_DEFAULT_DEVICEID -1

/* TGT API macros and structs */
enum {
  TGT_API_BAD,
  TGT_API_REGISTER_LIB,
  TGT_API_UNREGISTER_LIB,
  TGT_API_TARGET,
  TGT_API_TARGET_NOWAIT,
  TGT_API_TARGET_TEAMS,
  TGT_API_TARGET_TEAMS_NOWAIT,
  TGT_API_TARGET_DATA_BEGIN,
  TGT_API_TARGET_DATA_BEGIN_DEPEND,
  TGT_API_TARGET_DATA_BEGIN_NOWAIT,
  TGT_API_TARGET_DATA_BEGIN_NOWAIT_DEPEND,
  TGT_API_TARGET_DATA_END,
  TGT_API_TARGET_DATA_END_DEPEND,
  TGT_API_TARGET_DATA_END_NOWAIT,
  TGT_API_TARGET_DATA_END_NOWAIT_DEPEND,
  TGT_API_N_ENTRIES /* <-- Always last */
};

typedef struct any_tgt_struct {
  char *name;
  DTYPE dtype;
  int byval;
  int psptr;
} TGT_ST_TYPE;

struct tgt_api_entry_t {
  const char *name;     /* TGT API function name                    */
  const int ret_iliopc; /* TGT API function return value ili opcode */
  const DTYPE ret_dtype;  /* TGT API function return value type       */
};

/**
   \brief Register the file and load cubin file
 */
int ll_make_tgt_register_lib(void);

/**
   \brief Register the file and load cubin file
 */
int ll_make_tgt_register_lib2(void);
/**
   \brief Unregister the file
 */
int ll_make_tgt_unregister_lib(void);

/**
   \brief Start offload for target region
 */
int ll_make_tgt_target(SPTR, int64_t, SPTR);

/**
   \brief Start offload for target teams region
 */
int ll_make_tgt_target_teams(SPTR, int64_t, SPTR, int32_t, int32_t);

/**
   \brief Start target data begin.
 */
int ll_make_tgt_target_data_begin(int, OMPACCEL_TINFO *);

/**
   \brief Finish target data begin.
 */
int ll_make_tgt_target_data_end(int, OMPACCEL_TINFO *);

/**
   \brief create tgt_offload_entry dtype
 */
DTYPE ll_make_tgt_offload_entry(char *);

void init_tgtutil();

#endif /* __TGT_RUNTIME_H__ */
