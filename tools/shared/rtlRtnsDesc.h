
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

#ifndef FE_RTLRTNSDESC_H_
#define FE_RTLRTNSDESC_H_

/**
 * \file  Runtime Library routine descriptions.
 *
 */
#include "gbldefs.h"

typedef struct {
  char *baseNm;
  char fullNm[64];
  bool I8Descr;
  char largeRetValPrefix[4];
} FtnRteRtn;

#endif /* FE_RTLRTNSDESC_H_ */
