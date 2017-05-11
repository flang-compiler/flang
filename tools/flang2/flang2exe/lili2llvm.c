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

/**
   \file
   \brief Interface to LLVM intermediate format
 */

#define DECLARE_TY_KIND_AS_INT
#include "gbldefs.h"
#include "error.h"
void *
llgetlldtype(int dtype, int addrspace)
{
  interr("unimplemeted llgetlldtype() called", 0, 4);
  return 0;
}

void *
get_current_l2l_module()
{
  interr("unimplemeted get_current_l2l_module() called", 0, 4);
  return 0;
}

void *
get_current_module()
{

  interr("unimplemeted get_module() called", 0, 4);
  return 0;
}

