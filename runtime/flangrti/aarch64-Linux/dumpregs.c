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

#ifndef TARGET_OPENBSD
#include <sys/ucontext.h>

void
dumpregs(gregset_t *regs)
{
}
  

gregset_t *
getRegs(ucontext_t *u)
{ 
  return (gregset_t *)0;
} 

#else

void
dumpregs(void *regs)
{
}

void *
getRegs(void *u)
{
  return (void *)0;
}

#endif
