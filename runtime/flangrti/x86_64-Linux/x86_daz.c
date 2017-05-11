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

/* -Mdaz run-time support.
 * For fortran and C main programs, the compiler will add the address of
 * the support routine to ctors
 */

void
__daz(void)
{
#ifdef TARGET_LINUX_X8664
  __asm__("pushq	%rax");
  __asm__("stmxcsr	(%rsp)");
  __asm__("popq	%rax");
  __asm__("orq	$64, %rax");
  __asm__("pushq	%rax");
  __asm__("ldmxcsr	(%rsp)");
  __asm__("popq	%rax");
#endif
}

