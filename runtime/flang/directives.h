/*
 * Copyright (c) 1995-2018, NVIDIA CORPORATION.  All rights reserved.
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
 * \file
 * \brief directives.h - define macros for asm directives
 */

#define	_ASM_CONCAT(l,r) l##r
#define	ASM_CONCAT(l,r) _ASM_CONCAT(l,r)

#if defined(TARGET_WIN_X8664)
#define ENT(n) n
#define ALN_WORD .align 4
#define ALN_FUNC .align 16
#define ALN_DBLE .align 8
#define ALN_QUAD .align 16
#define ELF_FUNC(s) .type ENT(s), @function
#define ELF_OBJ(s) .type ENT(s), @object
#define ELF_SIZE(s) .size ENT(s), .- ENT(s)
#define AS_VER .version "01.01"
#define I1 % rcx
#define I1W % ecx
#define I2 % rdx
#define I2W % edx
#define I3 % r8
#define I3W % r8d
#define I4 % r9
#define F1 % xmm0
#define F2 % xmm1
#define F3 % xmm2
#define F4 % xmm3

#else
#define ENT(n) n
#define ALN_WORD .align 4
#define ALN_FUNC .align 16
#define ALN_DBLE .align 8
#define ALN_QUAD .align 16
#define ELF_FUNC(s) .type ENT(s), @function
#define ELF_OBJ(s) .type ENT(s), @object
#define ELF_SIZE(s) .size ENT(s), .- ENT(s)
#define AS_VER .version "01.01"
#define I1 % rdi
#define I1W % edi
#define I2 % rsi
#define I2W % esi
#define I3 % rdx
#define I3W % edx
#define I4 % rcx
#define F1 % xmm0
#define F2 % xmm1
#define F3 % xmm2
#define F4 % xmm3

#endif

/* macros for handling pic and non-pic code */

#if defined(PG_PIC) && ! defined (TARGET_OSX_X8664)
#define GBLTXT(fn) fn @PLT
#define LDL(var, tmp, lreg)                                                    \
  leaq var(% rip), tmp;                                                        \
  movl(tmp), lreg
#define STL(lreg, tmp, var)                                                    \
  leaq var(% rip), tmp;                                                        \
  movl lreg, (tmp)
#define LDQ(var, tmp, qreg)                                                    \
  leaq var(% rip), tmp;                                                        \
  movq(tmp), qreg
#define STQ(qreg, tmp, var)                                                    \
  leaq var(% rip), tmp;                                                        \
  movq qreg, (tmp)
#define LDDQU(var, tmp, qreg)                                                  \
  leaq var(% rip), tmp;                                                        \
  movdqu(tmp), qreg
#define STDQU(qreg, tmp, var)                                                  \
  leaq var(% rip), tmp;                                                        \
  movdqu qreg, (tmp)
#define LEAQ(var, tmp) leaq var(% rip), tmp
#define XCHGL(lreg, tmp, var)                                                  \
  leaq var(% rip), tmp;                                                        \
  xchgl lreg, (tmp)
#define FNSTCW(tmp, var)                                                       \
  leaq var(% rip), tmp;                                                        \
  fnstcw(tmp)
#define FLDCW(var, tmp)                                                        \
  leaq var(% rip), tmp;                                                        \
  fldcw(tmp)
#else
#define GBLTXT(fn) fn
#define LDL(var, tmp, lreg) movl var(% rip), lreg
#define STL(lreg, tmp, var) movl lreg, var(% rip)
#define LDQ(var, tmp, qreg) movq var(% rip), qreg
#define STQ(qreg, tmp, var) movq qreg, var(% rip)
#define LDDQU(var, tmp, qreg) movdqu var(% rip), qreg
#define STDQU(qreg, tmp, var) movdqu qreg, var(% rip)
#define LEAQ(var, tmp) leaq var(% rip), tmp
#define XCHGL(lreg, tmp, var) xchgl lreg, var(% rip)
#define FNSTCW(tmp, var) fnstcw var(% rip)
#define FLDCW(var, tmp) fldcw var(% rip)
#endif

#define CALL(fn) call GBLTXT(fn)
#define JMP(fn) jmp GBLTXT(fn)

/* macros for handling the red zone of a stack, i.e., the 128-byte area
 * below a leaf function's stack pointer as defined in the linux abi.
 * For other enviroments, the red zone must be allocated.
 * Macros:
 *    RZ_PUSH   -- create the 128-byte redzone
 *    RZ_POP    -- relinquish the 128-byte redzone
 *    RZ_OFF(n) -- produce the offset representing n (positive) bytes below the
 *                 stackpointer wrt the linux abi; e.g., for the linux abi
 *                 RZ_OFF(24)(%rsp) => -24(%rsp); for other systems, the
 *                 offset would be computed as 128-24.
 */

#if defined(TARGET_WIN_X8664)
#define RZ_PUSH subq $128, % rsp
#define RZ_POP addq $128, % rsp
#define RZ_OFF(n) (128## - n)
#else
#define RZ_PUSH
#define RZ_POP
#define RZ_OFF(n) -##n
#endif

#define LDDP movlpd

#ifdef	__LBL
#error	Macro "__LBL" should not be defined
#endif
#define	__LBL(_n,_t) _n ## _ ## _t
#ifdef	_LBL
#error	Macro "_LBL" should not be defined
#endif
#define	_LBL(_n, _m) __LBL(_n, _m)
#ifdef	LBL
#error	Macro "LBL" should not be defined
#endif
#define	LBL(_n) _LBL(_n, NNN)
