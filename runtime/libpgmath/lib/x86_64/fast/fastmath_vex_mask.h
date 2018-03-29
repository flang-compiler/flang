/* 
 * Copyright (c) 2013-2018, NVIDIA CORPORATION.  All rights reserved.
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
 */


/*
 *   __fvd_cos_vex_256_mask(argument, mask)
 *   __fvd_cos_fma4_256_mask(argument, mask)
 * 
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the cosine of the arguments whose mask is non-zero
 *
 */
        .text
	ALN_FUNC
#ifdef FMA4_TARGET
	.globl ENT(__fvd_cos_fma4_256_mask)
ENT(__fvd_cos_fma4_256_mask):
#else
	.globl ENT(__fvd_cos_vex_256_mask)
ENT(__fvd_cos_vex_256_mask):
#endif

/*	RZ_PUSH */
	subq $8, %rsp

        vptest	.L_zeromask(%rip), %ymm1
	je	LBL(.L_fvd_cos_256_done)

	vandpd	%ymm0,%ymm1,%ymm0
#ifdef FMA4_TARGET
	CALL(ENT(__fvd_cos_fma4_256))
#else
	CALL(ENT(__fvd_cos_vex_256))
#endif

LBL(.L_fvd_cos_256_done):

/*	RZ_POP   */
	addq $8, %rsp
	ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_cos_fma4_256_mask)
        ELF_SIZE(__fvd_cos_fma4_256_mask)
#else
        ELF_FUNC(__fvd_cos_vex_256_mask)
        ELF_SIZE(__fvd_cos_vex_256_mask)
#endif

/*
 *   __fvd_cos_vex_mask(argument, mask)
 *   __fvd_cos_fma4_mask(argument, mask)
 * 
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the cosine of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_cos_fma4_mask)
ENT(__fvd_cos_fma4_mask):
#else
        .globl ENT(__fvd_cos_vex_mask)
ENT(__fvd_cos_vex_mask):
#endif

        subq $8, %rsp

        vptest  .L_zeromask(%rip), %xmm1
        je      LBL(.L_fvd_cos_done)

        vandpd  %xmm0,%xmm1,%xmm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvd_cos_fma4))
#else
        CALL(ENT(__fvd_cos_vex))
#endif

LBL(.L_fvd_cos_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_cos_fma4_mask)
        ELF_SIZE(__fvd_cos_fma4_mask)
#else
        ELF_FUNC(__fvd_cos_vex_mask)
        ELF_SIZE(__fvd_cos_vex_mask)
#endif


/*
 *   __fvs_cos_vex_256_mask(argument, mask)
 *
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the cosine of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_cos_fma4_256_mask)
ENT(__fvs_cos_fma4_256_mask):
#else
        .globl ENT(__fvs_cos_vex_256_mask)
ENT(__fvs_cos_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_s_zeromask(%rip), %ymm1
        je      LBL(.L_fvs_cos_256_done)

        vandps %ymm0,%ymm1,%ymm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvs_cos_fma4_256))
#else
        CALL(ENT(__fvs_cos_vex_256))
#endif

LBL(.L_fvs_cos_256_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_cos_fma4_256_mask)
        ELF_SIZE(__fvs_cos_fma4_256_mask)
#else
        ELF_FUNC(__fvs_cos_vex_256_mask)
        ELF_SIZE(__fvs_cos_vex_256_mask)
#endif

/*
 *   __fvs_cos_vex_mask(argument, mask)
 *   __fvs_cos_fma4_mask(argument, mask)
 *
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the cosine of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_cos_fma4_mask)
ENT(__fvs_cos_fma4_mask):
#else
        .globl ENT(__fvs_cos_vex_mask)
ENT(__fvs_cos_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_s_zeromask(%rip), %xmm1
        je      LBL(.L_fvs_cos_done)

        vandps %xmm0,%xmm1,%xmm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvs_cos_fma4))
#else
        CALL(ENT(__fvs_cos_vex))
#endif

LBL(.L_fvs_cos_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_cos_fma4_mask)
        ELF_SIZE(__fvs_cos_fma4_mask)
#else
        ELF_FUNC(__fvs_cos_vex_mask)
        ELF_SIZE(__fvs_cos_vex_mask)
#endif


/*
 *   __fvd_sin_vex_256_mask(argument, mask)
 *   __fvd_sin_fma4_256_mask(argument, mask)
 * 
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the sine of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_sin_fma4_256_mask)
ENT(__fvd_sin_fma4_256_mask):
#else
        .globl ENT(__fvd_sin_vex_256_mask)
ENT(__fvd_sin_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %ymm1
        je      LBL(.L_fvd_sin_256_done)

        vandpd %ymm0,%ymm1,%ymm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvd_sin_fma4_256))
#else
        CALL(ENT(__fvd_sin_vex_256))
#endif

LBL(.L_fvd_sin_256_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_sin_fma4_256_mask)
        ELF_SIZE(__fvd_sin_fma4_256_mask)
#else
        ELF_FUNC(__fvd_sin_vex_256_mask)
        ELF_SIZE(__fvd_sin_vex_256_mask)
#endif

/*
 *   __fvd_sin_vex_mask(argument, mask)
 *   __fvd_sin_fma4_mask(argument, mask)
 * 
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the sine of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_sin_fma4_mask)
ENT(__fvd_sin_fma4_mask):
#else
        .globl ENT(__fvd_sin_vex_mask)
ENT(__fvd_sin_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %xmm1
        je      LBL(.L_fvd_sin_done)

        vandpd %xmm0,%xmm1,%xmm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvd_sin_fma4))
#else
        CALL(ENT(__fvd_sin_vex))
#endif

LBL(.L_fvd_sin_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_sin_fma4_mask)
        ELF_SIZE(__fvd_sin_fma4_mask)
#else
        ELF_FUNC(__fvd_sin_vex_mask)
        ELF_SIZE(__fvd_sin_vex_mask)
#endif


/*
 *   __fvs_sin_vex_256_mask(argument, mask)
 *   __fvs_sin_fma4_256_mask(argument, mask)
 *
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the sine of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_sin_fma4_256_mask)
ENT(__fvs_sin_fma4_256_mask):
#else
        .globl ENT(__fvs_sin_vex_256_mask)
ENT(__fvs_sin_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_s_zeromask(%rip), %ymm1
        je      LBL(.L_fvs_sin_256_done)

        vandps	%ymm0,%ymm1,%ymm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvs_sin_fma4_256))
#else
        CALL(ENT(__fvs_sin_vex_256))
#endif

LBL(.L_fvs_sin_256_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_sin_fma4_256_mask)
        ELF_SIZE(__fvs_sin_fma4_256_mask)
#else
        ELF_FUNC(__fvs_sin_vex_256_mask)
        ELF_SIZE(__fvs_sin_vex_256_mask)
#endif

/*
 *   __fvs_sin_vex_mask(argument, mask)
 *   __fvs_sin_fma4_mask(argument, mask)
 *
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the sine of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_sin_fma4_mask)
ENT(__fvs_sin_fma4_mask):
#else
        .globl ENT(__fvs_sin_vex_mask)
ENT(__fvs_sin_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_s_zeromask(%rip), %xmm1
        je      LBL(.L_fvs_sin_done)

        vandps %xmm0,%xmm1,%xmm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvs_sin_fma4))
#else
        CALL(ENT(__fvs_sin_vex))
#endif

LBL(.L_fvs_sin_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_sin_fma4_mask)
        ELF_SIZE(__fvs_sin_fma4_mask)
#else
        ELF_FUNC(__fvs_sin_vex_mask)
        ELF_SIZE(__fvs_sin_vex_mask)
#endif


/*
 *   __fvd_cosh_vex_256_mask(argument, mask)
 *   __fvd_cosh_fma4_256_mask(argument, mask)
 * 
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the hyperbolic cosine of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_cosh_fma4_256_mask)
ENT(__fvd_cosh_fma4_256_mask):
#else
        .globl ENT(__fvd_cosh_vex_256_mask)
ENT(__fvd_cosh_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %ymm1
        je      LBL(.L_fvd_cosh_256_done)

        vandpd  %ymm0,%ymm1,%ymm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvd_cosh_fma4_256))
#else
        CALL(ENT(__fvd_cosh_vex_256))
#endif

LBL(.L_fvd_cosh_256_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_cosh_fma4_256_mask)
        ELF_SIZE(__fvd_cosh_fma4_256_mask)
#else
        ELF_FUNC(__fvd_cosh_vex_256_mask)
        ELF_SIZE(__fvd_cosh_vex_256_mask)
#endif

/*
 *   __fvd_cosh_vex_mask(argument, mask)
 *   __fvd_cosh_fma4_mask(argument, mask)
 * 
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the hyperbolic cosine of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_cosh_fma4_mask)
ENT(__fvd_cosh_fma4_mask):
#else
        .globl ENT(__fvd_cosh_vex_mask)
ENT(__fvd_cosh_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %xmm1
        je      LBL(.L_fvd_cosh_done)

        vandpd  %xmm0,%xmm1,%xmm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvd_cosh_fma4))
#else
        CALL(ENT(__fvd_cosh_vex))
#endif

LBL(.L_fvd_cosh_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_cosh_fma4_mask)
        ELF_SIZE(__fvd_cosh_fma4_mask)
#else
        ELF_FUNC(__fvd_cosh_vex_mask)
        ELF_SIZE(__fvd_cosh_vex_mask)
#endif

/*
 *   __fvs_cosh_vex_256_mask(argument, mask)
 *   __fvs_cosh_fma4_256_mask(argument, mask)
 *
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the hyperbolic cosine of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_cosh_fma4_256_mask)
ENT(__fvs_cosh_fma4_256_mask):
#else
        .globl ENT(__fvs_cosh_vex_256_mask)
ENT(__fvs_cosh_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_s_zeromask(%rip), %ymm1
        je      LBL(.L_fvs_cosh_256_done)

        vandps	%ymm0,%ymm1,%ymm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvs_cosh_fma4_256))
#else
        CALL(ENT(__fvs_cosh_vex_256))
#endif

LBL(.L_fvs_cosh_256_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_cosh_fma4_256_mask)
        ELF_SIZE(__fvs_cosh_fma4_256_mask)
#else
        ELF_FUNC(__fvs_cosh_vex_256_mask)
        ELF_SIZE(__fvs_cosh_vex_256_mask)
#endif

/*
 *   __fvs_cosh_vex_mask(argument, mask)
 *   __fvs_cosh_fma4_mask(argument, mask)
 *
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the hyperbolic cosine of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_cosh_fma4_mask)
ENT(__fvs_cosh_fma4_mask):
#else
        .globl ENT(__fvs_cosh_vex_mask)
ENT(__fvs_cosh_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_s_zeromask(%rip), %xmm1
        je      LBL(.L_fvs_cosh_done)

        vandps  %xmm0,%xmm1,%xmm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvs_cosh_fma4))
#else
        CALL(ENT(__fvs_cosh_vex))
#endif

LBL(.L_fvs_cosh_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_cosh_fma4_mask)
        ELF_SIZE(__fvs_cosh_fma4_mask)
#else
        ELF_FUNC(__fvs_cosh_vex_mask)
        ELF_SIZE(__fvs_cosh_vex_mask)
#endif


/*
 *   __fvd_sinh_vex_256_mask(argument, mask)
 *   __fvd_sinh_fma4_256_mask(argument, mask)
 * 
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the hypobolic cosine of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_sinh_fma4_256_mask)
ENT(__fvd_sinh_fma4_256_mask):
#else
        .globl ENT(__fvd_sinh_vex_256_mask)
ENT(__fvd_sinh_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %ymm1
        je      LBL(.L_fvd_sinh_256_done)

        vandpd  %ymm0,%ymm1,%ymm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvd_sinh_fma4_256))
#else
        CALL(ENT(__fvd_sinh_vex_256))
#endif

LBL(.L_fvd_sinh_256_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_sinh_fma4_256_mask)
        ELF_SIZE(__fvd_sinh_fma4_256_mask)
#else
        ELF_FUNC(__fvd_sinh_vex_256_mask)
        ELF_SIZE(__fvd_sinh_vex_256_mask)
#endif

/*
 *   __fvd_sinh_vex_mask(argument, mask)
 *   __fvd_sinh_fma4_mask(argument, mask)
 * 
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the hypobolic cosine of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_sinh_fma4_mask)
ENT(__fvd_sinh_fma4_mask):
#else
        .globl ENT(__fvd_sinh_vex_mask)
ENT(__fvd_sinh_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %xmm1
        je      LBL(.L_fvd_sinh_done)

        vandpd  %xmm0,%xmm1,%xmm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvd_sinh_fma4))
#else
        CALL(ENT(__fvd_sinh_vex))
#endif

LBL(.L_fvd_sinh_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_sinh_fma4_mask)
        ELF_SIZE(__fvd_sinh_fma4_mask)
#else
        ELF_FUNC(__fvd_sinh_vex_mask)
        ELF_SIZE(__fvd_sinh_vex_mask)
#endif


/*
 *   __fvs_sinh_vex_256_mask(argument, mask)
 *   __fvs_sinh_fma4_256_mask(argument, mask)
 *
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the hyperbolic sinine of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_sinh_fma4_256_mask)
ENT(__fvs_sinh_fma4_256_mask):
#else
        .globl ENT(__fvs_sinh_vex_256_mask)
ENT(__fvs_sinh_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_s_zeromask(%rip), %ymm1
        je      LBL(.L_fvs_sinh_256_done)

        vandps  %ymm0,%ymm1,%ymm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvs_sinh_fma4_256))
#else
        CALL(ENT(__fvs_sinh_vex_256))
#endif

LBL(.L_fvs_sinh_256_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_sinh_fma4_256_mask)
        ELF_SIZE(__fvs_sinh_fma4_256_mask)
#else
        ELF_FUNC(__fvs_sinh_vex_256_mask)
        ELF_SIZE(__fvs_sinh_vex_256_mask)
#endif

/*
 *   __fvs_sinh_vex_mask(argument, mask)
 *   __fvs_sinh_fma4_mask(argument, mask)
 *
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the hyperbolic sinine of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_sinh_fma4_mask)
ENT(__fvs_sinh_fma4_mask):
#else
        .globl ENT(__fvs_sinh_vex_mask)
ENT(__fvs_sinh_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_s_zeromask(%rip), %xmm1
        je      LBL(.L_fvs_sinh_done)

        vandps  %xmm0,%xmm1,%xmm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvs_sinh_fma4))
#else
        CALL(ENT(__fvs_sinh_vex))
#endif

LBL(.L_fvs_sinh_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_sinh_fma4_mask)
        ELF_SIZE(__fvs_sinh_fma4_mask)
#else
        ELF_FUNC(__fvs_sinh_vex_mask)
        ELF_SIZE(__fvs_sinh_vex_mask)
#endif


/*
 *   __fvs_sincos_vex_256_mask(argument, mask)
 *   __fvs_sincos_fma4_256_mask(argument, mask)
 *
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the sincos of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_sincos_fma4_256_mask)
ENT(__fvs_sincos_fma4_256_mask):
#else
        .globl ENT(__fvs_sincos_vex_256_mask)
ENT(__fvs_sincos_vex_256_mask):
#endif
        subq $8, %rsp

	vmovaps	%ymm1, %ymm3
        vptest  .L_s_zeromask(%rip), %ymm1
        je      LBL(.L_fvs_sincos_256_done)

        vandps  %ymm0,%ymm3,%ymm0
        vandps  %ymm1,%ymm3,%ymm1
#ifdef FMA4_TARGET
        CALL(ENT(__fvs_sincos_fma4_256))
#else
        CALL(ENT(__fvs_sincos_vex_256))
#endif
	addq	$8, %rsp
	ret

LBL(.L_fvs_sincos_256_done):
	vmovaps	%ymm0, %ymm1
        addq	$8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_sincos_fma4_256_mask)
        ELF_SIZE(__fvs_sincos_fma4_256_mask)
#else
        ELF_FUNC(__fvs_sincos_vex_256_mask)
        ELF_SIZE(__fvs_sincos_vex_256_mask)
#endif

/*
 *   __fvs_sincos_vex_mask(argument, mask)
 *   __fvs_sincos_fma4_mask(argument, mask)
 *
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the sincos of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_sincos_fma4_mask)
ENT(__fvs_sincos_fma4_mask):
#else
        .globl ENT(__fvs_sincos_vex_mask)
ENT(__fvs_sincos_vex_mask):
#endif
        subq $8, %rsp

        vmovaps %xmm1, %xmm3
        vptest  .L_s_zeromask(%rip), %xmm1
        je      LBL(.L_fvs_sincos_done)

        vandps  %xmm0,%xmm3,%xmm0
        vandps  %xmm1,%xmm3,%xmm1
#ifdef FMA4_TARGET
        CALL(ENT(__fvs_sincos_fma4))
#else
        CALL(ENT(__fvs_sincos_vex))
#endif
        addq    $8, %rsp
        ret

LBL(.L_fvs_sincos_done):
        vmovaps %xmm0, %xmm1
        addq    $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_sincos_fma4_mask)
        ELF_SIZE(__fvs_sincos_fma4_mask)
#else
        ELF_FUNC(__fvs_sincos_vex_mask)
        ELF_SIZE(__fvs_sincos_vex_mask)
#endif


/*
 *   __fvd_sincos_vex_256_mask(argument, mask)
 *   __fvd_sincos_fma4_256_mask(argument, mask)
 *
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the hypobolic cosine of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_sincos_fma4_256_mask)
ENT(__fvd_sincos_fma4_256_mask):
#else
        .globl ENT(__fvd_sincos_vex_256_mask)
ENT(__fvd_sincos_vex_256_mask):
#endif
        subq $8, %rsp

        vmovapd %ymm1, %ymm3
        vptest  .L_zeromask(%rip), %ymm1
        je      LBL(.L_fvd_sincos_256_done)

        vandpd  %ymm0,%ymm3,%ymm0
        vandpd  %ymm1,%ymm3,%ymm1
#ifdef FMA4_TARGET
        CALL(ENT(__fvd_sincos_fma4_256))
#else
        CALL(ENT(__fvd_sincos_vex_256))
#endif
	addq	$8, %rsp
	ret

LBL(.L_fvd_sincos_256_done):
	vmovapd	%ymm0, %ymm1
        addq	$8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_sincos_fma4_256_mask)
        ELF_SIZE(__fvd_sincos_fma4_256_mask)
#else
        ELF_FUNC(__fvd_sincos_vex_256_mask)
        ELF_SIZE(__fvd_sincos_vex_256_mask)
#endif

/*
 *   __fvd_sincos_vex_mask(argument, mask)
 *   __fvd_sincos_fma4_mask(argument, mask)
 *
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the hypobolic cosine of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_sincos_fma4_mask)
ENT(__fvd_sincos_fma4_mask):
#else
        .globl ENT(__fvd_sincos_vex_mask)
ENT(__fvd_sincos_vex_mask):
#endif
        subq $8, %rsp

        vmovapd %xmm1, %xmm3
        vptest  .L_zeromask(%rip), %xmm1
        je      LBL(.L_fvd_sincos_done)

        vandpd  %xmm0,%xmm3,%xmm0
        vandpd  %xmm1,%xmm3,%xmm1
#ifdef FMA4_TARGET
        CALL(ENT(__fvd_sincos_fma4))
#else
        CALL(ENT(__fvd_sincos_vex))
#endif
        addq    $8, %rsp
        ret

LBL(.L_fvd_sincos_done):
        vmovapd %xmm0, %xmm1
        addq    $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_sincos_fma4_mask)
        ELF_SIZE(__fvd_sincos_fma4_mask)
#else
        ELF_FUNC(__fvd_sincos_vex_mask)
        ELF_SIZE(__fvd_sincos_vex_mask)
#endif


/*
 *   __fvd_exp_vex_256_mask(argument, mask)
 *   __fvd_exp_fma4_256_mask(argument, mask)
 * 
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the exp of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_exp_fma4_256_mask)
ENT(__fvd_exp_fma4_256_mask):
#else
        .globl ENT(__fvd_exp_vex_256_mask)
ENT(__fvd_exp_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %ymm1
        je      LBL(.L_fvd_exp_256_done)

        vandpd  %ymm0,%ymm1,%ymm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvd_exp_fma4_256))
#else
        CALL(ENT(__fvd_exp_vex_256))
#endif

LBL(.L_fvd_exp_256_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_exp_fma4_256_mask)
        ELF_SIZE(__fvd_exp_fma4_256_mask)
#else
        ELF_FUNC(__fvd_exp_vex_256_mask)
        ELF_SIZE(__fvd_exp_vex_256_mask)
#endif

/*
 *   __fvd_exp_vex_mask(argument, mask)
 *   __fvd_exp_fma4_mask(argument, mask)
 * 
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the exp of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_exp_fma4_mask)
ENT(__fvd_exp_fma4_mask):
#else
        .globl ENT(__fvd_exp_vex_mask)
ENT(__fvd_exp_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %xmm1
        je      LBL(.L_fvd_exp_done)

        vandpd  %xmm0,%xmm1,%xmm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvd_exp_fma4))
#else
        CALL(ENT(__fvd_exp_vex))
#endif

LBL(.L_fvd_exp_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_exp_fma4_mask)
        ELF_SIZE(__fvd_exp_fma4_mask)
#else
        ELF_FUNC(__fvd_exp_vex_mask)
        ELF_SIZE(__fvd_exp_vex_mask)
#endif


/*
 *   __fvs_exp_vex_256_mask(argument, mask)
 *   __fvs_exp_fma4_256_mask(argument, mask)
 *
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the exp of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_exp_fma4_256_mask)
ENT(__fvs_exp_fma4_256_mask):
#else
        .globl ENT(__fvs_exp_vex_256_mask)
ENT(__fvs_exp_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_s_zeromask(%rip), %ymm1
        je      LBL(.L_fvs_exp_256_done)

        vandps %ymm0,%ymm1,%ymm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvs_exp_fma4_256))
#else
        CALL(ENT(__fvs_exp_vex_256))
#endif

LBL(.L_fvs_exp_256_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_exp_fma4_256_mask)
        ELF_SIZE(__fvs_exp_fma4_256_mask)
#else
        ELF_FUNC(__fvs_exp_vex_256_mask)
        ELF_SIZE(__fvs_exp_vex_256_mask)
#endif

/*
 *   __fvs_exp_vex_mask(argument, mask)
 *   __fvs_exp_fma4_mask(argument, mask)
 *
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the exp of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_exp_fma4_mask)
ENT(__fvs_exp_fma4_mask):
#else
        .globl ENT(__fvs_exp_vex_mask)
ENT(__fvs_exp_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_s_zeromask(%rip), %xmm1
        je      LBL(.L_fvs_exp_done)

        vandps %xmm0,%xmm1,%xmm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvs_exp_fma4))
#else
        CALL(ENT(__fvs_exp_vex))
#endif

LBL(.L_fvs_exp_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_exp_fma4_mask)
        ELF_SIZE(__fvs_exp_fma4_mask)
#else
        ELF_FUNC(__fvs_exp_vex_mask)
        ELF_SIZE(__fvs_exp_vex_mask)
#endif


/*
 *   __fvd_pow_vex_256_mask(argument1, argument2, mask)
 *   __fvd_pow_fma4_256_mask(argument1, argument2, mask)
 * 
 *   argument:   ymm0, ymm1
 *   mask:       ymm2
 *
 *   Compute the power of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_pow_fma4_256_mask)
ENT(__fvd_pow_fma4_256_mask):
#else
        .globl ENT(__fvd_pow_vex_256_mask)
ENT(__fvd_pow_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %ymm2
        je      LBL(.L_fvd_pow_256_done)

        vmovupd .L_dpow_mask_two(%rip),%ymm3
        vblendvpd %ymm2,%ymm0,%ymm3,%ymm0
        vblendvpd %ymm2,%ymm1,%ymm3,%ymm1


#ifdef FMA4_TARGET
        CALL(ENT(__fvd_pow_fma4_256))
#else
        CALL(ENT(__fvd_pow_vex_256))
#endif

LBL(.L_fvd_pow_256_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_pow_fma4_256_mask)
        ELF_SIZE(__fvd_pow_fma4_256_mask)
#else
        ELF_FUNC(__fvd_pow_vex_256_mask)
        ELF_SIZE(__fvd_pow_vex_256_mask)
#endif

/*
 *   __fvd_pow_vex_mask(argument1, argument2, mask)
 *   __fvd_pow_fma4_mask(argument1, argument2, mask)
 * 
 *   argument:   xmm0, xmm1
 *   mask:       xmm2
 *
 *   Compute the power of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_pow_fma4_mask)
ENT(__fvd_pow_fma4_mask):
#else
        .globl ENT(__fvd_pow_vex_mask)
ENT(__fvd_pow_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %xmm2
        je      LBL(.L_fvd_pow_done)

        vmovupd .L_dpow_mask_two(%rip),%xmm3
        vblendvpd %xmm2,%xmm0,%xmm3,%xmm0
        vblendvpd %xmm2,%xmm1,%xmm3,%xmm1

#ifdef FMA4_TARGET
        CALL(ENT(__fvd_pow_fma4))
#else
        CALL(ENT(__fvd_pow_vex))
#endif

LBL(.L_fvd_pow_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_pow_fma4_mask)
        ELF_SIZE(__fvd_pow_fma4_mask)
#else
        ELF_FUNC(__fvd_pow_vex_mask)
        ELF_SIZE(__fvd_pow_vex_mask)
#endif


/*
 *   __fvs_pow_vex_256_mask(argument1, argument2, mask)
 *   __fvs_pow_fma4_256_mask(argument1, argument2, mask)
 *
 *   argument:   ymm0, ymm1
 *   mask:       ymm2
 *
 *   Compute the power of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_pow_fma4_256_mask)
ENT(__fvs_pow_fma4_256_mask):
#else
        .globl ENT(__fvs_pow_vex_256_mask)
ENT(__fvs_pow_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_s_zeromask(%rip), %ymm2
        je      LBL(.L_fvs_pow_256_done)

        vmovups .L_spow_mask_two(%rip),%ymm3
        vblendvps %ymm2,%ymm0,%ymm3,%ymm0
        vblendvps %ymm2,%ymm1,%ymm3,%ymm1

#ifdef FMA4_TARGET
        CALL(ENT(__fvs_pow_fma4_256))
#else
        CALL(ENT(__fvs_pow_vex_256))
#endif

LBL(.L_fvs_pow_256_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_pow_fma4_256_mask)
        ELF_SIZE(__fvs_pow_fma4_256_mask)
#else
        ELF_FUNC(__fvs_pow_vex_256_mask)
        ELF_SIZE(__fvs_pow_vex_256_mask)
#endif

/*
 *   __fvs_pow_vex_mask(argument1, argument2, mask)
 *   __fvs_pow_fma4_mask(argument1, argument2, mask)
 *
 *   argument:   xmm0, xmm1
 *   mask:       xmm2
 *
 *   Compute the power of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_pow_fma4_mask)
ENT(__fvs_pow_fma4_mask):
#else
        .globl ENT(__fvs_pow_vex_mask)
ENT(__fvs_pow_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_s_zeromask(%rip), %xmm2
        je      LBL(.L_fvs_pow_done)

        vmovups .L_spow_mask_two(%rip),%xmm3
        vblendvps %xmm2,%xmm0,%xmm3,%xmm0
        vblendvps %xmm2,%xmm1,%xmm3,%xmm1


#ifdef FMA4_TARGET
        CALL(ENT(__fvs_pow_fma4))
#else
        CALL(ENT(__fvs_pow_vex))
#endif

LBL(.L_fvs_pow_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_pow_fma4_mask)
        ELF_SIZE(__fvs_pow_fma4_mask)
#else
        ELF_FUNC(__fvs_pow_vex_mask)
        ELF_SIZE(__fvs_pow_vex_mask)
#endif

/*
 *   __fvs_sqrt_fma4_256_mask(argument, mask)
 *   __fvs_sqrt_vex_256_mask(argument, mask)
 *
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the square root of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_sqrt_fma4_256_mask)
ENT(__fvs_sqrt_fma4_256_mask):
#else
        .globl ENT(__fvs_sqrt_vex_256_mask)
ENT(__fvs_sqrt_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %ymm1
        je LBL(.L_done_fvs_sqrt_256)

        vandps %ymm0,%ymm1,%ymm0
        vsqrtps %ymm0,%ymm0

LBL(.L_done_fvs_sqrt_256):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_sqrt_fma4_256_mask)
        ELF_SIZE(__fvs_sqrt_fma4_256_mask)
#else
        ELF_FUNC(__fvs_sqrt_vex_256_mask)
        ELF_SIZE(__fvs_sqrt_vex_256_mask)
#endif

/*
 *   __fvs_sqrt_fma4_mask(argument, mask)
 *   __fvs_sqrt_vex_mask(argument, mask)
 *
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the square root of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_sqrt_fma4_mask)
ENT(__fvs_sqrt_fma4_mask):
#else
        .globl ENT(__fvs_sqrt_vex_mask)
ENT(__fvs_sqrt_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %xmm1
        je LBL(.L_done_fvs_sqrt)

        vandps %xmm0,%xmm1,%xmm0
        vsqrtps %xmm0,%xmm0

LBL(.L_done_fvs_sqrt):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_sqrt_fma4_mask)
        ELF_SIZE(__fvs_sqrt_fma4_mask)
#else
        ELF_FUNC(__fvs_sqrt_vex_mask)
        ELF_SIZE(__fvs_sqrt_vex_mask)
#endif

/*
 *   __fvd_sqrt_fma4_256_mask(argument, mask)
 *   __fvd_sqrt_vex_256_mask(argument, mask)
 *
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the square root of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_sqrt_fma4_256_mask)
ENT(__fvd_sqrt_fma4_256_mask):
#else
        .globl ENT(__fvd_sqrt_vex_256_mask)
ENT(__fvd_sqrt_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %ymm1
        je LBL(.L_done_fvd_sqrt_256)

        vandpd %ymm0,%ymm1,%ymm0
        vsqrtpd %ymm0,%ymm0

LBL(.L_done_fvd_sqrt_256):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_sqrt_fma4_256_mask)
        ELF_SIZE(__fvd_sqrt_fma4_256_mask)
#else
        ELF_FUNC(__fvd_sqrt_vex_256_mask)
        ELF_SIZE(__fvd_sqrt_vex_256_mask)
#endif

/*
 *   __fvd_sqrt_fma4_mask(argument, mask)
 *   __fvd_sqrt_vex_mask(argument, mask)
 *
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the square root of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_sqrt_fma4_mask)
ENT(__fvd_sqrt_fma4_mask):
#else
        .globl ENT(__fvd_sqrt_vex_mask)
ENT(__fvd_sqrt_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %xmm1
        je LBL(.L_done_fvd_sqrt)

        vandpd %xmm0,%xmm1,%xmm0
        vsqrtpd %xmm0,%xmm0

LBL(.L_done_fvd_sqrt):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_sqrt_fma4_mask)
        ELF_SIZE(__fvd_sqrt_fma4_mask)
#else
        ELF_FUNC(__fvd_sqrt_vex_mask)
        ELF_SIZE(__fvd_sqrt_vex_mask)
#endif

/*
 *   __fvs_div_fma4_256_mask(argument1, argument2, mask)
 *   __fvs_div_vex_256_mask(argument1, argument2, mask)
 *
 *   argument1(dividend):   ymm0
 *   argument2(divisor):    ymm1
 *   mask:                  ymm2
 *
 *   Compute argument1 / argument2 whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_div_fma4_256_mask)
ENT(__fvs_div_fma4_256_mask):
#else
        .globl ENT(__fvs_div_vex_256_mask)
ENT(__fvs_div_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %ymm2
        je LBL(.L_done_fvs_div_256)

        vmovups .L_one_for_mask_fvs(%rip), %ymm3

        vblendvps %ymm2,%ymm0,%ymm3,%ymm0
        vblendvps %ymm2,%ymm1,%ymm3,%ymm1

        vdivps %ymm1,%ymm0,%ymm0

LBL(.L_done_fvs_div_256):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_div_fma4_256_mask)
        ELF_SIZE(__fvs_div_fma4_256_mask)
#else
        ELF_FUNC(__fvs_div_vex_256_mask)
        ELF_SIZE(__fvs_div_vex_256_mask)
#endif

/*
 *   __fvs_div_fma4_mask(argument1, argument2, mask)
 *   __fvs_div_vex_mask(argument1, argument2, mask)
 *
 *   argument1(dividend):   xmm0
 *   argument2(divisor):    xmm1
 *   mask:                  xmm2
 *
 *   Compute argument1 / argument2 whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_div_fma4_mask)
ENT(__fvs_div_fma4_mask):
#else
        .globl ENT(__fvs_div_vex_mask)
ENT(__fvs_div_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %xmm2
        je LBL(.L_done_fvs_div)

        vmovups .L_one_for_mask_fvs(%rip), %xmm3

        vblendvps %xmm2,%xmm0,%xmm3,%xmm0
        vblendvps %xmm2,%xmm1,%xmm3,%xmm1

        vdivps %xmm1,%xmm0,%xmm0

LBL(.L_done_fvs_div):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_div_fma4_mask)
        ELF_SIZE(__fvs_div_fma4_mask)
#else
        ELF_FUNC(__fvs_div_vex_mask)
        ELF_SIZE(__fvs_div_vex_mask)
#endif

/*
 *   __fvd_div_fma4_256_mask(argument1, argument2, mask)
 *   __fvd_div_vex_256_mask(argument1, argument2, mask)
 *
 *   argument1(dividend):   ymm0
 *   argument2(divisor):    ymm1
 *   mask:                  ymm2
 *
 *   Compute argument1 / argument2 whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_div_fma4_256_mask)
ENT(__fvd_div_fma4_256_mask):
#else
        .globl ENT(__fvd_div_vex_256_mask)
ENT(__fvd_div_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %ymm2
        je LBL(.L_done_fvd_div_256)

        vmovupd .L_one_for_mask_fvd(%rip), %ymm3

        vblendvpd %ymm2,%ymm0,%ymm3,%ymm0
        vblendvpd %ymm2,%ymm1,%ymm3,%ymm1

        vdivpd %ymm1,%ymm0,%ymm0

LBL(.L_done_fvd_div_256):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_div_fma4_256_mask)
        ELF_SIZE(__fvd_div_fma4_256_mask)
#else
        ELF_FUNC(__fvd_div_vex_256_mask)
        ELF_SIZE(__fvd_div_vex_256_mask)
#endif

/*
 *   __fvd_div_fma4_mask(argument1, argument2, mask)
 *   __fvd_div_vex_mask(argument1, argument2, mask)
 *
 *   argument1(dividend):   xmm0
 *   argument2(divisor):    xmm1
 *   mask:                  xmm2
 *
 *   Compute argument1 / argument2 whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_div_fma4_mask)
ENT(__fvd_div_fma4_mask):
#else
        .globl ENT(__fvd_div_vex_mask)
ENT(__fvd_div_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %xmm2
        je LBL(.L_done_fvd_div)

        vmovupd .L_one_for_mask_fvd(%rip), %xmm3

        vblendvpd %xmm2,%xmm0,%xmm3,%xmm0
        vblendvpd %xmm2,%xmm1,%xmm3,%xmm1

        vdivpd %xmm1,%xmm0,%xmm0

LBL(.L_done_fvd_div):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_div_fma4_mask)
        ELF_SIZE(__fvd_div_fma4_mask)
#else
        ELF_FUNC(__fvd_div_vex_mask)
        ELF_SIZE(__fvd_div_vex_mask)
#endif

/*
 *   __fvs_log_fma4_256_mask(argument, mask)
 *   __fvs_log_vex_256_mask(argument, mask)
 *
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the logarithm of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_log_fma4_256_mask)
ENT(__fvs_log_fma4_256_mask):
#else
        .globl ENT(__fvs_log_vex_256_mask)
ENT(__fvs_log_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %ymm1
        je LBL(.L_done_fvs_log_256)

        vmovups .L_one_for_mask_fvs(%rip), %ymm2

        vblendvps %ymm1,%ymm0,%ymm2,%ymm0

#ifdef FMA4_TARGET
        CALL(ENT(__fvs_log_fma4_256))
#else
        CALL(ENT(__fvs_log_vex_256))
#endif

LBL(.L_done_fvs_log_256):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_log_fma4_256_mask)
        ELF_SIZE(__fvs_log_fma4_256_mask)
#else
        ELF_FUNC(__fvs_log_vex_256_mask)
        ELF_SIZE(__fvs_log_vex_256_mask)
#endif

/*
 *   __fvs_log_fma4_mask(argument, mask)
 *   __fvs_log_vex_mask(argument, mask)
 *
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the logarithm of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_log_fma4_mask)
ENT(__fvs_log_fma4_mask):
#else
        .globl ENT(__fvs_log_vex_mask)
ENT(__fvs_log_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %xmm1
        je LBL(.L_done_fvs_log)

        vmovups .L_one_for_mask_fvs(%rip), %xmm2

        vblendvps %xmm1,%xmm0,%xmm2,%xmm0

#ifdef FMA4_TARGET
        CALL(ENT(__fvs_log_fma4))
#else
        CALL(ENT(__fvs_log_vex))
#endif

LBL(.L_done_fvs_log):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_log_fma4_mask)
        ELF_SIZE(__fvs_log_fma4_mask)
#else
        ELF_FUNC(__fvs_log_vex_mask)
        ELF_SIZE(__fvs_log_vex_mask)
#endif

/*
 *   __fvd_log_fma4_256_mask(argument, mask)
 *   __fvd_log_vex_256_mask(argument, mask)
 *
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the logarithm of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_log_fma4_256_mask)
ENT(__fvd_log_fma4_256_mask):
#else
        .globl ENT(__fvd_log_vex_256_mask)
ENT(__fvd_log_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %ymm1
        je LBL(.L_done_fvd_log_256)

        vmovupd .L_one_for_mask_fvd(%rip), %ymm2

        vblendvpd %ymm1,%ymm0,%ymm2,%ymm0

#ifdef FMA4_TARGET
        CALL(ENT(__fvd_log_fma4_256))
#else
        CALL(ENT(__fvd_log_vex_256))
#endif

LBL(.L_done_fvd_log_256):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_log_fma4_256_mask)
        ELF_SIZE(__fvd_log_fma4_256_mask)
#else
        ELF_FUNC(__fvd_log_vex_256_mask)
        ELF_SIZE(__fvd_log_vex_256_mask)
#endif

/*
 *   __fvd_log_fma4_mask(argument, mask)
 *   __fvd_log_vex_mask(argument, mask)
 *
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the logarithm of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_log_fma4_mask)
ENT(__fvd_log_fma4_mask):
#else
        .globl ENT(__fvd_log_vex_mask)
ENT(__fvd_log_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %xmm1
        je LBL(.L_done_fvd_log)

        vmovupd .L_one_for_mask_fvd(%rip), %xmm2

        vblendvpd %xmm1,%xmm0,%xmm2,%xmm0

#ifdef FMA4_TARGET
        CALL(ENT(__fvd_log_fma4))
#else
        CALL(ENT(__fvd_log_vex))
#endif

LBL(.L_done_fvd_log):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_log_fma4_mask)
        ELF_SIZE(__fvd_log_fma4_mask)
#else
        ELF_FUNC(__fvd_log_vex_mask)
        ELF_SIZE(__fvd_log_vex_mask)
#endif

/*
 *   __fvs_log10_fma4_256_mask(argument, mask)
 *   __fvs_log10_vex_256_mask(argument, mask)
 *
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the logarithm(base 10) of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_log10_fma4_256_mask)
ENT(__fvs_log10_fma4_256_mask):
#else
        .globl ENT(__fvs_log10_vex_256_mask)
ENT(__fvs_log10_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %ymm1
        je LBL(.L_done_fvs_log10_256)

        vmovups .L_one_for_mask_fvs(%rip), %ymm2

        vblendvps %ymm1,%ymm0,%ymm2,%ymm0

#ifdef FMA4_TARGET
        CALL(ENT(__fvs_log10_fma4_256))
#else
        CALL(ENT(__fvs_log10_vex_256))
#endif

LBL(.L_done_fvs_log10_256):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_log10_fma4_256_mask)
        ELF_SIZE(__fvs_log10_fma4_256_mask)
#else
        ELF_FUNC(__fvs_log10_vex_256_mask)
        ELF_SIZE(__fvs_log10_vex_256_mask)
#endif

/*
 *   __fvs_log10_fma4_mask(argument, mask)
 *   __fvs_log10_vex_mask(argument, mask)
 *
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the logarithm(base 10) of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_log10_fma4_mask)
ENT(__fvs_log10_fma4_mask):
#else
        .globl ENT(__fvs_log10_vex_mask)
ENT(__fvs_log10_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %xmm1
        je LBL(.L_done_fvs_log10)

        vmovups .L_one_for_mask_fvs(%rip), %xmm2

        vblendvps %xmm1,%xmm0,%xmm2,%xmm0

#ifdef FMA4_TARGET
        CALL(ENT(__fvs_log10_fma4))
#else
        CALL(ENT(__fvs_log10_vex))
#endif

LBL(.L_done_fvs_log10):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_log10_fma4_mask)
        ELF_SIZE(__fvs_log10_fma4_mask)
#else
        ELF_FUNC(__fvs_log10_vex_mask)
        ELF_SIZE(__fvs_log10_vex_mask)
#endif

/*
 *   __fvd_log10_fma4_256_mask(argument, mask)
 *   __fvd_log10_vex_256_mask(argument, mask)
 *
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the logarithm(base 10) of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_log10_fma4_256_mask)
ENT(__fvd_log10_fma4_256_mask):
#else
        .globl ENT(__fvd_log10_vex_256_mask)
ENT(__fvd_log10_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %ymm1
        je LBL(.L_done_fvd_log10_256)

        vmovupd .L_one_for_mask_fvd(%rip), %ymm2

        vblendvpd %ymm1,%ymm0,%ymm2,%ymm0

#ifdef FMA4_TARGET
        CALL(ENT(__fvd_log10_fma4_256))
#else
        CALL(ENT(__fvd_log10_vex_256))
#endif

LBL(.L_done_fvd_log10_256):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_log10_fma4_256_mask)
        ELF_SIZE(__fvd_log10_fma4_256_mask)
#else
        ELF_FUNC(__fvd_log10_vex_256_mask)
        ELF_SIZE(__fvd_log10_vex_256_mask)
#endif


/*
 *   __fvd_log10_fma4_mask(argument, mask)
 *   __fvd_log10_vex_mask(argument, mask)
 *
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the logarithm(base 10) of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_log10_fma4_mask)
ENT(__fvd_log10_fma4_mask):
#else
        .globl ENT(__fvd_log10_vex_mask)
ENT(__fvd_log10_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %xmm1
        je LBL(.L_done_fvd_log10)

        vmovupd .L_one_for_mask_fvd(%rip), %xmm2

        vblendvpd %xmm1,%xmm0,%xmm2,%xmm0

#ifdef FMA4_TARGET
        CALL(ENT(__fvd_log10_fma4))
#else
        CALL(ENT(__fvd_log10_vex))
#endif

LBL(.L_done_fvd_log10):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_log10_fma4_mask)
        ELF_SIZE(__fvd_log10_fma4_mask)
#else
        ELF_FUNC(__fvd_log10_vex_mask)
        ELF_SIZE(__fvd_log10_vex_mask)
#endif


/*
 *   __fvd_tan_vex_256_mask(argument, mask)
 *   __fvd_tan_fma4_256_mask(argument, mask)
 * 
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the tangent of the arguments whose mask is non-zero
 *
 */
        .text
	ALN_FUNC
#ifdef FMA4_TARGET
	.globl ENT(__fvd_tan_fma4_256_mask)
ENT(__fvd_tan_fma4_256_mask):
#else
	.globl ENT(__fvd_tan_vex_256_mask)
ENT(__fvd_tan_vex_256_mask):
#endif

	subq $8, %rsp

        vptest	.L_zeromask(%rip), %ymm1
	je	LBL(.L_fvd_tan_256_done)

	vandpd	%ymm0,%ymm1,%ymm0
#ifdef FMA4_TARGET
	CALL(ENT(__fvd_tan_fma4_256))
#else
	CALL(ENT(__fvd_tan_vex_256))
#endif

LBL(.L_fvd_tan_256_done):

	addq $8, %rsp
	ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_tan_fma4_256_mask)
        ELF_SIZE(__fvd_tan_fma4_256_mask)
#else
        ELF_FUNC(__fvd_tan_vex_256_mask)
        ELF_SIZE(__fvd_tan_vex_256_mask)
#endif

/*
 *   __fvd_tan_vex_mask(argument, mask)
 *   __fvd_tan_fma4_mask(argument, mask)
 * 
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the tangent of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvd_tan_fma4_mask)
ENT(__fvd_tan_fma4_mask):
#else
        .globl ENT(__fvd_tan_vex_mask)
ENT(__fvd_tan_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_zeromask(%rip), %xmm1
        je      LBL(.L_fvd_tan_done)

        vandpd  %xmm0,%xmm1,%xmm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvd_tan_fma4))
#else
        CALL(ENT(__fvd_tan_vex))
#endif

LBL(.L_fvd_tan_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvd_tan_fma4_mask)
        ELF_SIZE(__fvd_tan_fma4_mask)
#else
        ELF_FUNC(__fvd_tan_vex_mask)
        ELF_SIZE(__fvd_tan_vex_mask)
#endif


/*
 *   __fvs_tan_vex_256_mask(argument, mask)
 *
 *   argument:   ymm0
 *   mask:       ymm1
 *
 *   Compute the tangent of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_tan_fma4_256_mask)
ENT(__fvs_tan_fma4_256_mask):
#else
        .globl ENT(__fvs_tan_vex_256_mask)
ENT(__fvs_tan_vex_256_mask):
#endif
        subq $8, %rsp

        vptest  .L_s_zeromask(%rip), %ymm1
        je      LBL(.L_fvs_tan_256_done)

        vandps %ymm0,%ymm1,%ymm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvs_tan_fma4_256))
#else
        CALL(ENT(__fvs_tan_vex_256))
#endif

LBL(.L_fvs_tan_256_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_tan_fma4_256_mask)
        ELF_SIZE(__fvs_tan_fma4_256_mask)
#else
        ELF_FUNC(__fvs_tan_vex_256_mask)
        ELF_SIZE(__fvs_tan_vex_256_mask)
#endif

/*
 *   __fvs_tan_vex_mask(argument, mask)
 *   __fvs_tan_fma4_mask(argument, mask)
 *
 *   argument:   xmm0
 *   mask:       xmm1
 *
 *   Compute the tangent of the arguments whose mask is non-zero
 *
 */
        .text
        ALN_FUNC
#ifdef FMA4_TARGET
        .globl ENT(__fvs_tan_fma4_mask)
ENT(__fvs_tan_fma4_mask):
#else
        .globl ENT(__fvs_tan_vex_mask)
ENT(__fvs_tan_vex_mask):
#endif
        subq $8, %rsp

        vptest  .L_s_zeromask(%rip), %xmm1
        je      LBL(.L_fvs_tan_done)

        vandps %xmm0,%xmm1,%xmm0
#ifdef FMA4_TARGET
        CALL(ENT(__fvs_tan_fma4))
#else
        CALL(ENT(__fvs_tan_vex))
#endif

LBL(.L_fvs_tan_done):
        addq $8, %rsp
        ret

#ifdef FMA4_TARGET
        ELF_FUNC(__fvs_tan_fma4_mask)
        ELF_SIZE(__fvs_tan_fma4_mask)
#else
        ELF_FUNC(__fvs_tan_vex_mask)
        ELF_SIZE(__fvs_tan_vex_mask)
#endif

