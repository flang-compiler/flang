/*
 * Copyright (c) 2006-2018, NVIDIA CORPORATION.  All rights reserved.
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
   \brief Structures to describe the x86 CPU type and CPU features
 */

#ifndef __X86_H
#define __X86_H

#define MACH_GENERIC 1
#define MACH_INTEL 2
#define MACH_INTEL_PENTIUM4 3
#define MACH_INTEL_CORE2 4
#define MACH_INTEL_PENRYN 5
#define MACH_INTEL_NEHALEM 6
#define MACH_INTEL_SANDYBRIDGE 7
#define MACH_INTEL_HASWELL 8
#define MACH_INTEL_KNIGHTS_LANDING 9
#define MACH_INTEL_SKYLAKE 10
#define MACH_INTEL_LARRABEE 11 /* delete this when possible! */
#define MACH_AMD 12
#define MACH_AMD_ATHLON 13
#define MACH_AMD_ATHLON_XP 14
#define MACH_AMD_HAMMER 15
#define MACH_AMD_GH 16
#define MACH_AMD_SHANGHAI 17
#define MACH_AMD_ISTANBUL 18
#define MACH_AMD_BULLDOZER 19
#define MACH_AMD_PILEDRIVER 20
#define MACH_AMD_ZEN 21

#define MACH_NUMBER 22

#define FEATURE_SCALAR_SSE 0 /* -Mscalarsse flag */
#define FEATURE_SSE 1        /* supports SSE */
#define FEATURE_SSE2 2       /* supports SSE2 */
#define FEATURE_SSE3 3       /* supports SSE3 */
#define FEATURE_SSE41 4      /*   "   SSE4.1 (>= Intel penryn, AMD bulldozer) */
#define FEATURE_SSE42 5      /*   "   SSE4.2 (>= Intel nehalem, AMD bulldozer)*/
#define FEATURE_SSE4A 6      /*   "   SSE4a  (>= AMD barcelona) */
#define FEATURE_SSE5 7       /*   "   SSE5 (AMD) */
#define FEATURE_MNI 8        /*   "   Meron New Instructions, SSSE3 Intel */
#define FEATURE_DAZ 9              /* -Mdaz flag, denorm as zero */
#define FEATURE_PREFER_MOVLPD 10   /* prefer movlpd over movsd, used in CG */
#define FEATURE_USE_INC 11         /* prefer incl over addl $1 */
#define FEATURE_USE_MOVAPD 12      /* use movapd instead of movsd */
#define FEATURE_MERGE_DEPENDENT 13 /* different CG decisions */
#define FEATURE_SCALAR_NONTEMP 14  /* in llvect */
#define FEATURE_SSEIMAX 15         /* use SSE code sequence for IMAX/IMIN */
#define FEATURE_MISALIGNEDSSE 16   /* allow misaligned SSE ops from memory */
#define FEATURE_LD_MOVUPD 17     /* use movupd for unaligned packed loads */
#define FEATURE_ST_MOVUPD 18     /* use movupd for unaligned packed stores */
#define FEATURE_UNROLL_16 19     /* extra unrolling, unroll factor is 16 \
                                  *   (initially for GH) */
#define FEATURE_DOUBLE_UNROLL 20 /* double unroll factor (initially for GH) */
#define FEATURE_PEEL_SHUFFLE 21  /* allow peel-shuffle */
#define FEATURE_PREFETCHNTA 22   /* allow prefetchnta */
#define FEATURE_PDSHUF 23        /* prefer PDSHUF over UNPCK[LH]PD etc. */
#define FEATURE_SSEPMAX 24     /* use PMAX/PMIN for IMAX/IMIN in SSE (SSE4.1) */
#define FEATURE_GHLIBS 25      /* use _gh library routines */
#define FEATURE_SSEMISALN 26   /* allow misaligned SSE memory operands */
#define FEATURE_ABM 27         /* allow advanced bit manipulation */
#define FEATURE_AVX 28         /* supports AVX - Advanced Vector Extensions */
#define FEATURE_LRBNI 29       /* supports LRBni - Larrabee new instructions */
#define FEATURE_FMA4 30        /* supports 4-operand FMA */
#define FEATURE_XOP 31         /* supports eXtended OPerations */
#define FEATURE_FMA3 32        /* supports 3-operand FMA */
#define FEATURE_MULTI_ACCUM 33 /* multiple accumulators for reductions */
#define FEATURE_SIMD128 34     /* Use SIMD:128, even with AVX */
#define FEATURE_NOPREFETCH 35  /* Disable prefetches */
#define FEATURE_ALIGNLOOP4 36  /* Align loops at 4 */
#define FEATURE_ALIGNLOOP8 37  /* Align loops at 8 */
#define FEATURE_ALIGNLOOP16 38 /* Align loops at 16 */
#define FEATURE_ALIGNLOOP32 39 /* Align loops at 32 */
#define FEATURE_ALIGNJMP8 40   /* Align after jump at 8 */
#define FEATURE_ALIGNJMP16 41  /* after after jump at 16 */
#define FEATURE_LD_VMOVUPD 42  /* use vmovupd for 32-byte unaligned loads */
#define FEATURE_ST_VMOVUPD 43  /* use vmovupd for 32-byte unaligned stores */
#define FEATURE_AVX2 44        /* supports AVX2 */
#define FEATURE_AVX512F 45     /* supports AVX-512F */
#define FEATURE_AVX512VL 46    /* supports AVX-512VL */

#define FEATURE_NUMBER 47

/*****  ARM -- recycle FEATURE_ x64/x86 manifests  *****/
#if defined(TARGET_LLVM_ARM)
#define FEATURE_SCALAR_NEON FEATURE_SCALAR_SSE
#define FEATURE_NEON FEATURE_SSE
#define FEATURE_FMA FEATURE_FMA3
#endif

/*****  POWER -- recycle FEATURE_ x64/x86 manifests  *****/
#if defined(TARGET_LLVM_POWER)
#define FEATURE_SCALAR_VSX FEATURE_SCALAR_SSE
#define FEATURE_VSX FEATURE_SSE
#define FEATURE_FMA FEATURE_FMA3
#endif

typedef struct {
  int tpval;
  int type[MACH_NUMBER];
  int feature[FEATURE_NUMBER];
  long cachesize;
} X86TYPE;

extern X86TYPE mach, mach_count;

/* These TP values should be sorted so the most powerful have the
 * largest values; these are used to sort the TP values, so we
 * generate the code for the most aggressive processors first.  The
 * lowest allowable value is 1.
 */
#define TP_PY 1
#define TP_PX 2
#define TP_P5 3
#define TP_ATHLON 4
#define TP_P6 5
#define TP_ATHLON_XP 6
#define TP_PIII 7
#define TP_K8 8
#define TP_P7 9
#define TP_K8E 10
#define TP_PIV 11
#define TP_GH 12
#define TP_CORE2 13
#define TP_PENRYN 14
#define TP_SHANGHAI 15
#define TP_ISTANBUL 16
#define TP_NEHALEM 17
#define TP_BULLDOZER 18
#define TP_SANDYBRIDGE 19
#define TP_IVYBRIDGE 20
#define TP_HASWELL 21
#define TP_LARRABEE 22 /* delete this when possible! */
#define TP_PILEDRIVER 23
#define TP_ZEN 24
#define TP_KNIGHTS_LANDING 25
#define TP_SKYLAKE 26

void init_mach_intersect(void);
void copy_mach_intersect(X86TYPE *mach);
void intersect_mach_intersect(X86TYPE *mach);
extern void set_mach(X86TYPE *, int); /* set mach based on TP_ value */
extern void set_mach_accel(X86TYPE *, int);
/* set mach.accel based on accelerator */
extern int machvalue(char *);        /* MACH_ value of a string */
extern void set_tp(char *);          /* add flg.tpvalue */
extern void check_tp(LOGICAL);
extern LOGICAL have_mach_accel(int); /* is this accel type set? */
extern LOGICAL any_gpu_device(); /* are there any GPU (cuda,opencl) devices? */

extern char *tpname[];
extern char *version_name[];

#define TEST_MACH(M) (++mach_count.type[M], mach.type[M])
#define TEST_MACH2(M1, M2) \
  (++mach_count.type[M1], ++mach_count.type[M2], mach.type[M1] || mach.type[M2])
#define TEST_MACHN(M, N) (mach_count.type[M] += N, mach.type[M])
#define TEST_FEATURE(M) (++mach_count.feature[M], mach.feature[M])
#define TEST_FEATURE2(M1, M2)                          \
  (++mach_count.feature[M1], ++mach_count.feature[M2], \
   mach.feature[M1] || mach.feature[M2])
#define TEST_FEATUREN(M, N) (mach_count.feature[M] += N, mach.feature[M])
#define TEST_CACHE (++mach_count.cachesize, mach.cachesize)
#define TEST_ACCEL mach.accel

#endif /* __X86_H */
