
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


/*
 * Real.
 */

typedef double  vrd1_t;
typedef double  vrd2_t  __attribute__((vector_size(2*sizeof(double))));
typedef double  vrd4_t  __attribute__((vector_size(4*sizeof(double))));
typedef double  vrd8_t  __attribute__((vector_size(8*sizeof(double))));
typedef	float	vrs1_t;
typedef	float	vrs4_t	__attribute__((vector_size(4*sizeof(float))));
typedef	float	vrs8_t	__attribute__((vector_size(8*sizeof(float))));
typedef	float	vrs16_t	__attribute__((vector_size(16*sizeof(float))));


/*
 * Complex.
 *
 * Note:
 * Vector structures cannot be made up of structures contaning real and
 * imaginary components.
 * As such, complex vector structures are in name only and simply
 * overloaded to the REALs.  To extract the R and i's, other macros or
 * C constructs must be used.
 */

typedef double  vcd1_t  __attribute__((vector_size(2*sizeof(double))));
typedef double  vcd2_t  __attribute__((vector_size(4*sizeof(double))));
typedef double  vcd4_t  __attribute__((vector_size(8*sizeof(double))));
typedef float   vcs1_t  __attribute__((vector_size(2*sizeof(float))));
typedef float   vcs2_t  __attribute__((vector_size(4*sizeof(float))));
typedef float   vcs4_t  __attribute__((vector_size(8*sizeof(float))));
typedef float   vcs8_t  __attribute__((vector_size(16*sizeof(float))));


/*
 * Integer.
 */

typedef	int32_t	vis1_t;
typedef	int32_t	vis2_t	__attribute__((vector_size(2*sizeof(int32_t))));
typedef	int32_t	vis4_t	__attribute__((vector_size(4*sizeof(int32_t))));
typedef	int32_t	vis8_t	__attribute__((vector_size(8*sizeof(int32_t))));
typedef	int32_t	vis16_t	__attribute__((vector_size(16*sizeof(int32_t))));
typedef	int64_t	vid1_t;
typedef	int64_t	vid2_t	__attribute__((vector_size(2*sizeof(int64_t))));
typedef	int64_t	vid4_t	__attribute__((vector_size(4*sizeof(int64_t))));
typedef	int64_t	vid8_t	__attribute__((vector_size(8*sizeof(int64_t))));

#define _CONCAT2(a,b)    a##b
#define CONCAT2(a,b) _CONCAT(a,b)
#define _CONCAT3(a,b,c)    a##b##c
#define CONCAT3(a,b,c) _CONCAT3(a,b,c)
#define _CONCAT4(a,b,c,d)    a##b##c##d
#define CONCAT4(a,b,c,d) _CONCAT4(a,b,c,d)
#define _CONCAT5(a,b,c,d,e)    a##b##c##d##e
#define CONCAT5(a,b,c,d,e) _CONCAT5(a,b,c,d,e)
#define _CONCAT6(a,b,c,d,e,f)    a##b##c##d##e##f
#define CONCAT6(a,b,c,d,e,f) _CONCAT6(a,b,c,d,e,f)
#define _CONCAT7(a,b,c,d,e,f,g)    a##b##c##d##e##f##g
#define CONCAT7(a,b,c,d,e,f,g) _CONCAT7(a,b,c,d,e,f,g)
#define _STRINGIFY(_n) #_n
#define STRINGIFY(_n) _STRINGIFY(_n)


#if ! defined(MAX_VREG_SIZE)
#error  MAX_VREG_SIZE must be defined.
#endif


#if MAX_VREG_SIZE == 64
#define VLS     1
#define VLD     1
#define VIS_T   vis1_t
#define VID_T   vid1_t
#define VRS_T   vrs1_t
#define VRD_T   vrd1_t
#define FMIN	1.0f
#define DMIN	1.0d
#define VRET(subscript) vret
#define ROUT(subscript) rout
#define RES(subscript) res
#define EXP(subscript) exp
#elif MAX_VREG_SIZE == 128
#define VLS     4
#define VLD     2
#define VIS_T   vis4_t
#define VID_T   vid2_t
#define VRS_T   vrs4_t
#define VRD_T   vrd2_t
#define FMIN	2.0f
#define DMIN	2.0d
#define VRET(subscript) vret[subscript]
#define ROUT(subscript) rout[subscript]
#define RES(subscript) res[subscript]
#define EXP(subscript) exp[subscript]
#elif   MAX_VREG_SIZE == 256
#define VLS     8
#define VLD     4
#define VIS_T   vis8_t
#define VID_T   vid4_t
#define VRS_T   vrs8_t
#define VRD_T   vrd4_t
#define FMIN	6.0f
#define DMIN	6.0d
#define VRET(subscript) vret[subscript]
#define ROUT(subscript) rout[subscript]
#define RES(subscript) res[subscript]
#define EXP(subscript) exp[subscript]
#elif   MAX_VREG_SIZE == 512
#define VLS     16
#define VLD     8
#define VIS_T   vis16_t
#define VID_T   vid8_t
#define VRS_T   vrs16_t
#define VRD_T   vrd8_t
#define FMIN	14.0f
#define DMIN	14.0d
#define VRET(subscript) vret[subscript]
#define ROUT(subscript) rout[subscript]
#define RES(subscript) res[subscript]
#define EXP(subscript) exp[subscript]
#else
#error  MAX_VREG_SIZE must be one of 64, 128, 256, or 512
#endif

#define FCONST1 0.0f
#define FCONST2 31.0f
#define DCONST1 0.0d
#define DCONST2 31.0d

#define EXTERN_EFUNC(name) \
    extern VRS_T \
    CONCAT5(__fs_,name,_,VLS,)(VRS_T), CONCAT5(__fs_,name,_,VLS,m)(VRS_T,VIS_T), \
    CONCAT5(__rs_,name,_,VLS,)(VRS_T), CONCAT5(__rs_,name,_,VLS,m)(VRS_T,VIS_T), \
    CONCAT5(__ps_,name,_,VLS,)(VRS_T), CONCAT5(__ps_,name,_,VLS,m)(VRS_T,VIS_T); \
    extern VRD_T \
    CONCAT5(__fd_,name,_,VLD,)(VRD_T), CONCAT5(__fd_,name,_,VLD,m)(VRD_T,VID_T), \
    CONCAT5(__rd_,name,_,VLD,)(VRD_T), CONCAT5(__rd_,name,_,VLD,m)(VRD_T,VID_T), \
    CONCAT5(__pd_,name,_,VLD,)(VRD_T), CONCAT5(__pd_,name,_,VLD,m)(VRD_T,VID_T)

#define EXTERN_EFUNC2(name) \
    extern VRS_T \
    CONCAT5(__fs_,name,_,VLS,)(VRS_T,VRS_T), CONCAT5(__fs_,name,_,VLS,m)(VRS_T,VRS_T,VIS_T), \
    CONCAT5(__rs_,name,_,VLS,)(VRS_T,VRS_T), CONCAT5(__rs_,name,_,VLS,m)(VRS_T,VRS_T,VIS_T), \
    CONCAT5(__ps_,name,_,VLS,)(VRS_T,VRS_T), CONCAT5(__ps_,name,_,VLS,m)(VRS_T,VRS_T,VIS_T); \
    extern VRD_T \
    CONCAT5(__fd_,name,_,VLD,)(VRD_T,VRD_T), CONCAT5(__fd_,name,_,VLD,m)(VRD_T,VRD_T,VID_T), \
    CONCAT5(__rd_,name,_,VLD,)(VRD_T,VRD_T), CONCAT5(__rd_,name,_,VLD,m)(VRD_T,VRD_T,VID_T), \
    CONCAT5(__pd_,name,_,VLD,)(VRD_T,VRD_T), CONCAT5(__pd_,name,_,VLD,m)(VRD_T,VRD_T,VID_T)


EXTERN_EFUNC(sin);
EXTERN_EFUNC(cos);
EXTERN_EFUNC(tan);
EXTERN_EFUNC(exp);
EXTERN_EFUNC(log);
EXTERN_EFUNC(asin);
EXTERN_EFUNC(acos);
EXTERN_EFUNC(atan);
EXTERN_EFUNC(sinh);
EXTERN_EFUNC(cosh);
EXTERN_EFUNC(tanh);
EXTERN_EFUNC(log10);

EXTERN_EFUNC2(atan2);


VRS_T
vrs_set_arg(float fmin, float fconst )
{
    VRS_T   vret __attribute__((aligned(64)));
    float   fdelta;
    int     i;

    fdelta = fconst + fmin;
    for (i = 0; i < VLS; i++) {
       VRET(i) = (1.0f / (fdelta + (float) i));
    }

    return vret;
}

VRD_T
vrd_set_arg(double dmin, double dconst )
{
    VRD_T   vret __attribute__((aligned(64)));
    double  ddelta;
    int     i;

    ddelta = dconst + dmin;
    for (i = 0; i < VLS; i++) {
       VRET(i) = (1.0d / (ddelta + (double) i));
    }

    return vret;
}

int
checkfltol(VRS_T res, VRS_T exp, int n, float ltol)
{
    int i;
    int tests_passed = 0;
    int tests_failed = 0;

    for (i = 0; i < n; i++) {
        if (EXP(i) == RES(i)) {
            tests_passed ++;
        }else if( EXP(i) != 0.0 && (fabsf((EXP(i)-RES(i))/EXP(i))) <= ltol ){
            tests_passed ++;
        }else if( EXP(i) == 0.0 && EXP(i) <= ltol ){
            tests_passed ++;
        } else {
            tests_failed ++;
            if( tests_failed < 50 )
            printf(
            "test number %d FAILED. res %f  exp %f\n",
             i+1,RES(i), EXP(i));
        }
    }
    if (tests_failed == 0) {
        printf("%3d tests completed. %d tests PASSED. %d tests failed.\n",
                      n, tests_passed, tests_failed);
    } else {
        printf("%3d tests completed. %d tests passed. %d tests FAILED.\n",
                      n, tests_passed, tests_failed);
    }
    exit(tests_failed);
}
