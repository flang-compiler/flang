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
#include <stdint.h>
#include <complex.h>

/*
 * Real.
 */

typedef	double	vrd1_t;
typedef	double	vrd2_t	__attribute__((vector_size(2*sizeof(double))));
typedef	double	vrd4_t	__attribute__((vector_size(4*sizeof(double))));
typedef	double	vrd8_t	__attribute__((vector_size(8*sizeof(double))));
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

typedef	double	vcd1_t	__attribute__((vector_size(2*sizeof(double))));
typedef	double	vcd2_t	__attribute__((vector_size(4*sizeof(double))));
typedef	double	vcd4_t	__attribute__((vector_size(8*sizeof(double))));
typedef	float	vcs1_t	__attribute__((vector_size(2*sizeof(float))));
typedef	float	vcs2_t	__attribute__((vector_size(4*sizeof(float))));
typedef	float	vcs4_t	__attribute__((vector_size(8*sizeof(float))));
typedef	float	vcs8_t	__attribute__((vector_size(16*sizeof(float))));

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

extern	vrs4_t	__ZGVxN4v__mth_i_vr4		(vrs4_t, float(*)(float));
extern	vrs4_t	__ZGVxM4v__mth_i_vr4		(vrs4_t, vis4_t, float(*)(float));
extern	vrs4_t	__ZGVxN4vv__mth_i_vr4vr4	(vrs4_t, vrs4_t, float(*)(float, float));
extern	vrs4_t	__ZGVxM4vv__mth_i_vr4vr4	(vrs4_t, vrs4_t, vis4_t, float(*)(float, float));
extern	vrd2_t	__ZGVxN2v__mth_i_vr8		(vrd2_t, double(*)(double));
extern	vrd2_t	__ZGVxM2v__mth_i_vr8		(vrd2_t, vid2_t, double(*)(double));
extern	vrd2_t	__ZGVxN2vv__mth_i_vr8vr8	(vrd2_t, vrd2_t, double(*)(double, double));
extern	vrd2_t	__ZGVxM2vv__mth_i_vr8vr8	(vrd2_t, vrd2_t, vid2_t, double(*)(double, double));
extern	vrs8_t	__ZGVyN8v__mth_i_vr4		(vrs8_t, float(*)(float));
extern	vrs8_t	__ZGVyM8v__mth_i_vr4		(vrs8_t, vis8_t, float(*)(float));
extern	vrs8_t	__ZGVyN8vv__mth_i_vr4vr4	(vrs8_t, vrs8_t, float(*)(float, float));
extern	vrs8_t	__ZGVyM8vv__mth_i_vr4vr4	(vrs8_t, vrs8_t, vis8_t, float(*)(float, float));
extern	vrd4_t	__ZGVyN4v__mth_i_vr8		(vrd4_t, double(*)(double));
extern	vrd4_t	__ZGVyM4v__mth_i_vr8		(vrd4_t, vid4_t, double(*)(double));
extern	vrd4_t	__ZGVyN4vv__mth_i_vr8vr8	(vrd4_t, vrd4_t, double(*)(double, double));
extern	vrd4_t	__ZGVyM4vv__mth_i_vr8vr8	(vrd4_t, vrd4_t, vid4_t, double(*)(double, double));
extern	vrs16_t	__ZGVzN16v__mth_i_vr4		(vrs16_t, float(*)(float));
extern	vrs16_t	__ZGVzM16v__mth_i_vr4		(vrs16_t, vis16_t, float(*)(float));
extern	vrs16_t	__ZGVzN16vv__mth_i_vr4vr4	(vrs16_t, vrs16_t, float(*)(float, float));
extern	vrs16_t	__ZGVzM16vv__mth_i_vr4vr4	(vrs16_t, vrs16_t, vis16_t, float(*)(float, float));
extern	vrd8_t	__ZGVzN8v__mth_i_vr8		(vrd8_t, double(*)(double));
extern	vrd8_t	__ZGVzM8v__mth_i_vr8		(vrd8_t, vid8_t, double(*)(double));
extern	vrd8_t	__ZGVzN8vv__mth_i_vr8vr8	(vrd8_t, vrd8_t, double(*)(double, double));
extern	vrd8_t	__ZGVzM8vv__mth_i_vr8vr8	(vrd8_t, vrd8_t, vid8_t, double(*)(double, double));

/* Complex */
extern	vcs1_t	__ZGVxN1v__mth_i_vc4		(vcs1_t, float complex func(float complex));
extern	vcs1_t	__ZGVxN1vv__mth_i_vc4vc4	(vcs1_t, vcs1_t, float complex func(float complex, float complex));
extern	vcs2_t	__ZGVxN2v__mth_i_vc4		(vcs2_t, float complex func(float complex));
extern	vcs2_t	__ZGVxN2vv__mth_i_vc4vc4	(vcs2_t, vcs2_t, float complex func(float complex, float complex));
extern	vcd1_t	__ZGVxN1v__mth_i_vc8		(vcd1_t, double complex func(double complex));
extern	vcd1_t	__ZGVxN1vv__mth_i_vc8vc8	(vcd1_t, vcd1_t, double complex func(double complex, double complex));

extern	vcs4_t	__ZGVyN4v__mth_i_vc4		(vcs4_t, float complex func(float complex));
extern	vcs4_t	__ZGVyN4vv__mth_i_vc4vc4	(vcs4_t, vcs4_t, float complex func(float complex, float complex));
extern	vcd2_t	__ZGVyN2v__mth_i_vc8		(vcd2_t, double complex func(double complex));
extern	vcd2_t	__ZGVyN2vv__mth_i_vc8vc8	(vcd2_t, vcd2_t, double complex func(double complex, double complex));

extern	vcs8_t	__ZGVzN8v__mth_i_vc4		(vcs8_t, float complex func(float complex));
extern	vcs8_t	__ZGVzN8vv__mth_i_vc4vc4	(vcs8_t, vcs8_t, float complex func(float complex, float complex));
extern	vcd4_t	__ZGVzN4v__mth_i_vc8		(vcd4_t, double complex func(double complex));
extern	vcd4_t	__ZGVzN4vv__mth_i_vc8vc8	(vcd4_t, vcd4_t, double complex func(double complex, double complex));


extern	vrs4_t  __ZGVxN4v__mth_i_vr4si4   (vrs4_t, int32_t, float (*)(float, int32_t));
extern	vrs4_t  __ZGVxM4v__mth_i_vr4si4   (vrs4_t, int32_t, vis4_t, float (*)(float, int32_t));
extern	vrs4_t  __ZGVxN4vv__mth_i_vr4vi4  (vrs4_t, vis4_t, float (*)(float, int32_t));
extern	vrs4_t  __ZGVxM4vv__mth_i_vr4vi4  (vrs4_t, vis4_t, vis4_t, float (*)(float, int32_t));
extern	vrs4_t  __ZGVxN4v__mth_i_vr4si8   (vrs4_t, long long, float (*)(float, long long));
extern	vrs4_t  __ZGVxM4v__mth_i_vr4si8   (vrs4_t, long long, vis4_t, float (*)(float, long long));
extern	vrs4_t  __ZGVxN4vv__mth_i_vr4vi8  (vrs4_t, vid2_t, vid2_t, float (*)(float, long long));
extern	vrs4_t  __ZGVxM4vv__mth_i_vr4vi8  (vrs4_t, vid2_t, vid2_t, vis4_t, float (*)(float, long long));
extern	vrd2_t  __ZGVxN2v__mth_i_vr8si4   (vrd2_t, int32_t, double (*)(double, int32_t));
extern	vrd2_t  __ZGVxM2v__mth_i_vr8si4   (vrd2_t, int32_t, vid2_t, double (*)(double, int32_t));
/*
 * POWER architecture needs the 32-bit integer vector array to be defined as a
 * full vector size - not required for X86-64 architectures.
 * Technically these worker functions should be defined as
 * extern	vrd2_t  __ZGVxN2vv__mth_i_vr8vi4  (vrd2_t, vis2_t, double (*)(double, int32_t));
 * extern	vrd2_t  __ZGVxM2vv__mth_i_vr8vi4  (vrd2_t, vis2_t, vid2_t, double (*)(double, int32_t));
 */
extern	vrd2_t  __ZGVxN2vv__mth_i_vr8vi4  (vrd2_t, vis4_t, double (*)(double, int32_t));
extern	vrd2_t  __ZGVxM2vv__mth_i_vr8vi4  (vrd2_t, vis4_t, vid2_t, double (*)(double, int32_t));
extern	vrd2_t  __ZGVxN2v__mth_i_vr8si8   (vrd2_t, long long, double (*)(double, long long));
extern	vrd2_t  __ZGVxM2v__mth_i_vr8si8   (vrd2_t, long long, vid2_t, double (*)(double, long long));
extern	vrd2_t  __ZGVxN2vv__mth_i_vr8vi8  (vrd2_t, vid2_t, double (*)(double, long long));
extern	vrd2_t  __ZGVxM2vv__mth_i_vr8vi8  (vrd2_t, vid2_t, vid2_t, double (*)(double, long long));
extern	vrs8_t  __ZGVyN8v__mth_i_vr4si4   (vrs8_t, int32_t, float (*)(float, int32_t));
extern	vrs8_t  __ZGVyM8v__mth_i_vr4si4   (vrs8_t, int32_t, vis8_t, float (*)(float, int32_t));
extern	vrs8_t  __ZGVyN8vv__mth_i_vr4vi4  (vrs8_t, vis8_t, float (*)(float, int32_t));
extern	vrs8_t  __ZGVyM8vv__mth_i_vr4vi4  (vrs8_t, vis8_t, vis8_t, float (*)(float, int32_t));
extern	vrs8_t  __ZGVyN8v__mth_i_vr4si8   (vrs8_t, long long, float (*)(float, long long));
extern	vrs8_t  __ZGVyM8v__mth_i_vr4si8   (vrs8_t, long long, vis8_t, float (*)(float, long long));
extern	vrs8_t  __ZGVyN8vv__mth_i_vr4vi8  (vrs8_t, vid4_t, vid4_t, float (*)(float, long long));
extern	vrs8_t  __ZGVyM8vv__mth_i_vr4vi8  (vrs8_t, vid4_t, vid4_t, vis8_t, float (*)(float, long long));
extern	vrd4_t  __ZGVyN4v__mth_i_vr8si4   (vrd4_t, int32_t, double (*)(double, int32_t));
extern	vrd4_t  __ZGVyM4v__mth_i_vr8si4   (vrd4_t, int32_t, vid4_t, double (*)(double, int32_t));
extern	vrd4_t  __ZGVyN4vv__mth_i_vr8vi4  (vrd4_t, vis4_t, double (*)(double, int32_t));
extern	vrd4_t  __ZGVyM4vv__mth_i_vr8vi4  (vrd4_t, vis4_t, vid4_t, double (*)(double, int32_t));
extern	vrd4_t  __ZGVyN4v__mth_i_vr8si8   (vrd4_t, long long, double (*)(double, long long));
extern	vrd4_t  __ZGVyM4v__mth_i_vr8si8   (vrd4_t, long long, vid4_t, double (*)(double, long long));
extern	vrd4_t  __ZGVyN4vv__mth_i_vr8vi8  (vrd4_t, vid4_t, double (*)(double, long long));
extern	vrd4_t  __ZGVyM4vv__mth_i_vr8vi8  (vrd4_t, vid4_t, vid4_t, double (*)(double, long long));
extern	vrs16_t __ZGVzN16v__mth_i_vr4si4  (vrs16_t, int32_t, float (*)(float, int32_t));
extern	vrs16_t __ZGVzM16v__mth_i_vr4si4  (vrs16_t, int32_t, vis16_t, float (*)(float, int32_t));
extern	vrs16_t __ZGVzN16vv__mth_i_vr4vi4 (vrs16_t, vis16_t, float (*)(float, int32_t));
extern	vrs16_t __ZGVzM16vv__mth_i_vr4vi4 (vrs16_t, vis16_t, vis16_t, float (*)(float, int32_t));
extern	vrs16_t __ZGVzN16v__mth_i_vr4si8  (vrs16_t, long long, float (*)(float, long long));
extern	vrs16_t __ZGVzM16v__mth_i_vr4si8  (vrs16_t, long long, vis16_t, float (*)(float, long long));
extern	vrs16_t __ZGVzN16vv__mth_i_vr4vi8 (vrs16_t, vid8_t, vid8_t, float (*)(float, long long));
extern	vrs16_t __ZGVzM16vv__mth_i_vr4vi8 (vrs16_t, vid8_t, vid8_t, vis16_t, float (*)(float, long long));
extern	vrd8_t  __ZGVzN8v__mth_i_vr8si4   (vrd8_t, int32_t, double (*)(double, int32_t));
extern	vrd8_t  __ZGVzM8v__mth_i_vr8si4   (vrd8_t, int32_t, vid8_t, double (*)(double, int32_t));
extern	vrd8_t  __ZGVzN8vv__mth_i_vr8vi4  (vrd8_t, vis8_t, double (*)(double, int32_t));
extern	vrd8_t  __ZGVzM8vv__mth_i_vr8vi4  (vrd8_t, vis8_t, vid8_t, double (*)(double, int32_t));
extern	vrd8_t  __ZGVzN8v__mth_i_vr8si8   (vrd8_t, long long, double (*)(double, long long));
extern	vrd8_t  __ZGVzM8v__mth_i_vr8si8   (vrd8_t, long long, vid8_t, double (*)(double, long long));
extern	vrd8_t  __ZGVzN8vv__mth_i_vr8vi8  (vrd8_t, vid8_t, double (*)(double, long long));
extern	vrd8_t  __ZGVzM8vv__mth_i_vr8vi8  (vrd8_t, vid8_t, vid8_t, double (*)(double, long long));
extern	vcs1_t	__ZGVxN1v__mth_i_vc4si4   (vcs1_t, int32_t, float complex func(float complex, int32_t));
extern	vcs1_t	__ZGVxN1v__mth_i_vc4si8   (vcs1_t, long long, float complex func(float complex, long long));
extern	vcd1_t	__ZGVxN1v__mth_i_vc8si4   (vcd1_t, int32_t, double complex func(double complex, int32_t));
extern	vcd1_t	__ZGVxN1v__mth_i_vc8si8   (vcd1_t, long long, double complex func(double complex, long long));



#if	defined(LINUX8664) || defined(TARGET_OSX_X8664)
#define	acosf	__mth_i_acos
#define	acos	__mth_i_dacos
#define	asinf	__mth_i_asin
#define	asin	__mth_i_dasin
#define	atan2f	__mth_i_atan2
#define	atan2	__mth_i_datan2
#define	atanf	__mth_i_atan
#define	atan	__mth_i_datan
#define	cosf	__mth_i_cos
#define	coshf	__mth_i_cosh
#define	cosh	__mth_i_dcosh
#define	cos	__mth_i_dcos
#define	expf	__mth_i_exp
#define	exp	__mth_i_dexp
#define	log10f	__mth_i_alog10
#define	log10	__mth_i_dlog10
#define	logf	__mth_i_alog
#define	log	__mth_i_dlog
#define	fmodf	__mth_i_amod
#define	fmod	__mth_i_dmod
#define	powf	__mth_i_rpowr
#define	pow	__mth_i_dpowd
#define	sinf	__mth_i_sin
#define	sinhf	__mth_i_sinh
#define	sinh	__mth_i_dsinh
#define	sin	__mth_i_dsin
#define	tanf	__mth_i_tan
#define	tanhf	__mth_i_tanh
#define	tanh	__mth_i_dtanh
#define	tan	__mth_i_dtan

/*
dpowd.c:__mth_i_dpowd(double x, double y)
dpowi.c:__mth_i_dpowi(double x, int i)
dpowk.c:__mth_i_dpowk(double x, long long i)
rpowi.c:__mth_i_rpowi(float x, int i)
rpowk.c:__mth_i_rpowk(float x, long long i)
rpowr.c:__mth_i_rpowr(float arg1, float arg2)
*/
#define	powi	__mth_i_dpowi
#define	powk	__mth_i_dpowk
#define	powif	__mth_i_rpowi
#define	powkf	__mth_i_rpowk

#endif		/* LINUX8664 || TARGET_OSX_X8664 */

#ifdef	TARGET_LINUX_POWER
// Removed these defines because we don't want the "precise" versions
// to be calling the xlmass routines.  Instead we choose to call the libm
// versions.

extern	float	acosf	(float);
extern	double	acos	(double);
extern	float	asinf	(float);
extern	double	asin	(double);
extern	float	atan2f	(float, float);
extern	double	atan2	(double, double);
extern	float	atanf	(float);
extern	double	atan	(double);
extern	float	cosf	(float);
extern	float	coshf	(float);
extern	double	cosh	(double);
extern	double	cos	(double);
extern	float	expf	(float);
extern	double	exp	(double);
extern	float	log10f	(float);
extern	double	log10	(double);
extern	float	logf	(float);
extern	double	fmod	(double,double);
extern	float	fmodf	(float,float);
extern	double	log	(double);
extern	float	powf	(float, float);
extern	double	pow	(double, double);
extern	float	sinf	(float);
extern	float	sinhf	(float);
extern	double	sinh	(double);
extern	double	sin	(double);
extern	float	tanf	(float);
extern	float	tanhf	(float);
extern	double	tanh	(double);
extern	double	tan	(double);

#define	powi	__mth_i_dpowi
#define	powk	__mth_i_dpowk
#define	powif	__mth_i_rpowi
#define	powkf	__mth_i_rpowk
#endif		/* TARGET_LINUX_POWER */

#if	defined(LINUX8664) || defined(TARGET_LINUX_POWER) || defined(TARGET_OSX_X8664)
#include "mthdecls.h"
//#endif
//#if	! defined(LINUX8664) && ! defined(TARGET_LINUX_POWER) || defined(TARGET_OSX_X8664)
#else		/* defined(LINUX8664) || defined(TARGET_LINUX_POWER) */
#error	"Missing routines for powi, powi1, powk, and powk1"
#include <math.h>
#endif		/* defined(LINUX8664) || defined(TARGET_LINUX_POWER) || defined(TARGET_OSX_X8664) */
