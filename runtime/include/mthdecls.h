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

/**
 * \file
 * \brief  mthdecls.h - Fortran math support (all platforms/targets) 
 */

/* pi/180 */
#define DEG_TO_RAD 0.174532925199432957692E-1
/* 180/pi */
#define RAD_TO_DEG 0.572957795130823208769E+2
#define CNVRTDEG(degrees) ((degrees)*DEG_TO_RAD)
#define CNVRTRAD(radians) ((radians)*RAD_TO_DEG)

/*
 * define a C type for long long so that the routines using this type
 * will always compile.  For those systems where long long isn't
 * supported, TM_I8 will not be defined, but at least the run-time routines
 * will compile.
 */
#if defined(TM_I8) || defined(LONG_IS_32)
typedef long long _LONGLONG_T;
typedef unsigned long long _ULONGLONG_T;
#else
typedef long _LONGLONG_T;
typedef unsigned long _ULONGLONG_T;
#endif

#ifndef	MTH_NO_STD_MATH_HDRS
#ifdef __cplusplus
#include <cmath>
#else
#include <math.h>
#endif
#include <complex.h>
#endif

#ifdef __cplusplus
typedef _Complex float complex_float;
typedef _Complex double complex_double;
#endif

typedef struct {
  float real;
  float imag;
} cmplx_t;

typedef struct {
  double real;
  double imag;
} dcmplx_t;

#if defined(__PGIC__)
#undef	creal
#define creal(x) __builtin_creal(x)
double __builtin_creal(double complex);

#undef	cimag
#define cimag(x) __builtin_cimag(x)
double __builtin_cimag(double complex);

#undef	crealf
#define crealf(x) __builtin_crealf(x)
float __builtin_crealf(float complex);

#undef	cimagf
#define cimagf(x) __builtin_cimagf(x)
float __builtin_cimagf(float complex);

#endif

#define	MTHCONCAT___(l,r)	l##r
#define	MTHCONCAT__(l,r)	MTHCONCAT___(l,r)

#define	__MTH_C99_CMPLX_SUFFIX	_c99

/* Old complex ABI */
#define	FLTFUNC_C_(_f)    \
        float _f(float real, float imag)
#define	DBLFUNC_C_(_f)    \
        double _f(double real, double imag)

#define	CMPLXFUNC_C_(_f)    \
        void _f(cmplx_t *cmplx, float real, float imag)
#define	CMPLXFUNC_C_C_(_f)  \
        void _f(cmplx_t *cmplx, float real1, float imag1, \
                                  float real2, float imag2)
#define	CMPLXFUNC_C_F_(_f)  \
        void _f(cmplx_t *cmplx, float real, float imag, float r)
#define	CMPLXFUNC_C_I_(_f)  \
        void _f(cmplx_t *cmplx, float real, float imag, int i)
#define	CMPLXFUNC_C_K_(_f)  \
        void _f(cmplx_t *cmplx, float real, float imag, long long i)

#define	ZMPLXFUNC_Z_(_f)    \
        void _f(dcmplx_t *dcmplx, double real, double imag)
#define	ZMPLXFUNC_Z_Z_(_f)  \
        void _f(dcmplx_t *dcmplx, double real1, double imag1, \
                                    double real2, double imag2)
#define	ZMPLXFUNC_Z_D_(_f)  \
        void _f(dcmplx_t *dcmplx, double real, double imag, double d)
#define	ZMPLXFUNC_Z_I_(_f)  \
        void _f(dcmplx_t *dcmplx, double real, double imag, int i)
#define	ZMPLXFUNC_Z_K_(_f)  \
        void _f(dcmplx_t *dcmplx, double real, double imag, long long i)

/* C99 complex ABI */
#ifdef __cplusplus
#define FLTFUNC_C_C99_(_f)    \
        float MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (complex_float carg)
#define DBLFUNC_C_C99_(_f)    \
        double MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (complex_double zarg)

#define CMPLXFUNC_C_C99_(_f)    \
        complex_float MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (complex_float carg)
#define CMPLXFUNC_C_C_C99_(_f)  \
        complex_float MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (complex_float carg1, complex_float carg2)
#define CMPLXFUNC_C_F_C99_(_f)  \
        complex_float MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (complex_float carg, float r)
#define CMPLXFUNC_C_I_C99_(_f)  \
        complex_float MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (complex_float carg, int i)
#define CMPLXFUNC_C_K_C99_(_f)  \
        complex_float MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (complex_float carg, long long i)

#define ZMPLXFUNC_Z_C99_(_f)    \
        complex_double MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (complex_double zarg)
#define ZMPLXFUNC_Z_Z_C99_(_f)  \
        complex_double MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (complex_double zarg1, complex_double zarg2)
#define ZMPLXFUNC_Z_D_C99_(_f)  \
        complex_double MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (complex_double zarg, double d)
#define ZMPLXFUNC_Z_I_C99_(_f)  \
        complex_double MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (complex_double zarg, int i)
#define ZMPLXFUNC_Z_K_C99_(_f)  \
        complex_double MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (complex_double zarg, long long i)
#else
#define	FLTFUNC_C_C99_(_f)    \
        float MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (float complex carg)
#define	DBLFUNC_C_C99_(_f)    \
        double MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (double complex zarg)

#define	CMPLXFUNC_C_C99_(_f)    \
        float complex MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (float complex carg)
#define	CMPLXFUNC_C_C_C99_(_f)  \
        float complex MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (float complex carg1, float complex carg2)
#define	CMPLXFUNC_C_F_C99_(_f)  \
        float complex MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (float complex carg, float r)
#define	CMPLXFUNC_C_I_C99_(_f)  \
        float complex MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (float complex carg, int i)
#define	CMPLXFUNC_C_K_C99_(_f)  \
        float complex MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (float complex carg, long long i)

#define	ZMPLXFUNC_Z_C99_(_f)    \
        double complex MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (double complex zarg)
#define	ZMPLXFUNC_Z_Z_C99_(_f)  \
        double complex MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (double complex zarg1, double complex zarg2)
#define	ZMPLXFUNC_Z_D_C99_(_f)  \
        double complex MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (double complex zarg, double d)
#define	ZMPLXFUNC_Z_I_C99_(_f)  \
        double complex MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (double complex zarg, int i)
#define	ZMPLXFUNC_Z_K_C99_(_f)  \
        double complex MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX) \
        (double complex zarg, long long i)
#endif /* __cplusplus */

#ifndef	MTH_CMPLX_C99_ABI

#define	FLTFUNC_C(_f)		FLTFUNC_C_(_f)
#define	DBLFUNC_C(_f)		DBLFUNC_C_(_f)

#define	CMPLXFUNC_C(_f)		CMPLXFUNC_C_(_f)
#define	CMPLXFUNC_C_C(_f)	CMPLXFUNC_C_C_(_f)
#define	CMPLXFUNC_C_F(_f)	CMPLXFUNC_C_F_(_f)
#define	CMPLXFUNC_C_I(_f)	CMPLXFUNC_C_I_(_f)
#define	CMPLXFUNC_C_K(_f)	CMPLXFUNC_C_K_(_f)

#define	ZMPLXFUNC_Z(_f)		ZMPLXFUNC_Z_(_f)
#define	ZMPLXFUNC_Z_Z(_f)	ZMPLXFUNC_Z_Z_(_f)
#define	ZMPLXFUNC_Z_D(_f)	ZMPLXFUNC_Z_D_(_f)
#define	ZMPLXFUNC_Z_I(_f)	ZMPLXFUNC_Z_I_(_f)
#define	ZMPLXFUNC_Z_K(_f)	ZMPLXFUNC_Z_K_(_f)

#define CMPLXARGS_C
#define ZMPLXARGS_Z
#define CMPLXARGS_C_C
#define CMPLXARGS_C_F
#define CMPLXARGS_C_I
#define CMPLXARGS_C_K
#define ZMPLXARGS_Z_Z
#define ZMPLXARGS_Z_D
#define ZMPLXARGS_Z_I
#define ZMPLXARGS_Z_K

#define	CRETURN_F_F(_r, _i) do { cmplx->real = (_r); cmplx->imag = (_i); return; } while (0)
#define	ZRETURN_D_D(_r, _i) do { dcmplx->real = (_r); dcmplx->imag = (_i); return; } while (0)
#define CRETURN_C(_c)       do { (*cmplx = *((cmplx_t *)&(_c))); return; } while (0)
#define ZRETURN_Z(_z)       do { (*dcmplx = *((dcmplx_t *)&(_z))); return; } while (0)
#define CRETURN_F(_f)       return (_f)
#define ZRETURN_D(_d)       return (_d)

#define CMPLX_CALL_CR_C_C(_f,_cr,_c1,_c2) \
{ _f(cmplx, crealf(_c1), cimagf(_c1), crealf(_c2), cimagf(_c2)); \
  *(cmplx_t *)&_cr = *cmplx; }
#define ZMPLX_CALL_ZR_Z_Z(_f,_zr,_z1,_z2) \
{ _f(dcmplx, creal(_z1), cimag(_z1), creal(_z2), cimag(_z2)); \
  *(dcmplx_t *)&_zr = *dcmplx; }

#else		/* #ifdef MTH_CMPLX_C99_ABI */

#define	FLTFUNC_C(_f)		FLTFUNC_C_C99_(_f)
#define	DBLFUNC_C(_f)		DBLFUNC_C_C99_(_f)

#define	CMPLXFUNC_C(_f)		CMPLXFUNC_C_C99_(_f)
#define	CMPLXFUNC_C_C(_f)	CMPLXFUNC_C_C_C99_(_f)
#define	CMPLXFUNC_C_F(_f)	CMPLXFUNC_C_F_C99_(_f)
#define	CMPLXFUNC_C_I(_f)	CMPLXFUNC_C_I_C99_(_f)
#define	CMPLXFUNC_C_K(_f)	CMPLXFUNC_C_K_C99_(_f)

#define	ZMPLXFUNC_Z(_f)		ZMPLXFUNC_Z_C99_(_f)
#define	ZMPLXFUNC_Z_Z(_f)	ZMPLXFUNC_Z_Z_C99_(_f)
#define	ZMPLXFUNC_Z_D(_f)	ZMPLXFUNC_Z_D_C99_(_f)
#define	ZMPLXFUNC_Z_I(_f)	ZMPLXFUNC_Z_I_C99_(_f)
#define	ZMPLXFUNC_Z_K(_f)	ZMPLXFUNC_Z_K_C99_(_f)

#define CMPLXARGS_C     float real = crealf(carg), imag = cimagf(carg)
#define CMPLXARGS_C_C   float real1 = crealf(carg1), imag1 = cimagf(carg1), \
                              real2 = crealf(carg2), imag2 = cimagf(carg2)
#define	CMPLXARGS_C_F	CMPLXARGS_C
#define	CMPLXARGS_C_I	CMPLXARGS_C
#define	CMPLXARGS_C_K	CMPLXARGS_C

#define ZMPLXARGS_Z     double real = creal(zarg), imag = cimag(zarg)
#define ZMPLXARGS_Z_Z   double real1 = creal(zarg1), imag1 = cimag(zarg1), \
                               real2 = creal(zarg2), imag2 = cimag(zarg2)
#define	ZMPLXARGS_Z_D	ZMPLXARGS_Z
#define	ZMPLXARGS_Z_I	ZMPLXARGS_Z
#define	ZMPLXARGS_Z_K	ZMPLXARGS_Z

#define	CRETURN_F_F(_r, _i) return ((_r) + I * (_i))
#define	ZRETURN_D_D(_r, _i) return ((_r) + I * (_i))
#define CRETURN_C(_c)       return (_c)
#define ZRETURN_Z(_z)       return (_z)
#define CRETURN_F(_f)       return (_f)
#define ZRETURN_D(_d)       return (_d)

#define CMPLX_CALL_CR_C_C(_f,_cr,_c1,_c2) \
{_cr = MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX)(_c1, _c2); }
#define ZMPLX_CALL_ZR_Z_Z(_f,_zr,_z1,_z2) \
{_zr = MTHCONCAT__(_f,__MTH_C99_CMPLX_SUFFIX)(_z1, _z2); }

#endif		/* #ifdef MTH_CMPLX_C99_ABI */

/*
 * Define complex function declarations for both old and C99 ABI.
 * Declarations should only be used in mthdecls.h.
 * Function definitions should use/begin with the "...FUNC_..." macros.
 *
 * Note: semicolon ";" in statements.
 */
#define	FLTDECL_C(_f)		FLTFUNC_C_(_f)     ; FLTFUNC_C_C99_(_f);
#define	DBLDECL_C(_f)		DBLFUNC_C_(_f)     ; DBLFUNC_C_C99_(_f);

#define	CMPLXDECL_C(_f)		CMPLXFUNC_C_(_f)   ; CMPLXFUNC_C_C99_(_f);
#define	CMPLXDECL_C_C(_f)	CMPLXFUNC_C_C_(_f) ; CMPLXFUNC_C_C_C99_(_f);
#define	CMPLXDECL_C_F(_f)	CMPLXFUNC_C_F_(_f) ; CMPLXFUNC_C_F_C99_(_f);
#define	CMPLXDECL_C_I(_f)	CMPLXFUNC_C_I_(_f) ; CMPLXFUNC_C_I_C99_(_f);
#define	CMPLXDECL_C_K(_f)	CMPLXFUNC_C_K_(_f) ; CMPLXFUNC_C_K_C99_(_f);

#define	ZMPLXDECL_Z(_f)		ZMPLXFUNC_Z_(_f)   ; ZMPLXFUNC_Z_C99_(_f);
#define	ZMPLXDECL_Z_Z(_f)	ZMPLXFUNC_Z_Z_(_f) ; ZMPLXFUNC_Z_Z_C99_(_f);
#define	ZMPLXDECL_Z_D(_f)	ZMPLXFUNC_Z_D_(_f) ; ZMPLXFUNC_Z_D_C99_(_f);
#define	ZMPLXDECL_Z_I(_f)	ZMPLXFUNC_Z_I_(_f) ; ZMPLXFUNC_Z_I_C99_(_f);
#define	ZMPLXDECL_Z_K(_f)	ZMPLXFUNC_Z_K_(_f) ; ZMPLXFUNC_Z_K_C99_(_f);

/* the following macros are defined in case a future unix release has
   single precision versions of the math.h functions, in which case the
   single precision versions should be used:  */

#if defined(WIN64)

#define ACOSF acos
#define ASINF asin
#define ATANF atan
#define ATAN2F atan2
#define ACOSHF acosh
#define ASINHF asinh
#define ATANHF atanh
#define COSF cos
#define SINF sin
#define TANF tan
#define COSHF cosh
#define SINHF sinh
#define TANHF tanh
#define EXPF exp
#define FREXPF frexp
#define LDEXPF ldexp
#define LOGF log
#define LOG10F log10
#define MODFF modf
#define SQRTF sqrt
#define CEILF ceil
#define FABSF fabs
#define FLOORF floor
#define FMODF fmod
#define HYPOTF hypot
#define COPYSIGNF _copysignf
#define COPYSIGN _copysign
#define ERFF erf
#define ERFCF erfc
#define GAMMAF tgamma
#define LOG_GAMMAF lgamma
#define BESSEL_J0F _j0
#define BESSEL_J1F _j1
#define BESSEL_JNF _jn
#define BESSEL_Y0F _y0
#define BESSEL_Y1F _y1
#define BESSEL_YNF _yn
#define BESSEL_J0 _j0
#define BESSEL_J1 _j1
#define BESSEL_JN _jn
#define BESSEL_Y0 _y0
#define BESSEL_Y1 _y1
#define BESSEL_YN _yn
#define CACOSF cacos
#define CASINF casin
#define CATANF catan
#define CCOSHF ccosh
#define CSINHF csinh
#define CTANHF ctanh
#define CTANF ctan

/* define POWF specially here for win64 until we can leverage
 * our usual builtin mechanism on that target
 * Also, with MSOT8, hypot is depcrecated, and will not link
 * correctly. Need to use _hypot.
 */
#define POWF __mth_i_dpowd
#ifdef __PGI_TOOLS9
#define hypot _hypot
#endif

#else		/* #if defined (WIN64) */
#define ACOSF acosf
#define ASINF asinf
#define ATANF atanf
#define ATAN2F atan2f
#define ACOSHF acoshf
#define ASINHF asinhf
#define ATANHF atanhf
#define COSF cosf
#define SINF sinf
#define TANF tanf
#define COSHF coshf
#define SINHF sinhf
#define TANHF tanhf
#define EXPF expf
#define FREXPF frexp
#define LDEXPF ldexp
#define LOGF logf
#define LOG10F log10f
#define MODFF modff
#define POWF powf
#define SQRTF sqrt
#define CEILF ceilf
#define FABSF fabs
#define FLOORF floorf
#define FMODF fmodf
#ifdef __PGI_TOOLS14
#define HYPOTF _hypot
#else
#define HYPOTF hypotf
#endif
#define ERFF erff
#define ERFCF erfcf
#define GAMMAF tgammaf
#define LOG_GAMMAF lgammaf
#define COPYSIGNF copysignf
#define COPYSIGN copysign

#if !defined(TARGET_WIN)
#define CACOSF cacosf
#define CASINF casinf
#define CATANF catanf
#define CCOSHF ccoshf
#define CSINHF csinhf
#define CTANHF ctanhf
#define CTANF ctanf
#else
#define CACOSF cacos
#define CASINF casin
#define CATANF catan
#define CCOSHF ccosh
#define CSINHF csinh
#define CTANHF ctanh
#define CTANF ctan
#endif

#if defined(TARGET_WIN)
#define BESSEL_J0F _j0
#define BESSEL_J1F _j1
#define BESSEL_JNF _jn
#define BESSEL_Y0F _y0
#define BESSEL_Y1F _y1
#define BESSEL_YNF _yn

#define BESSEL_J0 _j0
#define BESSEL_J1 _j1
#define BESSEL_JN _jn
#define BESSEL_Y0 _y0
#define BESSEL_Y1 _y1
#define BESSEL_YN _yn
#elif defined(TARGET_OSX)
#define BESSEL_J0F j0
#define BESSEL_J1F j1
#define BESSEL_JNF jn
#define BESSEL_Y0F y0
#define BESSEL_Y1F y1
#define BESSEL_YNF yn

#define BESSEL_J0 j0
#define BESSEL_J1 j1
#define BESSEL_JN jn
#define BESSEL_Y0 y0
#define BESSEL_Y1 y1
#define BESSEL_YN yn

#define BESSEL_J0 j0
#define BESSEL_J1 j1
#define BESSEL_JN jn
#define BESSEL_Y0 y0
#define BESSEL_Y1 y1
#define BESSEL_YN yn
#else
#define BESSEL_J0F j0f
#define BESSEL_J1F j1f
#define BESSEL_JNF jnf
#define BESSEL_Y0F y0f
#define BESSEL_Y1F y1f
#define BESSEL_YNF ynf

#define BESSEL_J0 j0
#define BESSEL_J1 j1
#define BESSEL_JN jn
#define BESSEL_Y0 y0
#define BESSEL_Y1 y1
#define BESSEL_YN yn
#endif
#endif		/* #if defined (WIN64) */

/*  declarations for math functions */

float __mth_i_acos(float f);
float __mth_i_acosh(float f);
float __mth_i_alog(float f);
float __mth_i_alog10(float f);
float __mth_i_asin(float f);
float __mth_i_asinh(float f);
float __mth_i_atan(float f);
float __mth_i_atanh(float f);
float __mth_i_atan2(float f, float g);
float __mth_i_exp(float f);
float __mth_i_rpowr(float f, float g);
float __mth_i_sin(float f);
float __mth_i_sinh(float f);
float __mth_i_sqrt(float f);
float __mth_i_tan(float f);
float __mth_i_tanh(float f);
float __mth_i_amod(float f, float g);
float __mth_i_aint(float f);
float __mth_i_anint(float f);
float __mth_i_cosh(float f);
float __mth_i_cos(float f);
float __mth_i_rpowi(float x, int i);
float __mth_i_rpowk(float x, long long i);
float __pmth_i_rpowi(float x, int i);		/* Compute R4**I8 in R8 precision */
float __pmth_i_rpowk(float x, long long i);	/* Compute R4**I4 in R8 precision */
float __mth_i_acosd(float f);
float __mth_i_asind(float f);
float __mth_i_atand(float f);
float __mth_i_atan2d(float f, float g);
float __mth_i_sind(float f);
float __mth_i_tand(float f);
float __mth_i_cosd(float f);
float __mth_i_erf(float f);
float __mth_i_erfc(float f);
float __mth_i_erfc_scaled(float f);
float __mth_i_gamma(float f);
float __mth_i_log_gamma(float f);
float __mth_i_hypotf(float x, float y);
float __mth_i_bessel_j0(float arg);
float __mth_i_bessel_j1(float arg);
float __mth_i_bessel_jn(int n, float arg);
float __f90_bessel_jn(int n1, int n2, float f);
float __mth_i_bessel_y0(float arg);
float __mth_i_bessel_y1(float arg);
float __mth_i_bessel_yn(int n, float arg);
float __f90_bessel_yn(int n1, int n2, float f);

int __mth_i_idnint(double d);
int __mth_i_mod(int i, int j);
int __mth_i_nint(float d);
int __mth_i_ipowi(int x, int i);

double __mth_i_dacos(double d);
double __mth_i_dacosh(double d);
double __mth_i_dasin(double d);
double __mth_i_dasinh(double d);
double __mth_i_datan(double d);
double __mth_i_datanh(double d);
double __mth_i_datan2(double x, double y);
double __mth_i_dcos(double d);
double __mth_i_dcosh(double d);
double __mth_i_dexp(double d);
double __mth_i_dlog(double d);
double __mth_i_dlog10(double d);
double __mth_i_dpowd(double x, double y);
double __mth_i_dsin(double d);
double __mth_i_dsinh(double d);
double __mth_i_dsqrt(double d);
double __mth_i_dtan(double d);
double __mth_i_dtanh(double d);
double __mth_i_dmod(double f, double g);
double __mth_i_dint(double d);
double __mth_i_dnint(double d);
double __mth_i_dpowi(double x, int i);
double __mth_i_dpowk(double x, long long i);
double __pmth_i_dpowi(double x, int i);		/* Compute R8**I8 in R16 precision */
double __pmth_i_dpowk(double x, long long i);	/* Compute R8**I4 in R16 precision */
double __mth_i_dacosd(double f);
double __mth_i_dasind(double f);
double __mth_i_datand(double f);
double __mth_i_datan2d(double f, double g);
double __mth_i_dsind(double f);
double __mth_i_dtand(double f);
double __mth_i_dcosd(double f);
double __mth_i_derf(double f);
double __mth_i_derfc(double f);
double __mth_i_derfc_scaled(double f);
double __mth_i_dgamma(double f);
double __mth_i_dlog_gamma(double f);
double __mth_i_dhypot(double, double);
double __mth_i_pow(double, double);
double __mth_i_dbessel_j0(double arg);
double __mth_i_dbessel_j1(double arg);
double __mth_i_dbessel_jn(int n, double arg);
double __f90_dbessel_jn(int n1, int n, double d);
double __mth_i_dbessel_y0(double arg);
double __mth_i_dbessel_y1(double arg);
double __mth_i_dbessel_yn(int n, double arg);
double __f90_dbessel_yn(int n1, int n, double d);

#if	! defined (TARGET_X8664) && ! defined(LINUX8664)
/*
 * See explanation below for rationale behind the two flavors of __mth_sincos.
 */
static inline void __mth_sincos(float angle, float *s, float *c)
        __attribute__((always_inline));
static inline void __mth_dsincos(double angle, double *s, double *c)
        __attribute__((always_inline));
#else	/* ! defined (TARGET_X8664) && ! defined(LINUX8664) */
void __mth_sincos(float, float *, float *);
void __mth_dsincos(double, double *, double *);
#endif	/* ! defined (TARGET_X8664) && ! defined(LINUX8664) */

FLTDECL_C(__mth_i_cabs);
CMPLXDECL_C(__mth_i_cacos);
CMPLXDECL_C(__mth_i_casin);
CMPLXDECL_C(__mth_i_catan);
CMPLXDECL_C(__mth_i_ccos);
CMPLXDECL_C(__mth_i_ccosh);
CMPLXDECL_C_C(__mth_i_cdiv);
CMPLXDECL_C_F(__mth_i_cdivr);
CMPLXDECL_C(__mth_i_cexp);
CMPLXDECL_C(__mth_i_clog);
CMPLXDECL_C_C(__mth_i_cpowc);
CMPLXDECL_C_I(__mth_i_cpowi);
CMPLXDECL_C_K(__mth_i_cpowk);
CMPLXDECL_C(__mth_i_csin);
CMPLXDECL_C(__mth_i_csinh);
CMPLXDECL_C(__mth_i_csqrt);
CMPLXDECL_C(__mth_i_ctan);
CMPLXDECL_C(__mth_i_ctanh);

DBLDECL_C(__mth_i_cdabs);
ZMPLXDECL_Z(__mth_i_cdacos);
ZMPLXDECL_Z(__mth_i_cdasin);
ZMPLXDECL_Z(__mth_i_cdatan);
ZMPLXDECL_Z(__mth_i_cdcos);
ZMPLXDECL_Z(__mth_i_cdcosh);
ZMPLXDECL_Z_Z(__mth_i_cddiv);
ZMPLXDECL_Z_D(__mth_i_cddivd);
ZMPLXDECL_Z(__mth_i_cdexp);
ZMPLXDECL_Z(__mth_i_cdlog);
ZMPLXDECL_Z_Z(__mth_i_cdpowcd);
ZMPLXDECL_Z_I(__mth_i_cdpowi);
ZMPLXDECL_Z_K(__mth_i_cdpowk);
ZMPLXDECL_Z(__mth_i_cdsin);
ZMPLXDECL_Z(__mth_i_cdsinh);
ZMPLXDECL_Z(__mth_i_cdsqrt);
ZMPLXDECL_Z(__mth_i_cdtan);
ZMPLXDECL_Z(__mth_i_cdtanh);



#if defined(TARGET_WIN)
/* the following are part of Open Tools 12, we build with Open Tools 10 */
extern double erf(double x);
extern float erff(float x);
extern double erfc(double x);
extern float erfcf(float x);
extern double lgamma(double);
extern float lgammaf(float);
extern double tgamma(double);
extern float tgammaf(float);
extern double acosh(double);
extern float acoshf(float);
extern double asinh(double);
extern float asinhf(float);
extern double atanh(double);
extern float atanhf(float);
extern double _j0(double arg);
extern double _j1(double arg);
extern double _jn(int n, double arg);
extern double _y0(double arg);
extern double _y1(double arg);
extern double _yn(int n, double arg);
extern complex float cacosf(complex float);
extern complex double cacos(complex double);
extern complex float casinf(complex float);
extern complex double casin(complex double);
extern complex float catanf(complex float);
extern complex double catan(complex double);
extern complex float ccoshf(complex float);
extern complex double ccosh(complex double);
extern complex float csinhf(complex float);
extern complex double csinh(complex double);
extern complex float ctanhf(complex float);
extern complex double ctanh(complex double);
extern complex float ctanf(complex float);
extern complex double ctan(complex double);
#endif

/*
 * The following intrinsics are defined for platforms that do not have
 * architecture specific versions.
 * It is an attempt to standardize the math library source code across
 * architectures.
 *
 * For example: cexp.c was coded as:
 * #include "mthdecls.h"
 *
 * 	void
 * 	__mth_i_cexp(cmplx_t *cmplx, float real, float imag)
 *	 {
 *	   float x, y, z;
 *	    x = EXPF(real);
 *	 #ifndef LINUX8664
 *	    y = COSF(imag);
 *	    z = SINF(imag);
 *	  #else
 *	    __mth_sincos(imag, &z, &y);
 *	  #endif
 *	    y *= x;
 *	    z *= x;
 *	    r_dummy(y, z);
 *	  }
 *
 * The special casing of whether __mth_sincos() is available for
 * individual source files is not scalable.  A better alternative is to
 * have a version of __mth_sincos, even if it is not external available
 * during the build process.
 */

#define	__mth_sincos(_a,_s,_c) sincosf(_a,_s,_c)
#define	__mth_dsincos(_a,_s,_c) sincos(_a,_s,_c)
