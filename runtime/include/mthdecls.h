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

#include <math.h>
#include <complex.h>

typedef struct {
  float real;
  float imag;
} cmplx_t;

typedef struct {
  double real;
  double imag;
} dcmplx_t;

/* macros to return a complex value from a function returning complex */
/* WARNING!! if the macros r_dummy or d_dummy are used within a
 * conditional, you MUST enclose their reference with braces, { },
 * otherwise incorrect execution will result.
 */

#define r_dummy(x, y)                                                          \
  cmplx->real = x;                                                             \
  cmplx->imag = y;                                                             \
  return;

#define d_dummy(x, y)                                                          \
  dcmplx->real = x;                                                            \
  dcmplx->imag = y;                                                            \
  return;

/* the following macros are defined in case a future unix release has
   single precision versions of the math.h functions, in which case the
   single precision versions should be used:  */

#if defined(WIN64) || defined(SOLARIS8664)

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
#define COPYSIGNF copysignf
#define COPYSIGN copysign
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
#if defined(WIN64)
#define POWF __mth_i_dpowd
#define COPYSIGNF _copysignf
#define COPYSIGN _copysign
#ifdef __PGI_TOOLS9
#define hypot _hypot
#endif
#else
#define POWF pow
#endif

#else
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
#endif

/*
#if defined(INTERIX86)
extern double HYPOTF();
#endif
*/

#if defined(SOLARIS8664)
extern double hypot(double, double);
#endif

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
void __mth_i_cacos(cmplx_t *cmplx, float real, float imag);
void __mth_i_casin(cmplx_t *cmplx, float real, float imag);
void __mth_i_catan(cmplx_t *cmplx, float real, float imag);
void __mth_i_ccosh(cmplx_t *cmplx, float real, float imag);
void __mth_i_csinh(cmplx_t *cmplx, float real, float imag);
void __mth_i_ctanh(cmplx_t *cmplx, float real, float imag);
void __mth_i_ctan(cmplx_t *cmplx, float real, float imag);

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
void __mth_i_cdacos(dcmplx_t *dcmplx, double real, double imag);
void __mth_i_cdasin(dcmplx_t *dcmplx, double real, double imag);
void __mth_i_cdatan(dcmplx_t *dcmplx, double real, double imag);
void __mth_i_cdcosh(dcmplx_t *cmplx, double real, double imag);
void __mth_i_cdsinh(dcmplx_t *cmplx, double real, double imag);
void __mth_i_cdtanh(dcmplx_t *cmplx, double real, double imag);
void __mth_i_cdtan(dcmplx_t *cmplx, double real, double imag);

void __mth_sincos(float, float *, float *);
void __mth_dsincos(double, double *, double *);

float __mth_i_cabs(float real, float imag);
void __mth_i_cexp(cmplx_t *cmplx, float real, float imag);
void __mth_i_ccos(cmplx_t *cmplx, float real, float imag);
void __mth_i_csin(cmplx_t *cmplx, float real, float imag);
void __mth_i_clog(cmplx_t *cmplx, float real, float imag);
void __mth_i_csqrt(cmplx_t *cmplx, float real, float imag);
void __mth_i_cpowc(cmplx_t *cmplx, float real1, float imag1, float real2,
                   float imag2);
void __mth_i_cdiv(cmplx_t *cmplx, float real1, float imag1, float real2,
                  float imag2);
void __mth_i_cdivr(cmplx_t *cmplx, float real1, float imag1, float r);
void __mth_i_cpowi(cmplx_t *cmplx, float real, float imag, int i);

double __mth_i_cdabs(double real, double imag);
void __mth_i_cddiv(dcmplx_t *dcmplx, double real1, double imag1, double real2,
                   double imag2);
void __mth_i_cddivd(dcmplx_t *dcmplx, double real1, double imag1, double d);
void __mth_i_cdpowi(dcmplx_t *dcmplx, double real, double imag, int i);
void __mth_i_cdexp(dcmplx_t *dcmplx, double real, double imag);
void __mth_i_cdcos(dcmplx_t *dcmplx, double real, double imag);
void __mth_i_cdsin(dcmplx_t *dcmplx, double real, double imag);
void __mth_i_cdlog(dcmplx_t *dcmplx, double real, double imag);
void __mth_i_cdsqrt(dcmplx_t *dcmplx, double real, double imag);
void __mth_i_cdpowcd(dcmplx_t *dcmplx, double real1, double imag1, double real2,
                     double imag2);


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
