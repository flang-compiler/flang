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

/** \file
 * \brief  Set ieee floating point environment.
 */

#if (defined(TARGET_X8664) || defined(TARGET_X86) || defined(X86)) 

/* These routines are included in linux and osx.
   Plus, we can standardize our support of F2003 ieee_exceptions
   and ieee_arithmetic modules across all platforms
*/

typedef struct {
  unsigned int mxcsr;
  unsigned int x87cw;
  unsigned int x87sw;
} fenv_t;

typedef unsigned int fexcept_t;

#define IS_SSE_ENABLED 1
#define IS_SSE2_ENABLED 1

typedef union {
  int w;
  struct {
    unsigned int ie : 1;
    unsigned int de : 1;
    unsigned int ze : 1;
    unsigned int oe : 1;
    unsigned int ue : 1;
    unsigned int pe : 1;
    unsigned int rs2 : 2;
    unsigned int pc : 2; /* 0 - 32
                          * 1 - reserved
                          * 2 - 64
                          * 3 - 80
                          */
    unsigned int rc : 2; /* 0 - round to nearest
                          * 1 - round down
                          * 2 - round up
                          * 3 - chop
                          */
    unsigned int ic : 1;
    unsigned int rs3 : 3;
  } xbits;
} FCW;

typedef union {
  int w;
  struct {
    unsigned int ie : 1;
    unsigned int de : 1;
    unsigned int ze : 1;
    unsigned int oe : 1;
    unsigned int ue : 1;
    unsigned int pe : 1;
    unsigned int daz : 1;
    unsigned int iem : 1;
    unsigned int dem : 1;
    unsigned int zem : 1;
    unsigned int oem : 1;
    unsigned int uem : 1;
    unsigned int pem : 1;
    unsigned int rc : 2; /* 0 - round to nearest
                          * 1 - round down
                          * 2 - round up
                          * 3 - chop
                          */
    unsigned int fz : 1;
  } mbits;
} MXCSR;

int
__fenv_fegetround(void)
{
  FCW x87;
  MXCSR sse;

  int rmode_x87;
  int rmode_sse;
  rmode_sse = 0;

  asm("\tfnstcw %0" : "=m"(x87) :);
  rmode_x87 = x87.xbits.rc;
  if (IS_SSE_ENABLED) {
    asm("\tstmxcsr %0" : "=m"(sse) :);
    rmode_sse = sse.mbits.rc;
  }
  return ((rmode_x87 | rmode_sse) << 10);
}

int
__fenv_fesetround(int rmode)
{
  FCW x87;
  MXCSR sse;

  asm("\tfnstcw %0" : "=m"(x87) :);
  x87.xbits.rc = (rmode >> 10);
  asm("\tfldcw %0" ::"m"(x87));
  if (IS_SSE_ENABLED) {
    asm("\tstmxcsr %0" : "=m"(sse) :);
    sse.mbits.rc = (rmode >> 10);
    asm("\tldmxcsr %0" ::"m"(sse));
  }
  return 0;
}

int
__fenv_fegetexceptflag(fexcept_t *flagp, int exc)
{
  int x87;
  int sse;
  sse = 0;

  asm("\tfnstsw %0" : "=m"(x87) :);
  x87 &= exc;
  if (IS_SSE_ENABLED) {
    asm("\tstmxcsr %0" : "=m"(sse) :);
    sse &= exc;
  }
  *flagp = ((x87 | sse) & 63);
  return 0;
}

int
__fenv_fesetexceptflag(fexcept_t *flagp, int exc)
{
  unsigned int x87[7];
  unsigned int sse;
  unsigned int uexc;

  uexc = exc & 63;
  asm("\tfnstenv %0" : "=m"(x87[0]) :);
  x87[1] &= ~uexc;
  x87[1] |= (uexc & *flagp);
  asm("\tfldenv %0\n\tfwait" ::"m"(x87[0]));
  if (IS_SSE_ENABLED) {
    asm("\tstmxcsr %0" : "=m"(sse) :);
    sse &= ~uexc;
    sse |= (uexc & *flagp);
    asm("\tldmxcsr %0" ::"m"(sse));
  }
  return 0;
}

int
__fenv_fetestexcept(int exc)
{
  int x87;
  int sse;
  sse = 0;

/* Windows doesn't seem to preserve x87 exception bits across context
 * switches, so this info is unreliable.
 */
#ifdef WINNT
  x87 = 0;
#else
  asm("\tfnstsw %0" : "=m"(x87) :);
  x87 &= exc;
#endif
  if (IS_SSE_ENABLED) {
    asm("\tstmxcsr %0" : "=m"(sse) :);
    sse &= exc;
  }
  return ((x87 | sse) & 63);
}

int
__fenv_feclearexcept(int exc)
{
  unsigned int x87[7];
  int sse;
  unsigned int uexc;

  uexc = exc & 63;
  asm("\tfnstenv %0" : "=m"(x87[0]) :);
  x87[1] &= ~uexc;
  asm("\tfldenv %0\n\tfwait" ::"m"(x87[0]));

  if (IS_SSE_ENABLED) {
    asm("\tstmxcsr %0" : "=m"(sse) :);
    sse &= ~uexc;
    asm("\tldmxcsr %0" ::"m"(sse));
  }
  return 0;
}

int
__fenv_feclearx87except(int exc)
{
  unsigned int x87[7];
  int sse;
  unsigned int uexc;

  uexc = exc & 63;
  asm("\tfnstenv %0" : "=m"(x87[0]) :);
  x87[1] &= ~uexc;
  asm("\tfldenv %0\n\tfwait" ::"m"(x87[0]));
  return 0;
}

int
__fenv_feraiseexcept(int exc)
{
  unsigned int x87[7];
  int sse;

  exc &= 63;
  asm("\tfnstenv %0" : "=m"(x87[0]) :);
  x87[1] |= exc;
  asm("\tfldenv %0\n\tfwait" ::"m"(x87[0]));

  if (IS_SSE_ENABLED) {
    asm("\tstmxcsr %0" : "=m"(sse) :);
    sse |= exc;
    asm("\tldmxcsr %0" ::"m"(sse));
  }
  return 0;
}

int
__fenv_feenableexcept(int exc)
{
  unsigned int x87;
  unsigned int sse;
  unsigned int uexc;

  uexc = exc & 63;
  asm("\tfnstcw %0" : "=m"(x87) :);
  x87 &= ~uexc;
  asm("\tfldcw %0" ::"m"(x87));
  if (IS_SSE_ENABLED) {
    uexc = ((exc & 63) << 7);
    asm("\tstmxcsr %0" : "=m"(sse) :);
    sse &= ~uexc;
    asm("\tldmxcsr %0" ::"m"(sse));
  }
  return 0;
}

int
__fenv_fedisableexcept(int exc)
{
  int x87;
  int sse;

  asm("\tfnstcw %0" : "=m"(x87) :);
  x87 |= (exc & 63);
  asm("\tfldcw %0" ::"m"(x87));
  if (IS_SSE_ENABLED) {
    asm("\tstmxcsr %0" : "=m"(sse) :);
    sse |= ((exc & 63) << 7);
    asm("\tldmxcsr %0" ::"m"(sse));
  }
  return 0;
}

int
__fenv_fegetexcept(void)
{
  int x87;
  int sse;

  sse = 0;
  asm("\tfnstcw %0" : "=m"(x87) :);
  if (IS_SSE_ENABLED) {
    asm("\tstmxcsr %0" : "=m"(sse) :);
    sse = sse >> 7;
  }
  return (63 - ((x87 | sse) & 63));
}

int
__fenv_fegetenv(fenv_t *env)
{
  unsigned int fcw;
  unsigned int x87[7];
  unsigned int sse;

  asm("\tfnstcw %0" : "=m"(fcw) :);
  env->x87cw = fcw;
  asm("\tfnstenv %0" : "=m"(x87[0]) :);
  env->x87sw = x87[1];
  if (IS_SSE_ENABLED) {
    asm("\tstmxcsr %0" : "=m"(sse) :);
    env->mxcsr = sse;
  }
  return 0;
}

int
__fenv_feholdexcept(fenv_t *env)
{
  unsigned int fcw;
  unsigned int x87[7];
  unsigned int sse;
  unsigned int uexc;

  asm("\tfnstcw %0" : "=m"(fcw) :);
  env->x87cw = fcw;
  asm("\tfnstenv %0" : "=m"(x87[0]) :);
  env->x87sw = x87[1];
  if (IS_SSE_ENABLED) {
    asm("\tstmxcsr %0" : "=m"(sse) :);
    env->mxcsr = sse;
  }
  uexc = 63;
  fcw |= uexc;
  asm("\tfldcw %0" ::"m"(fcw));
  x87[1] &= ~uexc;
  asm("\tfldenv %0" ::"m"(x87[0]));

  if (IS_SSE_ENABLED) {
    sse &= ~uexc;
    sse |= (uexc << 7);
    asm("\tldmxcsr %0" ::"m"(sse));
  }
  return 0;
}

int
__fenv_fesetenv(fenv_t *env)
{
  unsigned int fcw;
  unsigned int x87[7];
  unsigned int sse;
  unsigned int uexc;

  asm("\tfnstcw %0" : "=m"(fcw) :);
  fcw = fcw & 0xFFFFF0C0;
  fcw = fcw | (env->x87cw & 0x00000F3F);
  asm("\tfldcw %0" ::"m"(fcw));

  asm("\tfnstenv %0" : "=m"(x87[0]) :);
  x87[1] = x87[1] & 0xFFFFFFC0;
  x87[1] = x87[1] | (env->x87sw & 0x0000003F);
  asm("\tfldenv %0" ::"m"(x87[0]));

  if (IS_SSE_ENABLED) {
    asm("\tstmxcsr %0" : "=m"(sse) :);
    sse = sse & 0xFFFF8040;
    sse = sse | (env->mxcsr & 0x00007FBF);
    asm("\tldmxcsr %0" ::"m"(sse));
  }
  return 0;
}

int
__fenv_feupdateenv(fenv_t *env)
{
  unsigned int fcw;
  unsigned int x87[7];
  unsigned int sse;
  unsigned int uexc;

  asm("\tfnstcw %0" : "=m"(fcw) :);
  fcw = fcw & 0xFFFFF0C0;
  fcw = fcw | (env->x87cw & 0x00000F3F);
  asm("\tfldcw %0" ::"m"(fcw));

  asm("\tfnstenv %0" : "=m"(x87[0]) :);
  x87[1] = x87[1] | (env->x87sw & 0x0000003F);
  asm("\tfldenv %0" ::"m"(x87[0]));

  if (IS_SSE_ENABLED) {
    asm("\tstmxcsr %0" : "=m"(sse) :);
    sse = sse & 0xFFFF807F;
    sse = sse | (env->mxcsr & 0x00007FBF);
    asm("\tldmxcsr %0" ::"m"(sse));
  }
  return 0;
}

/** \brief Set (flush to zero) underflow mode
 *
 * \param uflow zero to allow denorm numbers,
 *              non-zero integer to flush to zero
 *
 * \return zero (?)
 */
int
__fenv_fesetzerodenorm(int uflow)
{
  unsigned int sse;
  unsigned int uexc;

  if (IS_SSE2_ENABLED) {
    asm("\tstmxcsr %0" : "=m"(sse) :);
    uexc = (1 << 15) | (1 << 6);
    sse = sse & ~uexc;
    uexc = uflow ? 1 : 0;
    sse = sse | (uexc << 15);
    sse = sse | (uexc << 6);
    asm("\tldmxcsr %0" ::"m"(sse));
  }
  return 0;
}

/** \brief Get (flush to zero) underflow mode
 *
 * \return 1 if flush to zero is set, 0 otherwise
 */
int
__fenv_fegetzerodenorm(void)
{
  unsigned int sse;
  sse = 0;
  if (IS_SSE2_ENABLED) {
    asm("\tstmxcsr %0" : "=m"(sse) :);
    sse = ((sse >> 15) | (sse >> 6)) & 1;
  }
  return sse;
}

/** \brief
 * Mask mxcsr, e.g., a value of 0xffff7fbf says to clear FZ and DAZ
 * (i.e., enable 'full' denorm support).
 *
 * Save the current value of the mxcsr if requested.
 * Note this routine will only be called by the compiler for SSE2 or
 * better targets.
 */
void
__fenv_mask_mxcsr(int mask, int *psv)
{
  int tmp;
  asm("\tstmxcsr %0" : "=m"(tmp) :);
  if (psv)
    *psv = tmp;
  tmp &= mask;
  asm("\tldmxcsr %0" ::"m"(tmp));
  return;
}

/** \brief
 * Restore the current value of the mxcsr.
 */
void
__fenv_restore_mxcsr(int sv)
{
  asm("\tldmxcsr %0" ::"m"(sv));
  return;
}

#else
/*  if defined(TARGET_LINUX_ARM)  */

#include <fenv.h>

int
__fenv_fegetround(void)
{
  return fegetround();
}

int
__fenv_fesetround(int rmode)
{
  return fesetround(rmode);
}

int
__fenv_fegetexceptflag(fexcept_t *flagp, int exc)
{
  return fegetexceptflag(flagp, exc);
}

int
__fenv_fesetexceptflag(fexcept_t *flagp, int exc)
{
  return fesetexceptflag(flagp, exc);
}

int
__fenv_fetestexcept(int exc)
{
  return fetestexcept(exc);
}

int
__fenv_feclearexcept(int exc)
{
  return feclearexcept(exc);
}

int
__fenv_feraiseexcept(int exc)
{
  return feraiseexcept(exc);
}

int
__fenv_feenableexcept(int exc)
{
  return feenableexcept(exc);
}

int
__fenv_fedisableexcept(int exc)
{
  return fedisableexcept(exc);
}

int
__fenv_fegetexcept(void)
{
  return fegetexcept();
}

int
__fenv_fegetenv(fenv_t *env)
{
  return fegetenv(env);
}

int
__fenv_feholdexcept(fenv_t *env)
{
  return feholdexcept(env);
}

int
__fenv_fesetenv(fenv_t *env)
{
  return fesetenv(env);
}

int
__fenv_feupdateenv(fenv_t *env)
{
  return feupdateenv(env);
}

/** \brief Unimplemented: Set (flush to zero) underflow mode
 *
 * \param uflow zero to allow denorm numbers,
 *              non-zero integer to flush to zero
 */
int
__fenv_fesetzerodenorm(int uflow)
{
  return 0;
}

/** \brief Unimplemented: Get (flush to zero) underflow mode
 *
 * \return 1 if flush to zero is set, 0 otherwise
 */
int
__fenv_fegetzerodenorm(void)
{
  return 0;
}

#endif
