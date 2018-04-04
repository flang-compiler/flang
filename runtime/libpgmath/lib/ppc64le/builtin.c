/*
 * Copyright (c) 2015-2017, NVIDIA CORPORATION.  All rights reserved.
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

/* Note: the front end recognizes the __builtin_ functions as
 * special cases and produces the appropriate ILM - thus there
 * is no infinite recursion here.
 * These functions are only needed as a work-around for c++ needing
 * function pointers to the corresponding libm functions.
 */

/* inhibit floating point copy propagation */
#pragma global - Mx, 6, 0x100

int
__builtin_abs(int a)
{
  return __builtin_abs(a);
}

float
__builtin_fabsf(float a)
{
  float d = __builtin_fabsf(a);
  return d;
}

double
__builtin_fabs(double a)
{
  double d = __builtin_fabs(a);
  return d;
}

float
__builtin_sqrtf(float a)
{
  float f = __builtin_sqrtf(a);
  return f;
}

double
__builtin_sqrt(double a)
{
  double d = __builtin_sqrt(a);
  return d;
}

float
__builtin_sinf(float a)
{
  float f = __builtin_sinf(a);
  return f;
}

double
__builtin_sin(double a)
{
  double d = __builtin_sin(a);
  return d;
}

float
__builtin_cosf(float a)
{
  float f = __builtin_cosf(a);
  return f;
}

double
__builtin_cos(double a)
{
  double d = __builtin_cos(a);
  return d;
}

float
__builtin_tanf(float a)
{
  float f = __builtin_tanf(a);
  return f;
}

double
__builtin_tan(double a)
{
  double d = __builtin_tan(a);
  return d;
}

float
__builtin_truncf(float a)
{
  float f = __builtin_truncf(a);
  return f;
}

double
__builtin_trunc(double a)
{
  double d = __builtin_trunc(a);
  return d;
}

float
__builtin_acosf(float a)
{
  float f = __builtin_acosf(a);
  return f;
}

double
__builtin_acos(double a)
{
  double d = __builtin_acos(a);
  return d;
}

float
__builtin_asinf(float a)
{
  float f = __builtin_asinf(a);
  return f;
}

double
__builtin_asin(double a)
{
  double d = __builtin_asin(a);
  return d;
}

float
__builtin_atan2f(float a, float b)
{
  float f = __builtin_atan2f(a, b);
  return f;
}

double
__builtin_atan2(double a, double b)
{
  double d = __builtin_atan2(a, b);
  return d;
}

float
__builtin_atanf(float a)
{
  float f = __builtin_atanf(a);
  return f;
}

double
__builtin_atan(double a)
{
  double d = __builtin_atan(a);
  return d;
}

float
__builtin_expf(float a)
{
  float f = __builtin_expf(a);
  return f;
}

double
__builtin_exp(double a)
{
  double d = __builtin_exp(a);
  return d;
}

float
__builtin_logf(float a)
{
  float f = __builtin_logf(a);
  return f;
}

double
__builtin_log(double a)
{
  double d = __builtin_log(a);
  return d;
}

float
__builtin_log10f(float a)
{
  float f = __builtin_log10f(a);
  return f;
}

double
__builtin_log10(double a)
{
  double d = __builtin_log10(a);
  return d;
}

double
__builtin_pow(double a, double b)
{
  double d = __builtin_pow(a, b);
  return d;
}

float
__builtin_crealf(float _Complex a)
{
  float d = __builtin_crealf(a);
  return d;
}

float
__builtin_cimagf(float _Complex a)
{
  float d = __builtin_cimagf(a);
  return d;
}

float _Complex __builtin_conjf(float _Complex a)
{
  float _Complex d = __builtin_conjf(a);
  return d;
}

double
__builtin_creal(double _Complex a)
{
  double d = __builtin_creal(a);
  return d;
}

double
__builtin_cimag(double _Complex a)
{
  double d = __builtin_cimag(a);
  return d;
}

double _Complex __builtin_conj(double _Complex a)
{
  double _Complex d = __builtin_conj(a);
  return d;
}
