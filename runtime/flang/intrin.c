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

int
ftn_i_jishft(int x, int shift)
{
  if (shift < 0) {
    shift = -shift;
    if (shift >= 32)
      return 0;
    return (unsigned)x >> shift;
  } else {
    if (shift >= 32)
      return 0;
    return x << shift;
  }
}

int
ftn_i_shift(int x, int shift)
{
  if (shift < 0) {
    shift = -shift;
    return (unsigned)x >> shift;
  } else {
    return x << shift;
  }
}

float
ftn_i_rmin(float x, float y)
{
  if (x > y)
    return y;
  return x;
}

float
ftn_i_rmax(float x, float y)
{
  if (x < y)
    return y;
  return x;
}

double
ftn_i_dmax(double x, double y)
{
  if (x > y)
    return y;
  return x;
}

double
ftn_i_dmin(double x, double y)
{
  if (x < y)
    return y;
  return x;
}

int
ftn_i_isign(int x, int sign)
{
  if (sign >= 0) {
    if (x > 0)
      return x;
    return -x;
  }
  if (x < 0)
    return x;
  return -x;
}

float
ftn_i_sign(float x, int sign)
{
  if (sign >= 0) {
    if (x > 0.0)
      return x;
    return -x;
  }
  if (x < 0.0)
    return x;
  return -x;
}

double
ftn_i_dsign(double x, double sign)
{
  if (sign >= 0) {
    if (x > 0.0)
      return x;
    return -x;
  }
  if (x < 0.0)
    return x;
  return -x;
}

float
ftn_i_dim(float a, float b)
{
  if (a > b)
    return a - b;
  return 0.0;
}

int
ftn_i_idim(int a, int b)
{
  if (a > b)
    return a - b;
  return 0;
}

double
ftn_i_ddim(double a, double b)
{
  if (a > b)
    return a - b;
  return 0.0;
}
