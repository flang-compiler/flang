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
