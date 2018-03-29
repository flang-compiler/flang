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

/* Low level (backend) f90 merge intrinsics:
 * These functions are not in the usual hpf/f90 rte directory because
 * their calls are not generated  by the front-end.
 */

int
ftn_i_imerge(int tsource, int fsource, int mask)
{
  if (!mask)
    return fsource;
  return tsource;
}

long long
ftn_i_kmerge(long long tsource, long long fsource, int mask)
{
  if (!mask)
    return fsource;
  return tsource;
}

float
ftn_i_rmerge(float tsource, float fsource, int mask)
{
  if (!mask)
    return fsource;
  return tsource;
}

double
ftn_i_dmerge(double tsource, double fsource, int mask)
{
  if (!mask)
    return fsource;
  return tsource;
}
