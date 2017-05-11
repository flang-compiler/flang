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

/* dummy libnuma.a routines */

int
numa_available()
{
  return (-1);
}

void
nodemask_zero()
{
}

void
nodemask_set()
{
}

void
numa_set_membind()
{
}

int
numa_max_node()
{
  return 1;
}

void
numa_alloc_local()
{
}

void
numa_set_preferred()
{
}

void
numa_set_localalloc()
{
}

int
set_mempolicy()
{
  return (0);
}

int
get_mempolicy()
{
  return (0);
}

int
mbind()
{
  return (0);
}
