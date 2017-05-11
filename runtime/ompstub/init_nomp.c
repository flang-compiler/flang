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

/*
 * get calling thread's physical number
 * bad name, but it is historical.
 * should be an int, but needs to be a long for 64-bit systems
 */

/* Windows runtime does not have stubbed versions, full MP functionality
 * is available in pgc lib.
 */

#if !defined(WIN64)
/* get max avail cpus */

int
_mp_avlcpus()
{
  return (1);
}

int
_mp_get_maxlevels()
{
  return (1);
}

void
_mp_set_maxlevels(int unused)
{
}

int
_mp_get_tcpus()
{
  return (1);
}

long
_mp_lcpu3()
{
  return (0);
}

/* get current processor number */

/* should be an int, but needs to be a long for 64-bit systems */

long
_mp_lcpu2()
{
  return (0);
}

/* get current number of processors */

/* should be an int, but needs to be a long for 64-bit systems */

long
_mp_ncpus2()
{
  return (1);
}

/* get maximum number of processors */

/* should be an int, but needs to be a long for 64-bit systems */

long
_mp_ncpus3()
{
  return (1);
}

/*
 * get maximum number of processors - special version for profiling.  If
 * mp_init was never called, just return 1.
*/

int
_mp_ncpus3p()
{
  return (1);
}

/* get max cpus */

int
_mp_get_tcpus_max()
{
  return (1);
}

/* get parallel flag */

int
_mp_get_par()
{
  return (0);
}

/* get parpar count */

int
_mp_get_parpar(int n)
{
  return (0);
}

/* return team structure pointer */

struct team*
_mp_pcpu_team(void)
{
    return (0x0);
}

struct team* 
_mp_get_pteam(void * t) 
{
  return (0x0);
}

#endif
