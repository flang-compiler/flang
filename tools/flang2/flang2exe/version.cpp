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
 */

#include "gbldefs.h"
#include "release.h"
#include "version.h"

#ifndef LANGUAGE
#define LANGUAGE "F90"
#endif

#define PRODUCT ""

/* COPYRIGHT is extern to make it easy to find in symbol table */
/* it also has extra space to patch in interesting stuff */
char COPYRIGHT[128] =
    "";

VERSION version = {LANGUAGE, VHOST, VSN, BLD, DVSN, TARGET, PRODUCT, COPYRIGHT};

char *
get_version_string()
{
  static char buf[128];
  sprintf(buf, "%s%s", version.vsn, version.bld);
  return buf;
}
