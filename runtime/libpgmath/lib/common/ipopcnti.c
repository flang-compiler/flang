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

#include "mthdecls.h"

int
__mth_i_ipopcnti(int i, int size)
{
  unsigned ui, uj;

  switch (size) {
  default:
    ui = (unsigned)i;
    ui = (ui & 0x55555555) + (ui >> 1 & 0x55555555);
    ui = (ui & 0x33333333) + (ui >> 2 & 0x33333333);
    ui = (ui & 0x07070707) + (ui >> 4 & 0x07070707);
    ui += ui >> 8;
    ui += ui >> 16;
    ui &= 0x3f;
    break;
  case 2:
    ui = (unsigned)i;
    ui = (ui & 0x5555) + (ui >> 1 & 0x5555);
    ui = (ui & 0x3333) + (ui >> 2 & 0x3333);
    ui = (ui & 0x0707) + (ui >> 4 & 0x0707);
    ui += ui >> 8;
    ui &= 0x1f;
    break;
  case 1:
    ui = (unsigned)i;
    ui = (ui & 0x55) + (ui >> 1 & 0x55);
    ui = (ui & 0x33) + (ui >> 2 & 0x33);
    ui += ui >> 4;
    ui &= 0xf;
    break;
  }
  return ui;
}
