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

/* clang-format off */

/*
 * Define the support of an i/o statement to be a critical section:
 *    ENTF90IO(BEGIN, begin)() - marks the begin of an i/o statement
 *    ENTF90IO(END, end)()   - marks the end of an i/o statement
 * Routines are called by the generated code if the option -x 125 1 is
 * selected.  Functions to be completed by users who need this facility.
 */

#include "fioMacros.h"

void ENTF90IO(BEGIN, begin)() {}

void ENTF90IO(END, end)() {}

void ENTCRF90IO(BEGIN, begin)() {}

void ENTCRF90IO(END, end)() {}
