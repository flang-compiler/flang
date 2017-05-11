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

/** \file
  * \brief Implements LIB3F abort subprogram.  */

/* must include ent3f.h AFTER io3f.h */
#include "io3f.h"
#include "ent3f.h"
#include "open_close.h"

void ENT3F(ABORT, abort)()
{
  void *f, *q;

  for (f = GET_FIO_FCBS; f != NULL; f = q) {
    q = FIO_FCB_NEXT(f);
    (void) __fio_close(f, 0 /*dispose = default*/);
  }
  abort();
}
