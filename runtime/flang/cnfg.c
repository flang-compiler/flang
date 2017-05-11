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

/* cnfg.c - contains the definitions & routines which control
   configurable behavior */

#include "cnfg.h"
#include <string.h>
#include <stdlib.h>
#include "stdioInterf.h"


extern char *__fort_getenv();

/* WARNING: cnfg.c generates two objects, cnfg.o and pgfcnfg.o
 * define global variable which contains the configurable items:
 *
 * default_name - sprintf string used to construct the default name of a
 *                file upon an open of unit which does not contain a file
 *                specifier and is not a scratch file.
 *
 * true_mask    - mask value used to determine if a value is true or false;
 *                defined so that if the expression (true_mask & val) is
 *                non-zero, the value is true.
 *                    1 => bottom bit is tested (odd => true)
 *                   -1 => non-zero value is true
 *                Default is 1 (VAX-style)
 */

FIO_CNFG __fortio_cnfg_ = {
/* ending '_' so it can be accessed by user */
/* default_name */
    "fort.%d",

    /* vax-style */
    1,  /* odd => true */
    -1, /* internal value of .TRUE. */
};

/* fio access routines */

char *
__get_fio_cnfg_default_name(void)
{
  return __fortio_cnfg_.default_name;
}

int
__get_fio_cnfg_true_mask(void)
{
  return __fortio_cnfg_.true_mask;
}

int *
__get_fio_cnfg_true_mask_addr(void)
{
  return &__fortio_cnfg_.true_mask;
}

int
__get_fio_cnfg_ftn_true(void)
{
  return __fortio_cnfg_.ftn_true;
}

int *
__get_fio_cnfg_ftn_true_addr(void)
{
  return &__fortio_cnfg_.ftn_true;
}

void
__fortio_scratch_name(char *filename, int unit)
/* generate the name of an unnamed scracth file */
{
  extern char *__io_tempnam();
  char *nm;

#if defined(WINNT)
  if (getenv("TMP") == 0)
    nm = __io_tempnam("C:\\", "FTN");
  else
    nm = __io_tempnam((char *)0, "FTN");
  strcpy(filename, nm);
  if (nm)
    free(nm);
#else /*WINNT*/

  nm = __io_tempnam((char *)0, "FTN");
  strcpy(filename, nm);
  if (nm)
    free(nm);

#endif /*WINNT*/

}
