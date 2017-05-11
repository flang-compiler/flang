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

/**
 * \file
 * \brief cnfg.h -  declare external variables/arrays used by Fortran I/O
 */

/* declare structure which may alter the configuration */
typedef struct {
  char *default_name; /* sprintf string for default file name */
  int true_mask;      /* 1 => odd is true, -1 => nonzero is true */
  int ftn_true;       /* -1 ==> VAX; 1 => unix */
} FIO_CNFG;

#ifdef WINNT

extern char *__get_fio_cnfg_default_name(void);
extern int __get_fio_cnfg_true_mask(void);
extern int *__get_fio_cnfg_true_mask_addr(void);
extern int __get_fio_cnfg_ftn_true(void);
extern int *__get_fio_cnfg_ftn_true_addr(void);

#define GET_FIO_CNFG_DEFAULT_NAME __get_fio_cnfg_default_name()
#define GET_FIO_CNFG_TRUE_MASK __get_fio_cnfg_true_mask()
#define GET_FIO_CNFG_TRUE_MASK_ADDR __get_fio_cnfg_true_mask_addr()
#define GET_FIO_CNFG_FTN_TRUE __get_fio_cnfg_ftn_true()
#define GET_FIO_CNFG_FTN_TRUE_ADDR __get_fio_cnfg_ftn_true_addr()

#else

extern FIO_CNFG __fortio_cnfg_; /* ending '_' so it's accessible by the
                                * fortran programmer */

#define GET_FIO_CNFG_DEFAULT_NAME __fortio_cnfg_.default_name
#define GET_FIO_CNFG_TRUE_MASK __fortio_cnfg_.true_mask
#define GET_FIO_CNFG_TRUE_MASK_ADDR &__fortio_cnfg_.true_mask
#define GET_FIO_CNFG_FTN_TRUE __fortio_cnfg_.ftn_true
#define GET_FIO_CNFG_FTN_TRUE_ADDR &__fortio_cnfg_.ftn_true

#endif

extern void __fortio_scratch_name(char *, int);
