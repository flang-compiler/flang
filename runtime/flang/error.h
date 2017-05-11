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
 * \brief Initialization and error handling functions for Fortran I/O
 */

void set_gbl_newunit(bool newunit);
bool get_gbl_newunit();
void __fortio_errinit(__INT_T unit, __INT_T bitv, __INT_T *iostat, char *str);
void __fortio_errinit03(__INT_T unit, __INT_T bitv, __INT_T *iostat, char *str);
void __fortio_errend03();
void __fortio_fmtinit();
void __fortio_fmtend(void);
int __fortio_error(int errval);
char * __fortio_errmsg(int errval);
int __fortio_eoferr(int errval);
int __fortio_eorerr(int errval);
int __fortio_check_format(void);
int __fortio_eor_crlf(void);

int __fortio_no_minus_zero(void);

