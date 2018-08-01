/*
 * Copyright (c) 1997-2018, NVIDIA CORPORATION.  All rights reserved.
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

typedef struct fio_parm fio_parm;
struct fio_parm {
  char *ab;          /* array base address */
  DECL_HDR_PTRS(ac); /* array descriptor */

  int (*f90io_rw)(int kind, int cnt, int str, char *adr,
                  __CLEN_T len); /* f90io read/write function ptr */

  int (*pario_rw)(int fd, char *adr, int cnt, int str, int typ, int ilen,
                  int own); /* pario read/write function ptr */

  void (*fio_rw)(fio_parm *z); /* fio read/write function ptr */

  __INT_T index[MAXDIMS]; /* first element index */
  int cnt;                /* element count */
  int str;                /* element stride */
  int stat;               /* f90io function return status */
  int tcnt;               /* pario total transfer count */
  int fd;                 /* pario file descriptor */

  repl_t repl; /* replication descriptor */
};

void I8(__fortio_loop)(fio_parm *z, /* parameter struct */
                      int dim);      /* loop dimension */
