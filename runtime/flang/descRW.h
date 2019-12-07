/*
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
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
