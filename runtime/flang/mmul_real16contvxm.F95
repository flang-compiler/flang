!
! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
!

#include "mmul_dir.h"

subroutine F90_matmul_real16_contvxm(dest, src1, src2, m_extent, n_extent)

  DESC_INT n_extent
  DESC_INT m_extent
  REAL*16, dimension(m_extent) :: src1
  REAL*16, dimension(m_extent,n_extent) :: src2
  REAL*16, dimension(n_extent) :: dest

  REAL*16 rslt
  DESC_INT n
  DESC_INT m

  do n=1,n_extent
    dest(n) = 0.0_16;
    do m=1,m_extent
      dest(n) = dest(n) + src1(m) * src2(m,n)
    end do
 end do

end subroutine
