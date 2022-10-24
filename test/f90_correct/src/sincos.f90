! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
!

program foo
      real(kind=8) :: val, res1, res2
      read *, val
      res1 = sin(val)
      res2 = cos(val)
      print *, res1, res2
      print *, "PASSED" ! Check that this compiles without error.
end program foo
