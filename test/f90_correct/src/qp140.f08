! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
!
program main
  integer :: r = nint(-2147483650.4567_16, kind = 4), e
  e = -2147483648
  call check(r, e, 1)
end
