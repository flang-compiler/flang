!
! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
!

! test constant propagation optimization

program test
real(8) :: weight
real(8), allocatable, dimension(:) :: a

allocate(a(8))

weight = 1._8
a = weight + a

write(*, *) 'PASS'

end program test
