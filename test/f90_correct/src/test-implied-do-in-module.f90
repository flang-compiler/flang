! Part of the LLVM Project, under the Apache License v2.0 with LLVM
! Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
!
! Test the implied DO expression in module.
!

module mod_impliedDo
  integer :: i
  character(*), parameter :: a = "Hello world!"
  character(*), parameter :: b1(2) = [(a(i:i+4),i=1,7,6)]
end module

program p
  use mod_impliedDo
  character(5) :: c(2)
  c(1) = b1(1)
  c(2) = b1(2)
  call check(c, "Helloworld", 1)
end program
