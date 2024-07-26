!
! Part of the LLVM Project, under the Apache License v2.0 with LLVM
! Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
!
! Test for USE statement when there is rename of user-defined operator in the
! ONLY option.
! REQUIRES: x86_64-registered-target

program main
  real (kind=8) :: a(128), b(128), c(128)
  integer :: res,expect
  res=1
  expect=2
  do i=1,100
   c(i) = log(a(i)) + log(b(i))
  enddo
  call sub1(a,b,c,128,res)
  call check(res,expect,1);

contains

  subroutine sub1(a,b,c,N,res)
   real(kind=8), intent(in) :: a(:), b(:)
   real(kind=8), intent (out) :: c(:)
   integer, intent(IN) :: N
   integer, intent(OUT) :: res

   do i=1,N,16
     c(i) = exp(log(a(i))) + dlog(b(i))
   enddo
   res=2
  end subroutine

end program

