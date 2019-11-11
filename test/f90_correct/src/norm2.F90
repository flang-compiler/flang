! Copyright (c) 2019, Arm Ltd.  All rights reserved.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

program test

implicit none

  real, parameter :: tolerance = 1.E-12
  integer, parameter :: expec (2) = [1, 1]
  integer :: res(2)
  integer :: sz, i
  real :: x1(5) = [real::1, 2, 3, 4, 5]
  real :: x2(6) = [real:: 1, 2, 3, 4, 5, 999999999]
  real, allocatable :: x3(:)
  real, dimension (-2:2) :: x4
  real :: matrix2(3,3) = reshape((/1,2,3,1,2,3,1,2,3/), (/3,3/))
  real :: resultsA(10)
  real :: resultsB(10)
  real :: resultsC

  ! Tests array constructor as argument
  resultsA(1) = norm2(x1)
  resultsB(1) = norm2(x2(1:5))
  resultsC = norm2([real:: 1, 2, 3, 4, 5])

  ! Tests allocatable array as argument
  sz = 3
  allocate (x3(sz))
  do i=1, sz
    x3(i) = i*i
  enddo
  resultsA(2) = norm2(x3)
  resultsB(2) = sqrt(dot_product(x3, x3))

  ! Test dim
  resultsA(3:5) = norm2(matrix2, 1)
  resultsB(3) = sqrt(dot_product(matrix2(:,1), matrix2(:,1)))
  resultsB(4) = sqrt(dot_product(matrix2(:,2), matrix2(:,2)))
  resultsB(5) = sqrt(dot_product(matrix2(:,3), matrix2(:,3)))
  resultsA(6:8) = norm2(matrix2, 2)
  resultsB(6) = sqrt(dot_product(matrix2(1,:), matrix2(1,:)))
  resultsB(7) = sqrt(dot_product(matrix2(2,:), matrix2(2,:)))
  resultsB(8) = sqrt(dot_product(matrix2(3,:), matrix2(3,:)))

  x4 = [real :: -1, -2, 0, 10, 100]
  ! Test Adjustable Arrays
  resultsA(9) = norm2(x4,1)
  resultsB(9) = sqrt(dot_product(x4, x4))

  ! Test assumed shape
  resultsA(10) = norm2(x4(1:2))
  resultsB(10) = sqrt(dot_product(x4(1:2), x4(1:2)))

  if (abs(resultsA(1) - resultsB(1)) < tolerance) then
    if (abs(resultsB(1) - resultsC) < tolerance) then
       res(1) = 1
       print *, 'arrays tests match'
    else
      res(1) = -1
      print *, 'arrays tests mismatch'
    endif
  else
    res(1) = -2
    print *, 'arrays tests mismatch'
  endif

  if(all(abs(resultsA(2:10)-resultsB(2:10)) < tolerance)) then
    res(2) = 1
    print *, 'expect vs results match'
  else
    res(2) = -1
    print *, 'expect vs results mismatch'
  end if

  call check(res, expec, 2)
end program
