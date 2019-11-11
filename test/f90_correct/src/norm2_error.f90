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


subroutine s1 ! error
  implicit none
  integer :: x(10)
  real :: results
  integer :: i

  do i=1,10
    x(i) = i
  enddo
  !{error "PGF90-S-0074-Illegal number or type of arguments to norm2 - keyword argument x"}
  results = norm2(x)
end

subroutine s2 ! error
  implicit none
  real :: y(10)
  real :: results
  integer :: i
  do i=1,10
    y(i)=i
  end do
  !{error "PGF90-S-0423-Constant DIM= argument is out of range"}
  results = norm2(y, 2)
end

subroutine s3
  implicit none
  real :: z(10)
  real :: results
  integer :: i
  do i=1,10
    z(i)=i
  end do
  !{error "PGF90-S-0074-Illegal number or type of arguments to norm2 - keyword argument position 3"}
  results = norm2(z, 1, 0)
end

subroutine s4
  implicit none
  real :: z(10)
  real :: results
  integer :: i
  do i=1,10
    z(i)=i
  end do
  !{error "PGF90-S-0074-Illegal number or type of arguments to norm2 - 0 argument(s) present, 1-3 argument(s) expected"}
  results = norm2()
end

subroutine s5
  implicit none
  !{error "PGF90-S-0155-Intrinsic not supported in initialization: norm2"}
  real :: results = norm2([real :: 2, 3, 4, 5])
end

  call s1
  call s2
  call s3
  call s4
  call s4
  call s5
end
