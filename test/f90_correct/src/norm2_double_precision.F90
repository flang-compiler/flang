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
  use ISO_FORTRAN_ENV
  implicit none
  integer, parameter :: expec = 1
  integer :: res

  res = norm2Double()
  call check(res, expec, 1)

contains

function norm2Double()
  implicit none
  integer, parameter :: wp = REAL64
  real, parameter :: tolerance = 1.0E-12
  integer, parameter :: n = (Maxexponent(1.0_wp)/2)-1
  real(wp), parameter :: r = Radix(1.0_wp)
  real(wp), parameter :: big = r**n
  real(wp) :: x5(4) = [big, big, big, big]
  real(wp), parameter :: expectDouble = 3.8921198074991259E+307_wp
  real(wp) :: resultDouble
  integer :: norm2Double

  resultDouble = norm2(x5)
  if(abs(resultDouble - expectDouble) < tolerance) then
    norm2Double = 1
    print *, 'Double precision test match'
  else
    norm2Double = -1
    print *, 'Double precision test mismatch'
  end if
end function

end program
