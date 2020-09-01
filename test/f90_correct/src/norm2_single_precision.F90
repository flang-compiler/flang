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

  res = norm2Single()
  call check(res, expec, 1)

contains

function norm2Single()
  implicit none
  integer, parameter :: wp = REAL32
  real, parameter :: tolerance = 1.0E-12
  integer, parameter :: n = Maxexponent(1.0_wp) - 3
  real(wp), parameter :: r = Radix(1.0_wp)
  real(wp), parameter :: big = r**n
  real(wp) :: x5(3) = [big, big, big]
  real(wp) resultSingle
  real(wp), parameter :: expectSingle =  7.36732922E+37_wp
  integer :: norm2Single

  resultSingle = norm2(x5)

  if(abs(resultSingle - expectSingle) < tolerance) then
    norm2Single = 1
    print *, 'Single precision test  match'
  else
    norm2Single = -1
    print *, 'Single precision test  mismatch'
  end if
end function
end program
