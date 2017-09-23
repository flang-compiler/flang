! Copyright (c) 1990-2017, NVIDIA CORPORATION.  All rights reserved.
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

! f90 example causes false severe error 'Illegal implied DO expression'

module test
implicit none

integer, parameter :: nbands = 16

integer :: i

real, dimension(nbands), parameter :: &
wavenum1 = (/ 10,250,500,630,700,820,980,1080,1180,1390,1480,1800,2080,2250,2380,2600 /)
real, dimension(nbands), parameter :: wavenum2 = (/ (wavenum1(i), i = 2, nbands), 3000. /) ! Illegal?
real, dimension(nbands), parameter :: delwave = wavenum2 - wavenum1
end module test

program testp
use test

real*8, dimension(nbands*3) :: results
real*8, dimension(nbands*3) :: expect = &
 (/ 10,250,500,630,700,820,980,1080,1180,1390,1480,1800,2080,2250,2380,2600, &
    250,500,630,700,820,980,1080,1180,1390,1480,1800,2080,2250,2380,2600,3000, &
    240,250,130,70,120,160,100,100,210,90,320,280,170,130,220,400 /)
!print *, wavenum1
!print *, ""
!print *, wavenum2
!print *, ""
!print *, delwave
do i = 1,nbands
  results(i) = wavenum1(i)
  results(i+nbands) = wavenum2(i)
  results(i+nbands*2) = delwave(i)
end do
call checkd(results, expect, nbands*3)
end program testp
