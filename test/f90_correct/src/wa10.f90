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

! Initializing a named constant with an implied DO expression
! of non-integer type

program test2
implicit none

integer :: i
integer, parameter :: nn=12

real(8), dimension(nn), parameter :: result = (/(3.0*i, i=1,nn)/)
real(8), dimension(nn) :: expect = (/3.0,6.0,9.0,12.0,15.0,18.0,21.0,24.0,27.0,30.0,33.0,36.0/)

call check(result, expect, nn)

end program test2

