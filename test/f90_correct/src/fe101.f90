!
! Copyright (c) 2015, NVIDIA CORPORATION.  All rights reserved.
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
!

program character_minmax
  parameter(N=11) 
  character(len=*), parameter :: s = MIN ("a", "0", "C")
  character(len=*), parameter :: t = MAX ("a", "0", "C")
  character(len=*), parameter :: u(3) = (/"a", "0", "C"/)
  character*2, parameter ::  kkmin(2) = min((/"za","bb"/), (/"jj","dd"/))
  character*2, parameter ::  kkmax(2) = max((/"za","bb"/), (/"jj","dd"/))
!  character*2, parameter ::  kkmin(2) = (/"za","bb"/)
!  character*2, parameter ::  kkmax(2) = (/"jj","dd"/)
  character*2 tt
  integer result(N),expect(N)

  real,    allocatable :: a(:)
  logical, allocatable :: m(:)
  expect = 1
  result = 1

  if (s /= "0") then
     result(1) = 0
  end if
  if (t /= "a") then
     result(2) = 0
  end if
  if (MINVAL (u) /= s) then
     result(3) = 0
  end if
  if (MAXVAL (u) /= t) then
     result(4) = 0
  end if
  if (MINLOC (u,1) /= 2) then
     result(5) = 0
  end if
  if (MAXLOC (u,1) /= 1) then
     result(6) = 0
  end if
!  if (kkmin(1) /= "jj") then
!     result(7) = 0
!  end if

  tt = kkmin(1)
  if (tt /= "jj") then
     result(7) = 0
  end if
  tt = kkmin(2)
  if (tt /= "bb") then
     result(8) = 0
  end if

   tt = kkmax(1)
  if (tt /= "za") then
     result(9) = 0
  end if

   tt = kkmax(2)
  if (tt /= "dd") then
     result(10) = 0
  end if

  allocate (a(0), m(0))

  if (any ( (/ &
       minloc ( a ), &
       minloc ( a, mask=m ), &
       minloc ( (/ 1   /), mask=(/ .false. /) ), &
       minloc ( (/ 1.0 /), mask=(/ .false. /) ), &
       minloc ( (/ "A" /), mask=(/ .false. /) ), &
       maxloc ( (/ 1   /), mask=(/ .false. /) )  &
       /) /= 0)) then
     result(11) = 0
  end if

  call check(result,expect,N)

end 
