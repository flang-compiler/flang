! Copyright (c) 2012-2017, NVIDIA CORPORATION.  All rights reserved.
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

module mod
logical expect(8), rslt(8)
type :: objects(k1,k2)
integer, kind :: k2 = selected_char_kind("ASCII")
integer, kind :: k1 = selected_int_kind(4) + 6
end type

type,extends(objects) :: stuff(k11,k22)
integer,kind :: k22 = selected_real_kind(1)
integer,kind :: k11 = 3
integer :: st
integer(k1) :: i
character(kind=k2) :: c
real(k22) :: j
integer d
integer p(k1+10)
end type
end module

program p
use mod

type(stuff(k22=4)) :: x

x%i = x%k22

rslt(1) = x%k1 .eq. 8
rslt(2) = x%k22 .eq. 4
rslt(3) = kind(x%i) .eq. 8
rslt(4) = kind(x%j) .eq. 4
rslt(5) = kind(x%c) .eq. 1
rslt(6) = size(x%p) .eq. 18
rslt(7) = x%k2 .eq. 1

do i=1,size(x%p)
x%p(i) = i
enddo

rslt(8) = .true.
do i=1,size(x%p)
if (x%p(i) .ne. i) rslt(8) = .false.
enddo

expect = .true.
call check(rslt,expect,8)

end
