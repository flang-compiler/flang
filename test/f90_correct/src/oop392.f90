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
logical expect(5), rslt(5)
type :: objects(k1,k2)
integer, kind :: k2 = 2
integer, kind :: k1 = 4
end type

type,extends(objects) :: stuff(k11,k22)
integer,kind :: k22 = 2
integer,kind :: k11 = 3
integer :: st
integer p
integer(k22) :: i
end type

contains
subroutine foo(x)
type(stuff(4,2,3,2)) :: x
print *, x
end subroutine

end module

subroutine foo2(x)
use mod
type(stuff(4,2,3,2)) :: x
print *, x
end subroutine



program p
use mod

interface
subroutine foo2(x)
use mod
type(stuff(4,2,3,2)) :: x
end subroutine
end interface

type(stuff(4,2,3,2)) :: x
type(stuff)::y(10)
type(stuff)::z

z = x

rslt(1) = kind(z%i) .eq. 2
rslt(2) = z%k22 .eq. 2
rslt(3) = z%k22 .eq. 2
rslt(4) = x%k22 .eq. 2

rslt(5) = .true.
do i = 1, 10
z = y(i)
if (kind(z%i) .ne. 2) rslt(5) = .false.
enddo

expect=.true.
call check(rslt,expect,5)

end
