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
logical expect(12)
logical rslt(12)
type :: objects(k1,k2)
integer, kind :: k2
integer, kind :: k1
end type

type,extends(objects) :: stuff(k11,k22)
integer,kind :: k22 = 2
integer,kind :: k11
integer :: st
integer p
integer(k22) :: i
end type

contains
subroutine foo(x)
type(stuff(4,2,3,1)) :: x
rslt(3) = x%i .eq. 99
end subroutine

end module

subroutine foo2(x)
use mod
type(stuff(4,2,3,1)) :: x
rslt(4) = x%i .eq. 99
end subroutine



program p
use mod

interface
subroutine foo2(x)
use mod
type(stuff(4,2,3,1)) :: x
end subroutine
end interface

type(stuff(4,2,3,1)) :: y
type(stuff(4,2,3,1)) :: x

expect = .true.
rslt = .false.

rslt(1) = kind(x%i) .eq. 1

rslt(2) = x%k22 .eq. 1

x%i = 99

y = x

call foo(y)

call foo2(x)

rslt(5) = x%k1 .eq. 4
rslt(6) = x%k2 .eq. 2
rslt(7) = x%k11 .eq. 3
rslt(8) = x%k22 .eq. 1

rslt(9) = y%k1 .eq. 4
rslt(10) = y%k2 .eq. 2
rslt(11) = y%k11 .eq. 3
rslt(12) = y%k22 .eq. 1

call check(rslt,expect,12)


end
