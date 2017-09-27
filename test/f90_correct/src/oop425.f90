! Copyright (c) 2012, NVIDIA CORPORATION.  All rights reserved.
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
logical rslt(10),expect(10)
type :: objects(l1)
integer, len :: l1 = 5
integer :: p(l1)
character(len=l1) :: c
end type
contains
subroutine foo(x,n)
integer n
type(objects(10)):: x

rslt(5) = x%l1 .eq. 10

rslt(6) = .true.
do i=1, x%l1
if (x%p(i) .ne. i) rslt(6) = .false.
enddo

rslt(7) = size(x%p) .eq. x%l1

rslt(8) = x%c .eq. 'abcdefghij'

rslt(9) = len(x%c) .eq. x%l1

rslt(10) = len_trim(x%c) .eq. n

end subroutine

end module

program p
use mod
integer y 
type(objects(9+1))::z

expect = .true.
rslt = .false.

rslt(1) = z%l1 .eq. 10

do i=1, z%l1
z%p(i) = i
enddo

rslt(2) = .true.
do i=1, z%l1
if (z%p(i) .ne. i) rslt(2) = .false.
enddo

rslt(3) = size(z%p) .eq. z%l1

z%c = 'abcdefghij'

rslt(4) = z%c .eq. 'abcdefghij'

call foo(z,10)

call check(rslt,expect,10)

end
