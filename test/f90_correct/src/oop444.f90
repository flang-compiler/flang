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
logical expect(12),rslt(12)

type :: objects(k1,l1)
integer, kind:: k1=selected_int_kind(2)
integer, len :: l1 = 10
integer:: p(l1)
character(len=l1) :: c
integer(k1):: d
end type
contains
subroutine foo(x,n)
integer n
type(objects(l1=10)):: x

rslt(5) = x%c .eq. 'abcdefghij'
rslt(6) = x%l1 .eq. n 
rslt(7) = size(x%p) .eq. x%l1

rslt(8) = .true.
do i=1,size(x%p)
  if (x%p(i) .ne. i) rslt(8) = .false.
enddo

end subroutine

subroutine foo2(x,n)
integer n
type(objects):: x
!print *, x%c, len(x%c)
rslt(9) = x%c .eq. 'abcdefghij'
rslt(10) = x%l1 .eq. n
rslt(11) = size(x%p) .eq. x%l1

rslt(12) = .true.
do i=1,size(x%p)
  if (x%p(i) .ne. i) rslt(12) = .false.
enddo

end subroutine


end module

program ppp
use mod
integer y 
type(objects(l1=10))::z
type(objects(l1=10)):: q

do i=1, size(z%p)
z%p(i) = i
enddo


z%c = 'abcdefghij'
!print *, z%c
rslt(1) = z%c .eq. 'abcdefghij'
rslt(2) = z%l1 .eq. 10
rslt(3) = size(z%p) .eq. z%l1

rslt(4) = .true.
do i=1,size(z%p)
  if (z%p(i) .ne. i) rslt(4) = .false.
enddo

q = z

call foo(q,10)
call foo2(z,10)

expect = .true.
call check(rslt,expect,12)

end
