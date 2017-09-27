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
logical expect(10), rslt(10)
type :: objects(l1)
integer(4), len :: l1 = 5
integer :: p(l1)
character(len=l1) :: c
end type
contains
subroutine foo(n)
integer n
type(objects),allocatable :: x

rslt(1) = .not.allocated(x)
allocate(objects::x)
rslt(2) = x%l1 .eq. n
rslt(3) = allocated(x)
rslt(4) = size(x%p) .eq. n

do i=1,n
x%p(i) = i
enddo

rslt(5) = .true.
do i=1,n
if (x%p(i) .ne. i) rslt(5) = .false.
enddo

x%c = "1"
do i=1,n-1
x%c =  trim(x%c) // achar(iachar('1')+i)
enddo

rslt(6) = len_trim(x%c) .eq. n

rslt(7) = .true.
do i=1,n
if (x%c(i:i) .ne. achar(iachar('0')+i)) rslt(7) = .false.
enddo 

end subroutine

end module

program p
use mod
integer y 

rslt = .false.
call foo(5)

expect = .true.
call check(rslt,expect,7)



end
