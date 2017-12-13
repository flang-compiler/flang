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
type :: stuff(k11,k22)
integer,len :: k22 = 20
integer,kind :: k11 = 4
integer(k11) :: i = 3
integer(k11) :: j=4
integer :: p(k22)
end type
contains
subroutine foo(y,n)
integer n
type(stuff(2,n)) :: y

do i=1,y%k22
  y%p(i) = i
enddo


rslt(7) = y%i .eq. 3
rslt(8) = kind(y%i) .eq. 2
rslt(9) = y%k11 .eq. 2
rslt(10) = y%k22 .eq. size(y%p)
rslt(11) = y%j .eq. 4

rslt(12) = .true.
do i=1,y%k22
  if (y%p(i) .ne. i) rslt(12) = .false.
enddo
end subroutine


end module

program p
use mod
integer x

type(stuff(2,10)) :: y

do i=1,y%k22
 y%p(i) = i
enddo

rslt(1) = y%i .eq. 3
rslt(2) = kind(y%i) .eq. 2
rslt(3) = y%k11 .eq. 2 
rslt(4) = y%k22 .eq. 10
rslt(5) = y%j .eq. 4

rslt(6) = .true.
do i=1,y%k22
  if (y%p(i) .ne. i) rslt(6) = .false.
enddo

call foo(y,10)

expect = .true.
call check(rslt,expect,12)

end
