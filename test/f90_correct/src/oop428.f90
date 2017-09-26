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
logical rslt(4),expect(4)
type :: stuff
integer :: kk1=selected_int_kind(2)
integer :: ll1 = 10
character(len=10) :: cc
integer dd
integer:: pp(10)
end type

type :: objects(l1,k1)
integer, kind:: k1=selected_int_kind(2)
integer, len :: l1 = 10
character(len=l1) :: c
integer :: p(l1)
integer d
end type

end module

program ppp
use mod
integer y 
type(objects)::z
!type(objects(l1=10)):: q

rslt(1) = z%l1 .eq. 10
do i=1,z%l1
  z%p(i)=i
enddo

rslt(2) = .true.
do i=1,z%l1
  if (z%p(i) .ne. i) rslt(2) = .false.
enddo


z%c = 'abcdefghij'
rslt(3) = z%c .eq. 'abcdefghij'

rslt(4) = size(z%p) .eq. z%l1

expect = .true.
call check(rslt,expect,4)

end
