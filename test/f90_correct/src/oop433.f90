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

program p
implicit none
type :: objects(l1)
integer, len :: l1 = 10
character(len=l1) :: c
integer  p(l1)
end type

integer i
logical rslt(4), expect(4)

type(objects(5)) :: x

expect = .true.
rslt(1) = size(x%p) .eq. 5

do i=1,x%l1
  x%p(i) = i
enddo

rslt(2) = .true.
do i=1,x%l1
  if (x%p(i) .ne. i) rslt(2) = .false.
enddo

x%c = 'abcd'

rslt(3) = len(x%c) .eq. x%l1
rslt(4) = len_trim(x%c) .eq. len('abcd')

call check(rslt,expect,4)

end



