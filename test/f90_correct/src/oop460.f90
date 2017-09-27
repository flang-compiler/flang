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
type :: objects(l1,k1,l2)
integer , kind :: k1=4
integer(4), len :: l1 = 10
integer, len :: l2 = 20
integer z(l1)
end type
end module

program p
use mod
logical rslt(23)
logical expect(23)

type(objects(:)),allocatable :: x

allocate( objects(20) :: x )

expect = .true.

rslt(1) = allocated(x)
rslt(2) = x%l1 .eq. 20
rslt(3) = size(x%z,dim=1) .eq. 20 
do i=1, size(x%z)
  x%z(i) = i
enddo
do i=1, size(x%z)
  rslt(i+3) = x%z(i) .eq. i
enddo

call check(rslt,expect,23)


end
