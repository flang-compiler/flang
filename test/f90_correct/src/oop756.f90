! Copyright (c) 2019, NVIDIA CORPORATION.  All rights reserved.
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

module mod
type :: base(r)
  integer, kind :: r=12
  integer :: z
end type
type, extends(base) :: t
  integer :: i
end type
end module

program p
  use mod
  logical rslt(5),expect(5)
  type(base(20)), allocatable :: b(:)
  type(base(20)), allocatable :: z(:)
  type(base(20)), allocatable :: o(:)

  allocate(o(10))
  allocate(b, z, mold=o)
  
  rslt(1) = same_type_as(z,b)
  rslt(2) = size(b,1) .eq. size(o,1)
  rslt(3) = size(z,1) .eq. size(o,1)
 
  rslt(4) = .true.
  do i=1, size(b,1)
    if (b(i)%r .ne. o(i)%r) then
      rslt(3) = .false.
    endif
  enddo
  rslt(5) = .true.
  do i=1, size(z,1)
    if (z(i)%r .ne. o(i)%r) then
      rslt(5) = .false.
    endif
  enddo

  expect = .true.
  call check(rslt,expect,5)

  end
