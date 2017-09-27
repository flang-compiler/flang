! Copyright (c) 2011, NVIDIA CORPORATION.  All rights reserved.
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

module shape_mod

type shape
        integer :: color
        logical :: filled
        integer :: x
        integer :: y
end type shape

type, EXTENDS ( shape ) :: rectangle
        integer :: the_length
        integer :: the_width
	class(shape),allocatable :: sq
end type rectangle

type, extends (rectangle) :: square
real, allocatable :: r(:)
end type square

end module shape_mod

program p
USE CHECK_MOD
use shape_mod
logical l 
logical results(8)
logical expect(8)
class(rectangle),allocatable :: r
class(rectangle),allocatable :: r2
real, allocatable :: rr(:)

expect = .true.
results = .false.

results(1) = .not. allocated(r)
allocate(r)
results(2) = allocated(r)
results(3) = .not. allocated(r%sq)

allocate(square::r%sq)
select type(o=>r%sq)
class is (square)
allocate(o%r(10))

do i = 1, size(o%r)
o%r(i) = i
enddo

allocate(rr(size(o%r)),source=o%r)

results(4) = all(rr .eq. o%r)

allocate(r2,source=r)
select type(o2=>r2%sq)
class is (square)
results(5) = all(o2%r .eq. rr)

results(6) = all(o2%r .eq. o%r)

do i = 1, size(o2%r)
o2%r(i) = i + 50
rr(i) = i + 50
enddo

results(7) = .not. all(o2%r .eq. o%r)

!deallocate(r2)

results(8) = .not. all(o%r .eq. rr)
end select
end select

call check(results,expect,8)

end


