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
	type(square),pointer :: sq => null()
end type rectangle

type, extends (rectangle) :: square
real, allocatable :: r(:)
end type square

end module shape_mod

program p
USE CHECK_MOD
use shape_mod
logical l 
logical results(7)
logical expect(7)
class(rectangle),allocatable :: r
class(rectangle),allocatable :: r2
real, allocatable :: rr(:)

expect = .true.
results = .false.

results(1) = .not. allocated(r)
allocate(r)
results(2) = allocated(r)
results(3) = .not. associated(r%sq)

allocate(r%sq)
allocate(r%sq%r(10))

do i = 1, size(r%sq%r)
r%sq%r(i) = i
enddo

allocate(rr(size(r%sq%r)),source=r%sq%r)

results(4) = all(rr .eq. r%sq%r)

allocate(r2,source=r)

results(5) = all(r2%sq%r .eq. rr)

results(6) = all(r2%sq%r .eq. r%sq%r)

do i = 1, size(r2%sq%r)
r2%sq%r(i) = i + 50
enddo

results(7) = all(r2%sq%r .eq. r%sq%r)

call check(results,expect,7)

end


