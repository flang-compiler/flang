! Copyright (c) 2011-2019, NVIDIA CORPORATION.  All rights reserved.
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
! Same as oop322a.f90 except it has multiple mold= allocations.

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
!USE CHECK_MOD
use shape_mod
logical l 
logical results(9)
logical expect(9)
type(rectangle),allocatable :: r
type(rectangle),allocatable :: r2
real, allocatable :: rr(:), rr2(:)

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

allocate(rr(size(r%sq%r)),rr2,mold=r%sq%r)
rr = r%sq%r
rr2 = r%sq%r

results(4) = all(rr .eq. r%sq%r)
results(9) = all(rr2 .eq. r%sq%r)

allocate(r2,source=r)

results(5) = all(r2%sq%r .eq. rr)

results(6) = all(r2%sq%r .eq. r%sq%r)

do i = 1, size(r2%sq%r)
r2%sq%r(i) = i + 50
rr(i) = i + 50
enddo

results(7) = all(r2%sq%r .eq. r%sq%r)

deallocate(r2)

results(8) = all(r%sq%r .eq. rr)

call check(results,expect,9)

end


