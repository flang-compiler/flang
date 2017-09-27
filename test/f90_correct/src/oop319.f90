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
end type square

end module shape_mod

program p
USE CHECK_MOD
use shape_mod
logical l 
logical results(5)
logical expect(5)
class(rectangle),allocatable :: r

expect = .true.
results = .false.

results(1) = .not. allocated(r)
allocate(r)
results(2) = allocated(r)
results(3) = .not. associated(r%sq)
allocate(r%sq)
results(4) = associated(r%sq)
deallocate(r%sq)
deallocate(r)
results(5) = .not. allocated(r)

call check(results,expect,5)

end


