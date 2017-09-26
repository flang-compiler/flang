! Copyright (c) 2010, NVIDIA CORPORATION.  All rights reserved.
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
end type rectangle

type, extends (rectangle) :: square
	integer :: eq
end type square

end module shape_mod

program p
USE CHECK_MOD
use shape_mod
logical l 
logical results(6)
logical expect(6)
class(square),allocatable :: s
class(rectangle),allocatable :: r
class(shape),allocatable :: sh
class(shape),allocatable :: sh2

results = .false.
expect = .true.

allocate(r)
r%the_width = 987
r%color = -1
r%filled = 1
r%x = 100
r%y = 200
allocate(sh, source=r)

select type (sh)
type is (rectangle)
sh%the_length = 777
r%x = 200
class default
end select
sh%y = 300


select type (sh)
type is (rectangle)
results(1) = sh%the_length .eq. 777
results(2) = sh%the_width .eq. 987
results(3) = sh%color .eq. -1
results(4) = sh%x .eq. 100
results(5) = sh%y .eq. 300
results(6) = sh%filled .eq. 1
class is (rectangle)
end select

call check(results,expect,6)

end


