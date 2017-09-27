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
end type square

end module shape_mod

subroutine sub2(p1,results,p2)
use shape_mod
class(shape),pointer::p1
class(shape),pointer::p2
integer results(:)
type(shape) sh
type(rectangle) r
type(square) s
integer st

results(1) = SAME_TYPE_AS(p1,p2)
results(2) = SAME_TYPE_AS(p1,s)
results(3) = SAME_TYPE_AS(p1,r)
results(4) = SAME_TYPE_AS(p1,sh)
results(5) = EXTENDS_TYPE_OF(p1,p2)

end subroutine

program p
USE CHECK_MOD
use shape_mod

interface
subroutine sub2(p1,results,p2)
use shape_mod
class(shape),pointer::p1
class(shape),pointer::p2
integer results(:)
end subroutine
end interface

integer results(7)
integer expect(7)
type(square),target,allocatable :: s
type(rectangle),target,allocatable :: r
class(shape),pointer::ptr
class(shape),pointer::ptr2

data expect  /.false.,.true.,.false.,.false.,.true.,.false.,.false./
data results /.true.,.false.,.true.,.true.,.false.,.true.,.true./

allocate(s)
allocate(r)
ptr => s
ptr2 => r
call sub2(ptr,results,ptr2)
deallocate(s)
deallocate(r)
results(6) = allocated(s)
results(7) = allocated(r)
call check(expect,results,7)

end


