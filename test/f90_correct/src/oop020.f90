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

program p
USE CHECK_MOD
use shape_mod

integer results(8)
integer expect(8)
data expect /.true.,.true.,.true.,.false.,.true.,.false.,.false.,.true./
data results /.false.,.false.,.false.,.true.,.false.,.true.,.true.,.false./
type(square),target :: s
type(rectangle),target :: r
type(rectangle) :: r2
type(square) :: s2
class(rectangle),pointer::ptr

ptr=>r

results(1) = SAME_TYPE_AS(ptr,r)
results(2) = SAME_TYPE_AS(r2,ptr)
results(3) = SAME_TYPE_AS(ptr, s%rectangle)
results(4) = SAME_TYPE_AS(s2, ptr)
results(5) = SAME_TYPE_AS(s2%rectangle, ptr)

ptr=>s%rectangle

results(6) = SAME_TYPE_AS(ptr,s)
results(7) = SAME_TYPE_AS(s2,ptr)
results(8) = EXTENDS_TYPE_OF(ptr, s%rectangle)

call check(expect,results,8)

end


