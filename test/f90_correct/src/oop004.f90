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

subroutine test_types(s1,s2,r)
use shape_mod
class(shape) s1,s2
integer r(:)
type(square)::s3

r(1) = EXTENDS_TYPE_OF(s1,s2)
r(2) = EXTENDS_TYPE_OF(s2,s1)
r(3) = SAME_TYPE_AS(s1,s3)
r(4) = SAME_TYPE_AS(s3,s1)

end subroutine

program p
USE CHECK_MOD
use shape_mod
interface
subroutine test_types(s1,s2,r)
use shape_mod
class(shape) s1,s2
integer r(:)
end subroutine
end interface
logical l 
integer expect(4)
integer results(4)
data expect /.true.,.false.,.true.,.true./
data results /.false.,.true.,.false.,.false./
type(square) :: s
type(rectangle) :: r

call test_types(s,r,results)

call check(expect,results,4)

end


