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
logical l 
integer results(5)
integer expect(5)
data expect /.true.,.false.,.false.,.true.,.true./
data results /.false.,.true.,.true.,.false.,.false./
type(square) :: s
type(rectangle) :: r

results(1) = EXTENDS_TYPE_OF(s,r)
results(2) = EXTENDS_TYPE_OF(r,s)
results(3) = SAME_TYPE_AS(s,r)
results(4) = SAME_TYPE_AS(r,s%rectangle)
results(5) = SAME_TYPE_AS(s%rectangle,r)


call check(expect,results,5)

end


