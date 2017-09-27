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

module tmod

type my_type
integer :: x(10)
end type my_type

end module

subroutine sub (x,r,y)
use tmod
type(my_type), allocatable::x
integer r(:)
class(my_type) :: y

r(1) = extends_type_of(x,y)
r(2) = allocated(x)
r(3) = same_type_as(x,y)

end subroutine


program p
USE CHECK_MOD
use tmod

interface
subroutine sub(w,s,z)
use tmod
type(my_type),allocatable::w
integer s(:)
class(my_type) :: z
end subroutine
end interface 

type(my_type),allocatable::x
type(my_type) :: y
integer results(3)
integer expect(3)
data expect /.true.,.true.,.true./
data results /.false.,.false.,.false./


allocate(x)
call sub(x,results,y)
call check(results,expect,3)

end
