!
! Copyright (c) 1999, NVIDIA CORPORATION.  All rights reserved.
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
! from iso_varying_string test, make sure allocatables and pointers
! deallocated in contained subprograms work

program p

integer,dimension(:),allocatable :: ar
integer,dimension(:),pointer :: br

integer,dimension(6) :: result,expect
data expect/2,5,40,2,5,40/

allocate(ar(1:10))
allocate(br(1:10))
ar = 5
br = 5
call sub()

result(1) = lbound(ar,1)
result(2) = ubound(ar,1)
result(3) = sum(ar)
result(4) = lbound(br,1)
result(5) = ubound(br,1)
result(6) = sum(br)

call check(result,expect,6)

contains

subroutine sub

 deallocate(ar)
 allocate(ar(2:5))
 ar = 10
 deallocate(br)
 allocate(br(2:5))
 br = 10
end subroutine
end
