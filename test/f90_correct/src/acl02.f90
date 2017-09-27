!
! Copyright (c) 2015, NVIDIA CORPORATION.  All rights reserved.
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

program test_section_constructor

implicit none

type, abstract :: foo_base
end type foo_base

type, extends(foo_base) :: foo
   integer, allocatable :: a(:)
end type foo

type(foo) :: b

integer :: a1(2) = 0
integer :: a2(2,2) = 0

b = foo(a2(:,1)) ! Spurious warning

print *, "PASS"

end program test_section_constructor
