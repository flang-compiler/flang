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

module int_getter

implicit none

contains

  subroutine foo_int(a)
    integer, intent(out) :: a
    a = 20
  end subroutine foo_int

end module int_getter

program test_function_constructor

use int_getter, only: foo_int

implicit none

abstract interface
   subroutine get_int(a)
     integer, intent(out) :: a
   end subroutine get_int
end interface

type :: foo
   procedure(get_int), nopass, pointer :: get => null()
end type foo

type(foo) :: bar
integer :: x

! This line is valid code, but is rejected.
bar = foo(foo_int)
bar%get => foo_int

call bar%get(x)

if (x .eq. 20) then
    print *, "PASS"
else
    print *, "FAIL"
end if

end program test_function_constructor
