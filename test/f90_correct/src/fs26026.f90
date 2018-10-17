! Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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

module m
implicit none
integer :: i = 0
contains
  subroutine p()
    i = 1
  end subroutine p

  subroutine foo(fun_ptr)
    procedure(p), pointer, intent(out) :: fun_ptr
    fun_ptr => p
  end subroutine
end module m

program test
use m
implicit none
procedure(), pointer :: x
call foo(x)
call x()
if (i == 1) then
  write(*,*) 'PASS'
else
  write(*,*) 'FAIL'
  STOP 1
end if
end program test

