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

module A
implicit none
integer, target :: c
interface
  module subroutine A1(i)
    integer, intent(inout) :: i
  end subroutine A1

  module subroutine sub(ptr)
    integer, pointer :: ptr
  end subroutine
end interface

integer :: incr 

contains
  module subroutine A1(i)
    integer, intent(inout) :: i
    incr = incr + 1
    !print *, incr !<- should print 2
    if (incr .eq. 2) then
      print *, "PASS"
    else
      print *, "FAIL"
    endif
  end subroutine A1

  module procedure sub
    ptr => c
  end procedure
end module

program test
use A
integer :: i = 1
integer, pointer :: toC
incr = 1
call A1(i)
call sub(toC)
end
