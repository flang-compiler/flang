! Copyright (c) 1990-2018, NVIDIA CORPORATION.  All rights reserved.
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


! Test use-associated (same variable and function names)from two modules n and
! m.

module m
  integer, parameter :: i = -1
  interface
    module subroutine show_i
    end subroutine show_i
  end interface
contains
  integer function times_two (arg)
    integer :: arg
    times_two = -2*arg
  end function
end module m

module n
  integer, parameter :: i = 2
contains
  integer function times_two (arg)
    integer :: arg
    times_two = 2*arg
  end function
end module n

submodule (m) sm
  use n
contains
  module subroutine show_i
    if (i .ne. 2) then
      print *, "FAIL"
    else
      print *, "PASS"
    endif
    if (times_two (i) .ne. 4) then
      print *, "FAIL"
    else
      print *, "PASS"
    endif
  end subroutine show_i
end submodule sm

program p
  use m
  call show_i
  if (i .ne. -1) then
    print *, "FAIL"
  else
    print *, "PASS"
  endif
  if (times_two (i) .ne. 2) then
    print *, "FAIL"
  else
    print *, "PASS"
  endif
end program
