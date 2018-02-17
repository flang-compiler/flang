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

! Compile with -Mallocatable=03 to exercise the bug fix (disregard if 
! -Mallocatable=03 is now the default behavior in the compiler).

module mod
  integer count
  type base
     integer :: x
  end type base
  
  type comp
     class(base), allocatable :: b
   contains
     final :: dtor
  end type comp
  
contains
  
  subroutine dtor(this)
    type(comp) :: this
    count = count + 1
  end subroutine dtor
  
  subroutine foo()
    type(comp) :: x, y
    
    allocate(y%b)
    y%b%x = 99
    
    ! final subroutine, dtor, gets called 3 times (called for x,y, and  
    ! the temp used in the x = y assignment below).
    
    x = y
  end subroutine foo
  
end module mod

use mod
logical rslt(1), expect(1)
count = 0
call foo()

expect = .true.
rslt(1) = count .eq. 3
call check(rslt, expect, 1)
end
