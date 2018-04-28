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

! tests internal procedure as a pointer target

module mod
  logical rslt(2), expect(2)

contains

  subroutine foo()
    procedure(bar), pointer :: p
    integer a
    p=>bar
    a = 0
    call bar()
    !print *, a
    rslt(1) = a .eq. -99
    a = 1
    call p()
    !print *, a
    rslt(2) = a .eq. -99
    
  contains
    
    subroutine bar()
      a=-99
    end subroutine bar
  end subroutine foo
end module mod

use mod
expect = .true.
rslt = .false.
call foo()
call check(rslt, expect, 2)
end program
