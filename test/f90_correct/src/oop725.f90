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

! tests internal procedures as pointer targets 

module mod
  logical rslt(3), expect(3)
contains
  
  subroutine parent(p)
    procedure(), pointer :: p
    call p()
  end subroutine parent
  
  subroutine foo()
    procedure(bar), pointer :: p
    integer a
    a=0
    p=>bar
    call bar()
    rslt(1) = a .eq. 1
    call p()
    rslt(2) = a .eq. 2
    call parent(p)
    rslt(3) = a .eq. 3
  contains
    
    subroutine bar()
      a=a+1
    end subroutine bar
  end subroutine foo
end module mod

use mod
expect = .true.
rslt = .false.
call foo()
call check(rslt,expect,3)
end program
