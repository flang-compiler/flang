!
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
!
! test weird naming, such as a submodule named "submodule"

module module
  integer :: x = 0;
  interface
    module subroutine tick
    end subroutine
    module subroutine tick2
    end subroutine
  end interface
end module module 

submodule (module) submodule
contains
  module procedure tick
    x = x + 1
  end procedure tick
end submodule submodule

submodule (module:submodule) module
contains 
  module procedure tick2
    call tick
    call tick
  end procedure tick2
end submodule module

module submodule 
  integer :: y = 0
  interface
    module subroutine add2
    end subroutine
    module subroutine add4
    end subroutine
  end interface
end module submodule

submodule (submodule) module
contains
  module procedure add2
    y = y + 2
  end procedure add2
end submodule

submodule (submodule) submodule
contains
  module procedure add4
    y = y + 4
  end procedure add4
end submodule


program checkxy
use module
use submodule
call tick
call tick2
call add2
call add4
print *, "x is ", x, " and y is ", y
if ( x .EQ. 3 .AND. y .EQ. 6) then
  print *, " PASS "
else
  print *, "TEST FAILED"
end if
end program checkxy

