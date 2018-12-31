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

module top
  interface
    real module function f(x)
      real :: x
    end function f
    module subroutine test
    end subroutine test
  end interface
end module top

submodule(top) one
  type :: derived
   contains
    procedure, nopass :: tbp => f
  end type derived
end submodule one

submodule(top) two
 contains
  module procedure f
    f = x * 2.0
  end procedure f
end submodule two

submodule(top:one) three
 contains
  module procedure test
    type(derived) :: dt
    !print *, dt%tbp(1.0) ! <- should print 2.0
    if (dt%tbp(1.0) .eq. 2.000000) then
      print *, "PASS"
    else
      print *, "FAIL"
    end if
  end procedure test
end submodule three

program main
  use top
  call test
end program
