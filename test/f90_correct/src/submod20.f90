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

! C1413 (R1416) If a submodule-name appears in the end-submodule-stmt, it shall
! be identical to the one in the submodule-stmt.

! C1412 (R1418) The ancestor-module-name shall be the name of a nonintrinsic
! module that declares a separate module procedure; the parent-submodule-name
! shall be the name of a descendant of that module.

! C1411 (R1416) A submodule specification-part shall not contain a format-stmt


!C1412 ancestor-module is nonintrinsic, declares a module procedure
module ancestor
  interface 
    module subroutine hello
    end subroutine
    module subroutine hello2
    end subroutine
  end interface
end module ancestor

! C1411 - submodule specification-part does not contain a format-stmt
! C1413 - test that matching submodule-name is accepted
submodule (ancestor) descendant
  contains
  module procedure hello
!    print *, "hello world"
     write(*,"(a)",advance="no")" PA"  
  end procedure
end submodule descendant

! C1411 - submodule specification-part does not contain a format-stmt
! C1412 - parent-submodule-name (descendant) is a descendant of ancestor
! C1413 - test that end-submodule-stmt without submodule-name is accepted
submodule (ancestor:descendant) descendant2
  contains
  module procedure hello2
     write(*,"(a)",advance="no") "SS "
     print *, ""
!    print *, "hello again, world"
  end procedure
end submodule 


program main
  use ancestor
  call hello
  call hello2
end program main


