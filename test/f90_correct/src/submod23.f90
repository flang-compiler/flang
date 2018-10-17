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

! C1412 (R1418) The ancestor-module-name shall be the name of a nonintrinsic
! module that declares a separate module procedure; the parent-submodule-name
! shall be the name of a descendant of that module.

!C1412 ancestor-module is nonintrinsic, declares a module procedure
module ancestor
  interface 
    module subroutine hello
    end subroutine
    module subroutine hello2
    end subroutine
  end interface
end module ancestor

module ancestor2
  interface 
    module subroutine hello
    end subroutine
    module subroutine hello2
    end subroutine
  end interface
end module ancestor2

submodule (ancestor) descendant23a
  contains
  module procedure hello
    print *, "hello world"
  end procedure
end submodule descendant23a

submodule (ancestor2) descendant23b
  contains
  module procedure hello
    print *, "hello world"
  end procedure
end submodule descendant23b

! C1412 - negative - parent-submodule-name (descendant23b) is not a descendant of ancestor
submodule (ancestor:descendant23b) grand_descendant !{error "PGF90-F-0004-Unable to open MODULE file ancestor-descendant23b.mod"}
  contains
  module procedure hello2
    print *, "hello again, world"
  end procedure
end submodule 

program main
  use ancestor
  call hello
  call hello2
end program main
