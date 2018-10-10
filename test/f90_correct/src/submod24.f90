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
module ancestor
  interface 
    module subroutine hello
    end subroutine
  end interface
end module ancestor

! C1413 - negative - submodule-name in the end-submodule-stmt does not match
!  the name in the submodule-stmt. (compilation failure must report this)
submodule (ancestor) descendant
  contains
  module procedure hello
    print *, "hello world"
  end procedure
end submodule ancestor !{error "PGF90-S-0309-Incorrect name, ancestor, specified in END statement"}


program main
  use ancestor
  call hello
end program main


