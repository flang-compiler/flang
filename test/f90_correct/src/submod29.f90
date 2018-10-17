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
module foo_submod29
integer, allocatable :: arr
interface
    module subroutine check_alloc
    end subroutine
    module subroutine check_not_alloc
    end subroutine
end interface
end module

submodule (foo_submod29) bar
contains
    module procedure check_alloc
        if ( allocated(arr) ) then
            print *, " PASS "
        else
          print *, "FAIL"
        endif
    end procedure
    module procedure check_not_alloc
        if ( .not. allocated(arr) ) then
            print *, "PASS"
        else
          print *, "FAIL"
        endif
    end procedure
end submodule

program foobar
use foo_submod29
implicit none
call check_not_alloc
allocate (arr)
call check_alloc
end program foobar
