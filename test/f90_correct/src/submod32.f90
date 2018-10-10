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
module mod_submod32
integer a, b, m
interface
    module subroutine check_arr(arr)
        integer, intent(in) :: arr(:)
    end subroutine
end interface
end module mod_submod32


submodule (mod_submod32) submod_submod32
contains
    module procedure check_arr
        a = lbound(arr, DIM=1)
        b = size(arr)
        m = maxval(arr)
    end procedure
end submodule submod_submod32

program prog
    use mod_submod32
    integer x(7:15)
    x(7:15) = 0
    x(9:10) = 1
    x(15) = 10
    print *, "lbound: ", lbound(x)
    print *, "kind: ", kind(x)
    print *, "maxval: ", maxval(x)
    call check_arr(x)
    if ( a .EQ. lbound(x, DIM=1) .AND. b .EQ. size(x) .AND. maxval(x) .EQ. m) then 
        print *, " PASS "
    else 
        print *, "FAILED: lbound of arr in submod is ", a, " and lbound of x is ", lbound(x, DIM=1)
        print *, "FAILED: size of arr in submod is ", b, " and size of x is ", size(x)
        print *, "FAILED: maxval of arr in submod is", m, " and maxval of x is", maxval(x)
    end if
end program prog
