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
! Length of assumed-length CHARACTER function results (B.3.6)

module mod_set_foo_submod33
    Character*8,external :: FOO 
    Character*26 :: BAR
interface
  module subroutine call_foo
  end subroutine

  module subroutine  check_abc
  end subroutine
end interface
end module

submodule (mod_set_foo_submod33) submod_set_foo_submod33
    character*3 :: abc
contains
    module procedure call_foo
        print *, FOO(BAR)
    end procedure
    module procedure check_abc
        if (LEN(abc) .EQ. 3 ) then
            print *, "length of abc: "
        print *, " PASS "
        else
            print *, "FAIL, length of abc is wrong"
        endif
    end procedure
end submodule

program do_foo
    use mod_set_foo_submod33
    implicit none
    BAR = 'abcdefghijklmnopqrstuvwxyz'
    call call_foo
    if  ( LEN(BAR) .EQ. 26 ) then 
        print *, "length of bar "
        print *, " PASS "
    else 
        print *, "FAIL, length of BAR is wrong"
    end if
    
    if ( LEN(FOO(BAR)) .EQ. 8 ) then
        print *, "length of foo" 
        print *, " PASS "
    else
        print *, "FAIL, length of FOO is wrong"
    end if
    call check_abc
end program do_foo

    Character*(*) FUNCTION FOO(STR)
        CHARACTER*26 STR
        FOO = STR
        RETURN
    END function

