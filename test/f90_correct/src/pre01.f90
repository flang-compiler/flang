!
! Copyright (c) 2014, NVIDIA CORPORATION.  All rights reserved.
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
! This tests macro expansion with single and double quoted characters
!
#define STR1 "Don't"
#define STR2 'Don''t'
#define STR3 "Don""t"
#define STR4 'Don"t'
#define FOO(_str) _str

program p
    logical :: res(3) = (/.false., .false., .false./)
    logical :: expect(3) = (/.true., .true., .true./)
    character(32) :: word1 = STR1
    character(32) :: word2 = STR2
    character(32) :: word3 = STR3
    character(32) :: word4 = STR4
    character(32) :: word5 = FOO('foo "" bar')
    character(32) :: word6 = FOO("foo '' bar")
    print *, word1
    print *, word2
    print *, word3
    print *, word4
    print *, word5
    print *, word6

    if (word1 == "Don't" .and. word2 == "Don't") then
        res(1) = .true.
    endif
    if (word3 == 'Don"t' .and. word4 == 'Don"t') then
        res(2) = .true.
    endif

    if (word5 == 'foo "" bar' .and. word6 == "foo '' bar") then
        res(3) = .true.
    endif
    
    call check(res, expect, 3)
end program
