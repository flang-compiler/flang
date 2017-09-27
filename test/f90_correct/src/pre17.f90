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
! Test multi-line and stringification and concatenation directives
!
#define STR(_s) # _s
#define DEF character(32) ::
#define DEFSTR(_s) DEF _s \
    = \
    #_s

#define MKSTR(_a, _b) DEF _a ## _b \
    = \
    STR(_a ## _b)

program p
    logical :: res(1) = .false., expect(1) = .true.
    DEFSTR(foo)
    MKSTR(foo, bar)
    print *, foo
    print *, foobar

    if (foo == 'foo' .and. foobar == 'foobar') then
        res(1) = .true.
    endif

    call check(res, expect, 1)
end program
