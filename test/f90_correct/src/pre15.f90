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
! This example is slightly modified from the Preprocessor chapter in the
! C99 spec: Example 4 in section 6.10.3.4:
!
! "To illustrate the rules for creating character string literals and
! concatenating tokens..."
!
! Ensure that -Hx,124,0x100000 (Skip Fortran comments) is enabled
!
! Output should be:
! "hello"
! "hello" ", world"
#define glue(a, b)  a ## b
#define xglue(a, b) glue(a, b)
#define HIGHLOW     "hello"
#define LOW         LOW", world"
program p
    logical :: res(1) = .false., expect(1) = .true.
    print *, glue(HIGH, LOW)
    print *, xglue(HIGH, LOW)

    if (glue(HIGH, LOW) == "hello") then
        res(1) = .true.
    endif
    call check(res, expect, 1)
end program
