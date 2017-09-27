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
! Test concatenation and to ensure the preprocessor does not strip the
! concatenation operator, but also expands the items following '//'
!
#define STR  "bar"
#define STR2 "baz"
program p
    logical :: res(1) = .false., expect(1) = .true.
    print *, "foo"//STR//STR2
    if ("foo"//STR//STR2 == "foobarbaz") then
        res(1) = .true.
    endif

    call check(res, expect, 1)
end program
