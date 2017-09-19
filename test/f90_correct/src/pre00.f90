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
! This tests comments with single or double quotes in the middle, the
! preprocessor should not choke on this ' or " that.
! To enable c99 macro replacement, this should be run with:
!    -Mpreprocess
!    -Hy,124,0x100000 (Do not skip Fortran comments, preprocess them)
!
program p
    logical :: res(1) = .false., expect(1) = .true.
    ! This is a single quote ' that was a single quote, thank you.
    character(32) :: word1 = "Foo" ! This ' is a single quote .

    ! This is a double quote " that was a double quote.
    character(32) :: word2 = "Bar" ! This " is was a double quote.

    if (word1 == 'Foo' .and. word2 == 'Bar') then
        res(1) = .true.
    endif

    call check(res, expect, 1)
end program
