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
! This tests token pasteing (aka concatenation)
!
#define CMD(_prefix, _fname, _msg, _res) _prefix ##_fname(_msg, _res)

subroutine foobar(words, res)
    logical :: res(1)
    character(12) :: words
    print *, words
    if (words == "G'day! Mate!") then
        res(1) = .true.
    endif
end subroutine

program p
    logical :: res(1) = .false., expect(1) = .true.
    call CMD(foo, bar, "G'day! Mate!", res)
    call check(res, expect, 1)
end program
