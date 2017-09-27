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
! This tests stringization
!
#define STR(_s) #_s

program p
    logical :: res(1) = .false., expect(1) = .true.
    print *, STR(A quote "quote" and 'another' in the middle! ... Of a string!)
!    print *, STR(This is an awesome example of string replacement)
    if (STR(foobarbaz) == 'foobarbaz') then
        res(1) = .true.
    endif

    call check(res, expect, 1)
end program
