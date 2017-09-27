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
! This tests replacement and concatenation with single and 
! double quote literals.
!
#define STR(_s) _s
#define CAT(_a, _b) _a##_b

program p
    logical :: res(2), expect(2) = (/.true., .true./)
    print *, STR('c character ! bang " quote * asterisk') ! Single-quote
    res(1) = STR('c character ! bang " quote * asterisk') .eq. 'c character ! bang " quote * asterisk'
        
    print *, STR("c character ! bang ' quote * asterisk") ! Double-quote
    res(2) = STR("c character ! bang ' quote * asterisk") .eq. "c character ! bang ' quote * asterisk"

    print *, CAT('! bang " quote * asterisk', '! bang " quote * asterisk') ! Single-quote
    print *, CAT("! bang ' quote * asterisk", "! bang ' quote * asterisk") ! Double-quote

    call check(res, expect,  2)
end program
