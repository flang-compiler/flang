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
! This tests the warning and line directives.  The warning should be listed as
! occurring on line 43 since the #line directive should have adjusted the line
! offset.
!
! This tests 'C' in a macro proceeding a directive (it is not a comment)
!
#define C "A string"
program p
#ifdef C
    print *, C
    print *, "PASS"
    call check(.true., .true., 1)
#else
    print *, "FAIL"
    call check(.false., .true., 1)
#endif
end program
