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
! This tests macro replacement
!
#define PR(_expr, _msg) if (_expr) then; print *, _msg; res(1)=42; end if

program p
    integer :: x
    integer :: res(1) = 0, expect(1) = 42
    PR(100 .lt. 200, "Yes!")
    call check(res, expect, 1)
end program
