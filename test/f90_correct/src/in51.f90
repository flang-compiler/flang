! Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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

! undeclared intrinsic reference in an internal routine

character*4 :: s = 'FAIL'
if (f1() + f2() .eq. 5) s = 'PASS'
print*, s

contains
  integer function f1
    integer :: count
    f1 = count([.true., .false., .true.])
  end

  integer function f2
    count = 3
    f2 = count
  end
end
