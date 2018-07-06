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

! host routine count intrinsic with contained routine count variable

integer n
n = count([.true., .false., .true.])
call ss(n)

contains
  subroutine ss(n)
    integer :: count
    count = n
    if (count .ne. 2) print*, 'FAIL'
    if (count .eq. 2) print*, 'PASS'
  end
end
