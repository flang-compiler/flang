! Copyright (c) 1990-2019, NVIDIA CORPORATION.  All rights reserved.
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
!  Test of issue #721
!
logical :: failed=.FALSE.
character(:), allocatable :: a, b
a = 'foo'
call move_alloc(a, b)
if (allocated(a)) failed=.TRUE.
if (.not.allocated(b)) failed=.TRUE.
if (len(b) /= 3) failed=.TRUE.
if (b /= 'foo') failed=.TRUE.

if (failed) then
    call check(0,1,1)
else
    call check(1,1,1)
end if

end
