! Copyright (c) 2000, NVIDIA CORPORATION.  All rights reserved.
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

!   Initialization expressions containing PARAMETER references

module dd
 type dt
  real :: m
 end type
 type(dt), parameter, dimension(2:6) :: pp = &
   (/ dt(1.), dt(2.5), dt(3), dt(4), dt(5) /)
 type(dt), parameter, dimension(2) :: qq = (/ pp(3:4) /)
end module

use dd
real,dimension(2) :: result, expect
result = qq(:)%m
expect = pp(3:4)%m
call check(result,expect,2)
end
