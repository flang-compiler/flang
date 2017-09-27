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

module cc
 real, parameter, dimension(5) :: pp = (/ 1,2,3,4,5 /)
 real, parameter, dimension(10) :: qq = (/ pp(1:5),pp(1:5) /)
end module

use cc
real,expect(10)
expect(1:5) = pp
expect(6:10) = pp
call check(qq,expect,10)
end
