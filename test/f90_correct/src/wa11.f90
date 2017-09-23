! Copyright (c) 1990-2017, NVIDIA CORPORATION.  All rights reserved.
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

! bugs in named constant initialization.

type test
real r, r2(2,3)
end type test

real, dimension(7) :: result
real, dimension(7) :: expect = (/ 2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 /)

real, parameter :: Q(2,3)=1
type(test),parameter :: bugged = test(2,Q)

result(1) = bugged%r;
result(2) = bugged%r2(1,1)
result(3) = bugged%r2(1,2)
result(4) = bugged%r2(1,3)
result(5) = bugged%r2(2,1)
result(6) = bugged%r2(2,2)
result(7) = bugged%r2(2,3)
call check(result, expect, 7)

end
