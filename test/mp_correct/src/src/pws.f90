!
! Copyright (c) 2016, NVIDIA CORPORATION.  All rights reserved.
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
! Parallel workshare OpenMP test
!


program test
integer :: res = 0
integer :: expected = 1
call try1(res)
call check(res, expected, 1)
stop
end


subroutine try1(x)
integer :: x

!$OMP PARALLEL WORKSHARE
x = 1
!$OMP END PARALLEL WORKSHARE

end subroutine try1

