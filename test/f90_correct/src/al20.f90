! Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
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

! The bounds of x are changed in an internal subroutine so
! they can't be propagated into shapes.  y is not a problem.
program al20

  integer, allocatable :: x(:), y(:)

  allocate(x(1:2))
  allocate(y(1:2))
  x = 1
  y = 2
  call check(x, [1, 1], 2)
  call check(y, [2, 2], 2)
  call sub()
  x = 3
  y = 4
  call check(x, [3, 3, 3], 3)
  call check(y, [4, 4], 2)

contains

  subroutine sub
    deallocate(x)
    allocate(x(3:5))
  end subroutine

end program
