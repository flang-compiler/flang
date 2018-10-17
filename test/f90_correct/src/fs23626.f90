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
!
program a
  implicit none

  type :: t1
     integer, dimension(:), allocatable :: mem1
  end type
  type :: t2
     type(t1), dimension(:), allocatable :: mem2
  end type

  integer, dimension(2) :: i = (/2, 3/)
  type(t2) :: x
  type(t2) :: y
  allocate(x%mem2(8))

  x%mem2(1)%mem1 = (/11, 12, 13, 14/)
  x%mem2(2)%mem1 = (/21, 22, 23, 24/)
  x%mem2(3)%mem1 = (/31, 32, 33, 34/)

  y%mem2 = x%mem2(i)
  if (y%mem2(1)%mem1(1) /= 21) stop "FAIL"
  stop "PASS"

end program
