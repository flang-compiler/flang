!
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
!
! Sourced allocate of derived type with nested derived type.
program P
  type A
    integer :: a1
    integer, allocatable :: a2
    integer, allocatable :: a3(:)
  end type
  type B
    integer :: b1
    integer, allocatable :: b2
    type(A) :: b3
    type(A), allocatable :: b4
  end type
  type(B), allocatable :: x, y

  allocate(x)
  allocate(x%b4)
  allocate(x%b3%a3(3))
  x%b1 = 1
  x%b2 = 2
  x%b3%a1 = 3
  x%b3%a2 = 4
  x%b3%a3(:) = 9
  x%b4%a1 = 5
  x%b4%a2 = 6

  allocate(y, source=x)

  x%b1 = -1
  x%b2 = -2
  x%b3%a1 = -3
  x%b3%a2 = -4
  x%b3%a3(:) = -9
  x%b4%a1 = -5
  x%b4%a2 = -6
  call check( &
    [y%b1, y%b2, y%b3%a1, y%b3%a2, y%b4%a1, y%b4%a2, y%b3%a3(:)], &
    [1, 2, 3, 4, 5, 6, 9, 9, 9], 9)
end program
