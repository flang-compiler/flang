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
! flang#196 ICE: mk_mem_ptr_shape: lwb not sdsc
module t4
  implicit none

  ! error happened because both type have member named 'm'
  type T1
    integer, dimension(:,:), allocatable :: m
    integer, dimension(:,:), allocatable :: m1
  end type
  type T2
    type(T1), dimension(:,:), allocatable :: m
    type(T1), dimension(:,:), allocatable :: m2
  end type

contains

  subroutine sub(x)
    type(T2) :: x
    type(T1), dimension(:,:), allocatable :: tmp
    integer :: i = 1
    allocate(tmp(i,i))
    call move_alloc(tmp(i,i)%m1, x%m2(i,i)%m)
  end subroutine

end module
