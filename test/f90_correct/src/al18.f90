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
! Compile with -Mallocatable=03
! Tests for allocatable assignment.
program al18
  implicit none

  type :: A
    integer :: m = 0
  end type
  type :: B
    type(A), allocatable :: ptr(:)
  end type

  call test1()
  call test2()
  call test3()
  call test4()
  call test5()

contains

  ! Allocate conformable y
  subroutine test1()
    type(B) :: x
    integer, allocatable :: y(:)
    allocate(x%ptr(4))
    x%ptr%m = [1, 2, 3, 4]
    allocate(y(4))
    y = x%ptr%m
    call check(y, [1, 2, 3, 4], 4)
  end subroutine
  
  ! Allocate non-conformable y
  subroutine test2()
    type(B) :: x
    integer, allocatable :: y(:)
    allocate(x%ptr(4))
    x%ptr%m = [1, 2, 3, 4]
    allocate(y(2))
    y = x%ptr%m
    call check(y, [1, 2, 3, 4], 4)
  end subroutine

  ! Don't allocate y
  subroutine test3()
    type(B) :: x
    integer, allocatable :: y(:)
    allocate(x%ptr(4))
    x%ptr%m = [1, 2, 3, 4]
    y = x%ptr%m
    call check(y, [1, 2, 3, 4], 4)
  end subroutine
  
  ! Assign in other direction
  subroutine test4()
    integer, allocatable :: x(:)
    type(B) :: y
    allocate(x(4))
    allocate(y%ptr(4))
    x = [1, 2, 3, 4]
    y%ptr%m = x
    call check(y%ptr%m, [1, 2, 3, 4], 4)
  end subroutine

  ! Add another level of members: x%ptr(:)%a%m
  subroutine test5()
    integer :: i
    type :: A2
      type(A), allocatable :: a
    end type
    type :: B2
      type(A2), allocatable :: ptr(:)
    end type
    type(B2) :: x
    integer, allocatable :: y(:)
    allocate(x%ptr(4))
    do i = 1, 4
      allocate(x%ptr(i)%a)
    end do
    x%ptr%a%m = [1, 2, 3, 4]
    y = x%ptr%a%m
    call check(y, [1, 2, 3, 4], 4)
  end subroutine

end program
