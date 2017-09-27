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
! Test assigning section of array of derived type where lower bound and stride
! are not 1.
program dt71
  implicit none
  integer :: i, j, actual(60), expected(60), curr = 0

  integer :: res(4, 3)

  type t1
    integer :: m1(4:6)
  end type t1

  type t2
    type(t1), allocatable :: m2(:)
  end type t2

  type(t1) :: x(3:9)

  do i = 3, 9
    do j = 4, 6
      x(i)%m1(j) = 10*i + j
    end do
  end do

  call test1(x)
  call test2(x)
  call test3(x)
  call test4(x)
  call test5(x)

  call check(actual, (/ &
      34, 35, 36, 54, 55, 56, 74, 75, 76, 94, 95, 96, &
      34, 35, 36, 54, 55, 56, 74, 75, 76, 94, 95, 96, &
      34, 35, 36, 54, 55, 56, 74, 75, 76, 94, 95, 96, &
      34, 35, 36, 54, 55, 56, 74, 75, 76, 94, 95, 96, &
      34, 35, 36, 54, 55, 56, 74, 75, 76, 94, 95, 96  &
    /), curr)

contains

  ! Target of assignment is object of type t1
  subroutine test1(x)
    type(t1), intent(in) :: x(3:9)
    type(t1) :: y(2:5)
    integer :: i, j
    y = x(3:9:2)
    do i = 2, 5
      do j = 4, 6
        curr = curr + 1
        actual(curr) = y(i)%m1(j)
      end do
    end do
  end subroutine

  ! Target of assignment is member of type t1
  subroutine test2(x)
    type(t1), intent(in) :: x(3:9)
    type(t2) :: y
    integer :: i, j
    y%m2 = x(3:9:2)
    do i = 1, 4
      do j = 4, 6
        curr = curr + 1
        actual(curr) = y%m2(i)%m1(j)
      end do
    end do
  end subroutine

  ! Like test1 but y is allocatable
  subroutine test3(x)
    type(t1), intent(in) :: x(3:9)
    type(t1), allocatable :: y(:)
    integer :: i, j
    allocate(y(2:5))
    ! TODO: y is getting bounds 1:4 -- should remain 2:5?
    y = x(3:9:2)
    do i = lbound(y, 1), ubound(y, 1)
      do j = 4, 6
        curr = curr + 1
        actual(curr) = y(i)%m1(j)
      end do
    end do
  end subroutine

  ! Like test3 but y is allocatable and not explicitly allocated
  ! Requires -Mallocatable=03
  subroutine test4(x)
    type(t1), intent(in) :: x(3:9)
    type(t1), allocatable :: y(:)
    integer :: i, j
    y = x(3:9:2)
    do i = 1, 4
      do j = 4, 6
        curr = curr + 1
        actual(curr) = y(i)%m1(j)
      end do
    end do
  end subroutine

  ! Like test3 but assigning to a section of y
  subroutine test5(x)
    type(t1), intent(in) :: x(3:9)
    type(t1), allocatable :: y(:)
    integer :: i, j
    allocate(y(1:13))
    y(4:13:3) = x(3:9:2)
    do i = 4, 13, 3
      do j = 4, 6
        curr = curr + 1
        actual(curr) = y(i)%m1(j)
      end do
    end do
  end subroutine

end program
