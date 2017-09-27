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
program dt70
  implicit none
  integer :: i, j, k, actual(32), expected(32), curr = 0

  type dt1
    integer, allocatable :: m1
  end type

  type dt2
    type(dt1), allocatable :: m2(:)
  end type

  type(dt2), allocatable :: x1(:), x2(:,:)

  allocate(x1(1:4))
  allocate(x2(1:4, 1:4))

  do j = 1, 4
    allocate(x1(j)%m2(2:3))
    do k = 2, 3
      allocate(x1(j)%m2(k)%m1)
      x1(j)%m2(k)%m1 = 100*i + 10*j + k
    end do
  end do

  do i = 1, 4
    do j = 1, 4
      allocate(x2(i, j)%m2(2:3))
      do k = 2, 3
        allocate(x2(i, j)%m2(k)%m1)
        x2(i, j)%m2(k)%m1 = 100*i + 10*j + k
      end do
    end do
  end do

  call test_1_dim(0)
  call test_1_dim(10)
  call test_2_dim(0)
  ! problem case: 2 dimensional with 2nd dim of y different
  call test_2_dim(10)
  call check(actual, expected, curr)

contains

  subroutine test_1_dim(offset)
    integer, intent(in) :: offset
    type(dt2), allocatable :: y(:)
    allocate(y(offset+1:offset+4))
    y(:) = x1(:)
    do j = 1, 4
      do k = 2, 3
        curr = curr + 1
        actual(curr) = y(offset+j)%m2(k)%m1
        expected(curr) = 10*j + k
      end do
    end do
  end subroutine

  subroutine test_2_dim(offset)
    integer, intent(in) :: offset
    type(dt2), allocatable :: y(:,:)
    allocate(y(1:4, offset+1:offset+4))
    y(1,:) = x2(1,:)
    do j = 1, 4
      do k = 2, 3
        curr = curr + 1
        actual(curr) = y(1, offset+j)%m2(k)%m1
        expected(curr) = 100*1 + 10*j + k
      end do
    end do
  end subroutine

end program
