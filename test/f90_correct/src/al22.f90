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

! Test assignment of array-valued intrinsics with F2003 allocatable
! assignment semantics. Compile with -Mallocatable=03.
! Like al21.f90 but source array is allocatable too.
program al22
  implicit none
  logical :: fail = .false.
  integer :: i
  real, allocatable :: a1(:)
  real :: a2(4,4)
  a1 = [ (i, i=1, 16) ]
  a2 = reshape(a1, [4, 4])

  call test_minval()
  call test_product()
  call test_sum()
  call test_cshift()
  call test_eoshift()
  if (.not. fail) write(*,'("PASS")')

contains

  subroutine test_minval()
    real, allocatable :: b1(:)
    b1 = minval(a2, 1)
    call check('minval', b1, [1.0, 5.0, 9.0, 13.0])
  end subroutine

  subroutine test_cshift()
    real, allocatable :: b1(:)
    b1 = cshift(a1, 4)
    call check('cshift', b1, [ &
       5.0,  6.0,  7.0,  8.0, 9.0, 10.0, 11.0, 12.0, &
      13.0, 14.0, 15.0, 16.0, 1.0,  2.0,  3.0,  4.0  &
    ])
  end subroutine

  subroutine test_product()
    real, allocatable :: b1(:)
    b1 = product(a2, 1)
    call check('product', b1, [24.0, 1680.0, 11880.0, 43680.0])
  end subroutine

  subroutine test_sum()
    real, allocatable :: b1(:)
    b1 = sum(a2, 1)
    call check('sum', b1, [10.0, 26.0, 42.0, 58.0])
  end subroutine

  subroutine test_eoshift()
    real, allocatable :: b1(:)
    b1 = eoshift(a1, 4)
    call check('cshift', b1, [ &
       5.0,  6.0,  7.0,  8.0, 9.0, 10.0, 11.0, 12.0, &
      13.0, 14.0, 15.0, 16.0, 0.0,  0.0,  0.0,  0.0  &
    ])
  end subroutine

  ! Check that actual is the same as expected; report failure if not.
  subroutine check(label, actual, expected)
    character(len=*) :: label
    real :: actual(:)
    real :: expected(:)
    if (size(actual) /= size(expected)) then
    else if (any(actual /= expected)) then
    else
      return
    end if
    write(*,'("FAIL: ",a)') label
    print *," expected:", expected
    print *," actual:  ", actual
    fail = .true.
  end subroutine

end
