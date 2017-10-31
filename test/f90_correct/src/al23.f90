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
! Like al21 and al22 but array elements are derived type with allocatable members.
program al23
  implicit none
  logical :: fail = .false.
  integer :: i
  type :: dt
    real, allocatable :: mem
  end type
  type(dt) :: a1(16)
  type(dt), allocatable :: a2(:)
  do i = 1, 16
    a1(i)%mem = i
  end do
  a2 = a1

  call test_cshift1()
  call test_cshift2()
  call test_cshift3(a1)
  if (.not. fail) write(*,'("PASS")')

contains

  subroutine test_cshift1()
    type(dt), allocatable :: b1(:)
    b1 = cshift(a1, 4)
    call check('cshift', b1, [ &
       5.0,  6.0,  7.0,  8.0, 9.0, 10.0, 11.0, 12.0, &
      13.0, 14.0, 15.0, 16.0, 1.0,  2.0,  3.0,  4.0  &
    ])
  end subroutine

  subroutine test_cshift2()
    type(dt), allocatable :: b2(:)
    b2 = cshift(a2, 4)
    call check('cshift', b2, [ &
       5.0,  6.0,  7.0,  8.0, 9.0, 10.0, 11.0, 12.0, &
      13.0, 14.0, 15.0, 16.0, 1.0,  2.0,  3.0,  4.0  &
    ])
  end subroutine

  subroutine test_cshift3(a1)
    type(dt) :: a1(:)
    type(dt), allocatable :: b1(:)
    b1 = cshift(a1, 4)
    call check('cshift', b1, [ &
       5.0,  6.0,  7.0,  8.0, 9.0, 10.0, 11.0, 12.0, &
      13.0, 14.0, 15.0, 16.0, 1.0,  2.0,  3.0,  4.0  &
    ])
  end subroutine

  ! Check that actual is the same as expected; report failure if not.
  subroutine check(label, actual, expected)
    character(len=*) :: label
    type(dt) :: actual(:)
    real :: expected(:)
    logical :: equal
    equal = .true.
    if (size(actual) /= size(expected)) then
      equal = .false.
    else
      do i = 1, size(expected)
        if (actual(i)%mem /= expected(i)) then
          print *,'i:',i
          equal = .false.
        end if
      end do
    end if
    if (.not. equal) then
      write(*,'("FAIL: ",a)') label
      print *," expected:", expected
      print *," actual:  ", actual
      fail = .true.
    end if
  end subroutine

end
