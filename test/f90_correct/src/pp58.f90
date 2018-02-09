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

! Test pointer to contiguous array section is passed correctly for
! assumed-shape dummy argument.
module fs25090
  implicit none
  logical :: failed = .false.

contains

  subroutine sub1(l, m, n, nn)
    integer, intent(in) :: l, m, n, nn
    integer :: i, j, k
    integer(8) :: expected, actual
    integer(8), pointer, contiguous :: p(:,:)
    integer(8), pointer :: matrix(:,:,:)
    ! initialize matrix
    allocate(matrix(l, m, n))
    do i = 1, l
      do j = 1, m
        do k = 1, n
          matrix(i, j, k) = 100*l + 10*m + n
        end do
      end do
    end do
    p => matrix(:,:,nn)
    call sub2(p)
    ! check matrix
    do i = 1, l
      do j = 1, m
        do k = 1, n
          expected = 100*l + 10*m + n
          if (k .eq. nn) expected = -expected
          actual = matrix(i, j, k)
          if (expected .ne. actual) then
            write(*,'("FAIL at",3i3,": expected=",i5," actual=",i5)') i, j, k, expected, actual
            failed = .true.
          end if
        end do
      end do
    end do
  end subroutine

  subroutine sub2(x)
    integer(8) :: x(:,:)
    integer(8) :: i, j
    do i = lbound(x, 1), ubound(x, 1)
      do j = lbound(x, 2), ubound(x, 2)
        x(i, j) = -x(i, j)
      end do
    end do
  end subroutine

end module

use fs25090
call sub1(3, 3, 3, 2)
call sub1(2, 3, 4, 4)
if (.not. failed) write(*,'("PASS")')
end
