! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
!
! Test for the reshape function calls when the array element of the source is
! an assumed length character varivable.

program test
  call check_alloc('1', '2', '3', '4')
  call check_auto('12', '34', '56', '78', 4)
  call check_cp('123', '345', '567', '789')
  call check_fixed('abcd', 'efgh', 'ijkl', 'mnop')
  call check_p('12', '34', '56', '78')
  print *, "PASS"
contains
  subroutine check_alloc(w, x, y, z)
    character(*), intent(in) :: w, x, y, z
    character(1), allocatable :: a(:), b(:)

    a = [w, x, y, z]
    b = reshape([w, x, y, z], [4])
    if (any(a /= b)) stop 1
  end subroutine

  subroutine check_auto(w, x, y, z, n)
    character(*), intent(in) :: w, x, y, z
    integer, intent(in) :: n
    character(2) :: a(n), b(n)

    a = [w, x, y, z]
    b = reshape([w, x, y, z], [4])
    if (any(a /= b)) stop 2
  end subroutine

  subroutine check_cp(w, x, y, z)
    character(*), intent(in) :: w, x, y, z
    character(3), contiguous, pointer :: a(:), b(:)

    allocate(a(4), b(4))
    a = [w, x, y, z]
    b = reshape([w, x, y, z], [4])
    if (any(a /= b)) stop 3
  end subroutine
  
  subroutine check_fixed(w, x, y, z)
    character(*), intent(in) :: w, x, y, z
    character(4) :: a(4), b(4)

    a = [w, x, y, z]
    b = reshape([w, x, y, z], [4])
    if (any(a /= b)) stop 4
  end subroutine

  subroutine check_p(w, x, y, z)
    character(*), intent(in) :: w, x, y, z
    character(2), pointer :: a(:), b(:)

    allocate(a(4), b(4))
    a = [w, x, y, z]
    b = reshape([w, x, y, z], [4])
    if (any(a /= b)) stop 5
  end subroutine
end program
