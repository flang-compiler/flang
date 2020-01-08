!
! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
!


module test_m
    implicit none

    type A_t
    contains
! Case 1:
        procedure ,nopass :: f_int
        procedure :: f_real
        generic :: f => f_int, f_real
! Case 2:
        procedure :: f_int1
        procedure ,nopass :: f_real1
        generic :: f1 => f_int1, f_real1
! Case 3:
        procedure ,nopass:: f_int2
        procedure ,nopass :: f_real2
        generic :: f2 => f_int2, f_real2
! Case 4:
        procedure :: f_int3
        procedure :: f_real3
        generic :: f3 => f_int3, f_real3
    endtype

contains
! Case 1:
    integer function f_int( n ) result (RSLT)
        integer :: n
        RSLT = n - 1
    end function f_int
    integer function f_real( me, x ) result (RSLT)
        class(A_t) :: me
        real :: x
        RSLT = x + 1
    end function f_real

! Case 2:
    integer function f_int1( me, n ) result (RSLT)
        class(A_t) :: me
        integer :: n
        RSLT = n - 1
    end function f_int1
    integer function f_real1( x ) result (RSLT)
        real :: x
        RSLT = x + 1
    end function f_real1

! Case 3:
    integer function f_int2( n ) result (RSLT)
        integer :: n
        RSLT = n - 1
    end function f_int2
    integer function f_real2( x ) result (RSLT)
        real :: x
        RSLT = x + 1
    end function f_real2

! Case 3:
    integer function f_int3( me, n ) result (RSLT)
        class(A_t) :: me
        integer :: n
        RSLT = n - 1
    end function f_int3
    integer function f_real3( me, x ) result (RSLT)
        class(A_t) :: me
        real :: x
        RSLT = x + 1
    end function f_real3
end module

program main
USE CHECK_MOD
    use test_m
    implicit none
    type(A_t) :: A
    logical results(4)
    logical expect(4)

    results = .false.
    expect = .true.

    results(1) = 9 .eq. A%f(10)
    results(2) = 99 .eq. A%f1(100)
    results(3) = 999 .eq. A%f2(1000)
    results(4) = 9999 .eq. A%f3(10000)

    call check(results,expect,4)
end
