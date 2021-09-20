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
        procedure :: f_none
        procedure ,nopass :: f_int
        procedure :: f_real
        generic :: f => f_none, f_int, f_real
! Case 2:
        procedure , nopass :: f_none1
        procedure :: f_int1
        procedure ,nopass :: f_real1
        generic :: f1 => f_none1, f_int1, f_real1
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
    integer function f_none( me ) result (RSLT)
        class(A_t) :: me
        RSLT = 1
    end function f_none
    integer function f_int( n ) result (RSLT)
        integer :: n
        RSLT = n - 1
    end function f_int
    real function f_real( me, x ) result (RSLT)
        class(A_t) :: me
        real :: x
        RSLT = x + 1
    end function f_real

! Case 2:
    integer function f_none1() result (RSLT)
        RSLT = 2
    end function f_none1
    integer function f_int1( me, n ) result (RSLT)
        class(A_t) :: me
        integer :: n
        RSLT = n - 1
    end function f_int1
    real function f_real1( x ) result (RSLT)
        real :: x
        RSLT = x + 1
    end function f_real1

! Case 3:
    integer function f_int2( n ) result (RSLT)
        integer :: n
        RSLT = n - 1
    end function f_int2
    real function f_real2( x ) result (RSLT)
        real :: x
        RSLT = x + 1
    end function f_real2

! Case 3:
    integer function f_int3( me, n ) result (RSLT)
        class(A_t) :: me
        integer :: n
        RSLT = n - 1
    end function f_int3
    real function f_real3( me, x ) result (RSLT)
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
    logical results(10)
    logical expect(10)

    results = .false.
    expect = .true.

    results(1) = 9 .eq. A%f(10)
    results(2) = 99 .eq. A%f1(100)
    results(3) = 999 .eq. A%f2(1000)
    results(4) = 9999 .eq. A%f3(10000)

    results(5) = 11.1 .eq. A%f(10.1)
    results(6) = 101.1 .eq. A%f1(100.1)
    results(7) = 1001.1 .eq. A%f2(1000.1)
    results(8) = 10001.1 .eq. A%f3(10000.1)

    results(9) = 1 .eq. A%f()
    results(10) = 2 .eq. A%f1()

    call check(results,expect,10)
end
