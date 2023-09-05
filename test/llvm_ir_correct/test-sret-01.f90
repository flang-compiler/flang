! Part of the LLVM Project, under the Apache License v2.0 with LLVM
! Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
!
! Test for the scenario that when Fortran case calls the function of
! C with a struct as the return value.

! RUN: %flang -S -emit-flang-llvm %s -o %t
! cat %t | FileCheck %s -check-prefix=CHECK-SRET

! CHECK-SRET: call void @c_function (ptr sret([[TYPENAME:%.*]]) {{%.*}}, ptr byval([[TYPENAME]]) {{%.*}})
! CHECK-SRET: declare void @c_function(ptr sret([[TYPENAME]]), ptr byval([[TYPENAME]]))
module c_interface
  use, intrinsic :: iso_c_binding
  type, bind(c) :: c_type
     real(kind = c_double) :: year = 0, month = 0, day = 0, hour = 0, minute = 0
  end type
  interface
    type(c_type) function c_function(dur) bind(c)
      use iso_c_binding
      import :: c_type
      type(c_type), value :: dur
    end function
  end interface
end module

module test
  use c_interface
contains
  function ss(dur) result(res)
  use c_interface, only : c_type
    implicit none
    type(c_type), intent(in) :: dur
    type(c_type) :: res
    res = c_function(dur)
  END FUNCTION
END MODULE

