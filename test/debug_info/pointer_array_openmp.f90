! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! REQUIRES: llvm-19
! RUN: %flang -g -fopenmp -S -emit-llvm %s -o - | FileCheck %s

! CHECK: define internal void @main_sub
! CHECK: define internal void @__nv_main_sub_
! CHECK:      #dbg_declare(ptr %"res$p
! CHECK-NEXT: #dbg_declare(ptr %"res$p
! CHECK-NEXT: #dbg_declare(ptr %"res$sd

program main
  type :: dtype
    integer(4) :: fdim
    real(8), pointer :: fld_ptr(:)
  end type dtype
  type(dtype) :: dvar
  allocate(dvar%fld_ptr(100))
  call sub(dvar)
  deallocate(dvar%fld_ptr)

contains

  subroutine sub(arg)
    type(dtype),intent(inout) :: arg
    integer:: count               ! indices
    real(8), pointer :: res(:)
!$OMP PARALLEL DO PRIVATE (COUNT, RES)
    do count=1, 100
      res  => arg%fld_ptr(1:10)
    end do
  end subroutine sub
end program main
