! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! REQUIRES: llvm-19
! RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s

! Check that non-debug instructions should not have debug location.

! CHECK: define void @show_
! CHECK:      #dbg_declare(ptr %message, {{.*}}, [[DLOC:![0-9]+]])
! CHECK-NEXT: #dbg_value(ptr %array, {{.*}}, [[DLOC]])
! CHECK-NEXT: #dbg_declare(ptr %"array$sd", {{.*}}, [[DLOC]])
! CHECK-NOT: getelementptr {{.*}}, !dbg
! CHECK: store i64 {{%[0-9]+}}, ptr %z_b_3_{{[0-9]+}}, align 8
! CHECK: br label
! CHECK: ret void, !dbg {{![0-9]+}}
subroutine show (message, array)
  character (len=*) :: message
  integer :: array(:)

  print *, message
  print *, array
end subroutine show

! CHECK: define void @MAIN_
! CHECK: #dbg_declare(ptr %"array$sd{{.*}}, [[DLOC:![0-9]+]]
! CHECK-NOT: br label {{.*}}, !dbg
! CHECK: ret void, !dbg
program prolog
  interface
    subroutine show (message, array)
      character (len=*) :: message
      integer :: array(:)
    end subroutine show
  end interface

  integer :: array(10) = (/1,2,3,4,5,6,7,8,9,10/)

  call show ("array", array)
end program prolog
