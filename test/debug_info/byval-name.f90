! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! REQUIRES: llvm-19
! RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s

! Verify that the IR contains "_V_" (Flang's naming convention for byval
! parameters) and that the operation using them are well-formed.

! CHECK: sub_(i32 [[PREFIXED_ARG_NAME:%_V_arg_abc.arg]])
! CHECK: [[PREFIXED_LOCAL_NAME:%_V_arg_abc.addr]] = alloca i32, align 4
! CHECK: #dbg_declare(ptr [[PREFIXED_LOCAL_NAME]]
! CHECK: store i32 [[PREFIXED_ARG_NAME]], ptr [[PREFIXED_LOCAL_NAME]], align 4

! Verify that the names in the DebugInfo metadata have dropped the "_V_" prefix.

! CHECK-NOT: DILocalVariable(name: "_V_arg_abc"
! CHECK-COUNT-2: DILocalVariable(name: "arg_abc"
! CHECK-NOT: DILocalVariable(name: "_V_arg_abc"

subroutine sub(arg_abc)
    integer, value :: arg_abc
    integer :: abc_local
    abc_local = arg_abc
    print *, arg_abc
end subroutine
