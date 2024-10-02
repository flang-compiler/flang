! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! REQUIRES: llvm-19
! RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s

! CHECK: #dbg_declare(ptr %rvar_{{[0-9]+}}, [[RESULT:![0-9]+]], !DIExpression()
! CHECK: [[RESULT]] = !DILocalVariable(name: "rvar"

function func(arg) result(rvar)
  integer, intent(in) :: arg ! input
  integer :: rvar ! output
  rvar = arg + 2
end function func
