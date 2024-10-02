! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! REQUIRES: llvm-19
! RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s

! CHECK: #dbg_value(i64 99, [[SPAR:![0-9]+]], !DIExpression()
! CHECK: distinct !DIGlobalVariable(name: "apar"
! CHECK: [[SPAR]] = !DILocalVariable(name: "spar"

program main
  integer (kind=8) :: svar
  integer (kind=8) :: avar(5)
  integer (kind=8), parameter :: spar = 99
  integer (kind=8), parameter :: apar(5) = (/99, 98, 97, 96, 95/)
  svar = spar
  avar = apar

  print *, svar, avar, spar, apar

end program main
