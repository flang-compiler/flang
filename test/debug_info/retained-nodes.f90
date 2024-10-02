! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! REQUIRES: llvm-19
! RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s

subroutine addf(a, b, c, d, e, f, g, h, i)
  integer :: a, b, c, d, e, f, g, h, i
! CHECK: #dbg_declare(ptr %a, [[A:![0-9]+]]
! CHECK: #dbg_declare(ptr %b, [[B:![0-9]+]]
! CHECK: #dbg_declare(ptr %c, [[C:![0-9]+]]
! CHECK: #dbg_declare(ptr %d, [[D:![0-9]+]]
! CHECK: #dbg_declare(ptr %e, [[E:![0-9]+]]
! CHECK: #dbg_declare(ptr %f, [[F:![0-9]+]]
! CHECK: #dbg_declare(ptr %g, [[G:![0-9]+]]
! CHECK: #dbg_declare(ptr %h, [[H:![0-9]+]]
! CHECK: #dbg_declare(ptr %i, [[I:![0-9]+]]
end subroutine

! CHECK-DAG: [[NODES:![0-9]+]] = !{{{.*}}[[A]], [[B]], [[C]], [[D]], [[E]], [[F]], [[G]], [[H]], [[I]]{{.*}}}
! CHECK-DAG: !{{.*}} = distinct !DISubprogram{{.*}}({{.*}}retainedNodes: [[NODES]]
