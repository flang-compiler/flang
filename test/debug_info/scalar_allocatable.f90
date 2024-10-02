! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! REQUIRES: llvm-19
! RUN: %flang %s -g -S -emit-llvm -o - | FileCheck %s

! Ensure that for an allocatable variable, we're recording the type of
! allocatable variable as DW_TAG_pointer_type.

! CHECK: #dbg_declare(ptr %{{.*}}, ![[DILocalVariable:[0-9]+]], !DIExpression()
! CHECK: ![[DILocalVariable]] = !DILocalVariable(name: "alcvar"
! CHECK-SAME: type: ![[PTRTYPE:[0-9]+]]
! CHECK: ![[PTRTYPE]] = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: ![[TYPE:[0-9]+]]
! CHECK: ![[TYPE]] = !DIBasicType(name: "double precision",{{.*}}

program main
  real(kind=8), allocatable :: alcvar
  allocate(alcvar)
  alcvar = 7.7
  print *, alcvar
end program main
