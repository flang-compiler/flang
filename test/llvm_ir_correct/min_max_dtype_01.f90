!
! Part of the LLVM Project, under the Apache License v2.0 with LLVM
! Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
!
! Test that MIN/MAX intrinsic have different kind type.
!
! RUN: %flang -emit-llvm -S %s -o %t
! RUN: cat %t | FileCheck %s -check-prefix=CHECK-DTYPE

! CHECK-DTYPE: fpext float {{%.*}} to double
program test
  real(kind = 4) :: r4 = 2.0
  real(kind = 8) :: r8 = 1.0
  real(kind = 8) :: res
  res = min(r4, r8)
end

