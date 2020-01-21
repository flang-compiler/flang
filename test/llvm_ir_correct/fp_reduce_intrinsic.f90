! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! RUN: %flang -O3                             -emit-flang-llvm %s -o - | FileCheck %s  --check-prefixes=CHECK
! RUN: %flang -Ofast                          -emit-flang-llvm %s -o - | FileCheck %s  --check-prefixes=FAST
! RUN: %flang -O3 -Mx,216,0x8                 -emit-flang-llvm %s -o - | FileCheck %s  --check-prefixes=NSZ
! RUN: %flang -O3 -Mx,216,0x10                -emit-flang-llvm %s -o - | FileCheck %s  --check-prefixes=REASSOC
! RUN: %flang -O3 -Mx,216,0x8 -Mx,216,0x10    -emit-flang-llvm %s -o - | FileCheck %s  --check-prefixes=NSZ_REASSOC
! RUN: %flang -Ofast -Mx,216,0x8 -Mx,216,0x10 -emit-flang-llvm %s -o - | FileCheck %s  --check-prefixes=FAST

real function acc(arr1,arr2)
  real :: arr1(:)
  real :: arr2(:)
  acc = SUM(arr1 * arr2)
! CHECK-NOT: call fast float @llvm.fmuladd.f32
! CHECK-NOT: call nsz float @llvm.fmuladd.f32
! CHECK-NOT: call nsz reassoc float @llvm.fmuladd.f32
! FAST-NOT: call nsz float @llvm.fmuladd.f32
! FAST-NOT: call reassoc float @llvm.fmuladd.f32
! NSZ: call nsz float @llvm.fmuladd.f32
! NSZ-NOT: call fast float @llvm.fmuladd.f32
! NSZ-NOT: call reassoc {{.*}}float @llvm.fmuladd.f32
! REASSOC-NOT: call fast float @llvm.fmuladd.f32
! REASSOC-NOT: call nsz {{.*}}float @llvm.fmuladd.f32
! REASSOC: call reassoc float @llvm.fmuladd.f32
! NSZ_REASSOC: call nsz reassoc float @llvm.fmuladd.f32
! NSZ_REASSOC-NOT: call fast float @llvm.fmuladd.f32
end function
