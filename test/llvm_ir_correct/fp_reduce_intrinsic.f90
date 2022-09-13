! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! REQUIRES: non-ms-sdk
! REQUIRES: llvm-13

! RUN: %flang -O3                             -emit-flang-llvm %s -o /dev/stdout | FileCheck %s  --check-prefixes=CHECK
! RUN: %flang -Ofast                          -emit-flang-llvm %s -o /dev/stdout | FileCheck %s  --check-prefixes=FAST
! RUN: %flang -O3 -Mx,216,0x8                 -emit-flang-llvm %s -o /dev/stdout | FileCheck %s  --check-prefixes=NSZ
! RUN: %flang -O3 -Mx,216,0x10                -emit-flang-llvm %s -o /dev/stdout | FileCheck %s  --check-prefixes=REASSOC
! RUN: %flang -O3 -Mx,216,0x8 -Mx,216,0x10    -emit-flang-llvm %s -o /dev/stdout | FileCheck %s  --check-prefixes=NSZ_REASSOC
! RUN: %flang -Ofast -Mx,216,0x8 -Mx,216,0x10 -emit-flang-llvm %s -o /dev/stdout | FileCheck %s  --check-prefixes=FAST

real function acc(arr1,arr2)
  real :: arr1(:)
  real :: arr2(:)
  acc = SUM(arr1 * arr2)
! CHECK-NOT: {{[[:space:]]}}fast
! CHECK-NOT: {{[[:space:]]}}nsz
! CHECK-NOT: {{[[:space:]]}}reassoc
! FAST-NOT: {{[[:space:]]}}nsz
! FAST-NOT: {{[[:space:]]}}reassoc
! NSZ: {{[[:space:]]}}nsz
! NSZ-NOT: {{[[:space:]]}}fast
! NSZ-NOT: {{[[:space:]]}}reassoc
! REASSOC-NOT: {{[[:space:]]}}fast
! REASSOC-NOT: {{[[:space:]]}}nsz
! REASSOC: {{[[:space:]]}}reassoc
! NSZ_REASSOC: {{[[:space:]]}}nsz reassoc
! NSZ_REASSOC-NOT: {{[[:space:]]}}fast
end function
