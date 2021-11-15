! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! Check that the vectorize_width(int) directive generates the correct metadata.
!
! RUN: %flang -S -emit-llvm %s -o - | FileCheck %s --check-prefix=CHECK
!
! CHECK:      [[LOOP:L.LB[0-9]_[0-9]+]]:{{[' ',\t]+}}; preds = %[[LOOP]], %L.LB
! CHECK:      store i32
! CHECK:      br i1 {{.*}}, label %[[LOOP]], label %L.LB
! CHECK:      !"llvm.loop.vectorize.width", i32 2}
! CHECK:      !"llvm.loop.vectorize.scalable.enable", i1 false
! CHECK:      !"llvm.loop.vectorize.enable", i1 true

! Check that LLVM vectorizes the loop automatically at -O2.
! RUN: %flang -O2 -S -emit-llvm %s -o - | FileCheck %s -check-prefix=CHECK-O2
! CHECK-O2:      vector.body:{{[ \t]+}}; preds = %vector.body,
! CHECK-02:      !"llvm.loop.vectorize.width", i32 2}
! CHECK-02:      !"llvm.loop.vectorize.scalable.enable", i1 false
! CHECK-02:      !"llvm.loop.vectorize.enable", i1 true
! CHECK-O2:      !{!"llvm.loop.isvectorized", i32 1}
subroutine func1(a, b, m)
  integer :: i, m, a(m), b(m)
  !dir$ vectorize_width(2)
  do i = 1, m
    b(i) = a(i) + 1
  end do
end subroutine func1
