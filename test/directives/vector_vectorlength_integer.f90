! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! Check that the vectorize_width(int) directive generates the correct metadata.
! RUN: %flang -S -emit-llvm %s -o - | FileCheck %s --check-prefix=CHECK-00

! Check that LLVM vectorizes the loop automatically at -O2.
! RUN: %flang -O2 -S -emit-llvm %s -o - | FileCheck %s -check-prefix=CHECK-O2

! Check that "-Hx,59,2" disables both kinds of vector directives.
! RUN: %flang -Hx,59,2 -S -emit-llvm %s -o - | FileCheck %s --check-prefixes=CHECK-DISABLED

subroutine func1(a, b, m)
! CHECK-00-LABEL: define void @func1
  integer :: i, m, a(m), b(m)
  !dir$ vector vectorlength(2)
  do i = 1, m
    b(i) = a(i) + 1
  end do
! CHECK-00:      [[LOOP:L.LB1_[0-9]+]]:{{[' ',\t]+}}; preds = %[[LOOP]], %L.LB
! CHECK-00:      store i32
! CHECK-00:      br i1 {{.*}}, label %[[LOOP]], label %L.LB
! CHECK-O2:      vector.body:{{[ \t]+}}; preds = %vector.body,
end subroutine func1

subroutine func2(a, b, m)
! CHECK-00-LABEL: define void @func2
  integer :: i, m, a(m), b(m)
  !dir$ vector vectorlength(2,4,16)
  do i = 1, m
    b(i) = a(i) + 1
  end do
! CHECK-00:      [[LOOP:L.LB2_[0-9]+]]:{{[' ',\t]+}}; preds = %[[LOOP]], %L.LB
! CHECK-00:      store i32
! CHECK-00:      br i1 {{.*}}, label %[[LOOP]], label %L.LB
! CHECK-O2:      vector.body:{{[ \t]+}}; preds = %vector.body,
end subroutine func2

! CHECK-DISABLED-NOT: !"llvm.loop.vectorize.width", i32 2}
! CHECK-DISABLED-NOT: !"llvm.loop.vectorize.scalable.enable", i1 false
! CHECK-DISABLED-NOT: !"llvm.loop.vectorize.enable", i1 true

! CHECK-00:      !"llvm.loop.vectorize.enable", i1 true
! CHECK-00:      !"llvm.loop.vectorize.scalable.enable", i1 false
! CHECK-00:      !"llvm.loop.vectorize.width", i32 2}
! CHECK-02:      !"llvm.loop.vectorize.width", i32 2}
! CHECK-02:      !"llvm.loop.vectorize.scalable.enable", i1 false
! CHECK-02:      !"llvm.loop.vectorize.enable", i1 true
! CHECK-O2:      !{!"llvm.loop.isvectorized", i32 1}
