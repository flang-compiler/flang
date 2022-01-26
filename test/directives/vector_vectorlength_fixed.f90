! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! Check that the vector vectorlength(fixed) directive generates the correct metadata.
! RUN: %flang -O0 -S -emit-llvm %s -o - | FileCheck %s --check-prefixes=CHECK-00,CHECK-ALL

! Check that LLVM vectorizes the loop automatically at -O2.
! RUN: %flang -O2 -S -emit-llvm %s -o - | FileCheck %s -check-prefixes=CHECK-O2,CHECK-ALL

! Check that "-Hx,59,2" disables vector directive.
! RUN: %flang -Hx,59,2 -S -emit-llvm %s -o - | FileCheck %s --check-prefixes=CHECK-DISABLED,CHECK-ALL

subroutine func1(a, b, m)
! CHECK-ALL: define void @func1
  integer :: i, m, a(m), b(m)

  !dir$ vector vectorlength(fixed)
  do i = 1, m
    b(i) = a(i) + 1
  end do
! CHECK-00:      [[LOOP:L.LB[0-9]_[0-9]+]]:{{[' ',\t]+}}; preds = %[[LOOP]], %L.LB
! CHECK-00:      store i32
! CHECK-00:      br i1 {{.*}}, label %[[LOOP]], label %L.LB
! CHECK-O2:      vector.body:{{[ \t]+}}; preds = %vector.body,
! CHECK-O2:      br i1 {{.*}}, label {{.*}}
end subroutine func1

subroutine func2(a, b, m)
! CHECK-ALL: define void @func2
  integer :: i, m, a(m), b(m)
  !dir$ vector vectorlength(2,fixed)
  do i = 1, m
    b(i) = a(i) + 1
  end do
! CHECK-00:      [[LOOP:L.LB2_[0-9]+]]:{{[' ',\t]+}}; preds = %[[LOOP]], %L.LB
! CHECK-00:      store i32
! CHECK-00:      br i1 {{.*}}, label %[[LOOP]], label %L.LB
! CHECK-O2:      vector.body:{{[ \t]+}}; preds = %vector.body,
! CHECK-O2:      br i1 {{.*}}, label {{.*}}
end subroutine func2

! CHECK-DISABLED-NOT: !"llvm.loop.vectorize.width"
! CHECK-DISABLED-NOT: !"llvm.loop.vectorize.scalable.enable", i1 false
! CHECK-DISABLED-NOT: !"llvm.loop.vectorize.enable", i1 true

! CHECK-00-NOT:  !"llvm.loop.vectorize.width"
! CHECK-00:      !"llvm.loop.vectorize.enable", i1 true
! CHECK-00:      !"llvm.loop.vectorize.scalable.enable", i1 false
! CHECK-02-NOT:  !"llvm.loop.vectorize.width"
! CHECK-02:      !"llvm.loop.vectorize.scalable.enable", i1 false
! CHECK-02:      !"llvm.loop.vectorize.enable", i1 true
! CHECK-O2:      !{!"llvm.loop.isvectorized", i32 1}
! CHECK-00:  !"llvm.loop.vectorize.width", i32 2
! CHECK-02:  !"llvm.loop.vectorize.width", i32 2
