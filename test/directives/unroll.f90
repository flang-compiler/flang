!
! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
!

!** Test checking unroll pragma
! RUN: %flang -S -emit-llvm %s -o - | FileCheck %s -check-prefix=METADATA
! RUN: %flang -Hx,59,2 -S -emit-llvm %s -o - | FileCheck %s -check-prefix=ENABLE-METADATA
! RUN: %flang -S -emit-llvm %s -o - | FileCheck %s
program tz
  integer :: i
  integer ::acc(100)
  !dir$ unroll
  do i=1,100
    acc(i) = 5
  end do
  print *, acc(100)
end program
! METADATA: !"llvm.loop.unroll.enable"
! ENABLE-METADATA-NOT: !"llvm.loop.unroll.enable"
! CHECK: [[BRNCH:L.LB[0-9]_[0-9]+]]:{{[' ',\t]+}}; preds = %[[BRNCH]], %L.LB
! CHECK: br i1 {{.*}}, label %[[BRNCH]], label %L.LB
