
!** Test checking no pragma
! RUN: %flang -S -emit-llvm %s -o - | FileCheck %s
! RUN: %flang -S -emit-llvm -O2 %s -o - | FileCheck %s -check-prefix=CHECK2
 program tz
      integer :: i

      integer ::acc(10000)
       do i=1,10000
            acc(i) = i
        end do
       print *, acc(1000)
 end program

! CHECK: [[BRNCH:L.LB[0-9]_[0-9]+]]:{{[' ',\t]+}}; preds = %[[BRNCH]], %L.LB
! CHECK: br i1 {{.*}}, label %[[BRNCH]], label %L.LB
! CHECK-NOT: !llvm.loop ![0-9]
! CHECK2: vector.body:{{[' ',\t]+}}; preds = %vector.body, %L.
! CHECK2: br i1 {{.*}}, label %vector.body, !llvm.loop
