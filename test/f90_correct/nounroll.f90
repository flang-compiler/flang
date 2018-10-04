
!** Test the NOUNROLL pragma
! RUN: %flang -S -emit-llvm %s -o - | FileCheck %s -check-prefix=METADATA
! RUN: %flang -Hx,59,2 -S -emit-llvm %s -o - | FileCheck %s -check-prefix=ENABLE-METADATA
! RUN: %flang -S -emit-llvm %s -o - | FileCheck %s
 program tz
      integer :: i
      integer :: acc(100)
      integer :: sz
      !dir$ nounroll
      do i=1,sz
            acc(i) = i
        end do
       print *, acc(100)
 end program
! METADATA: !"llvm.loop.unroll.disable"
! ENABLE-METADATA-NOT: !"llvm.loop.unroll.disable
! CHECK: [[BRNCH:L.LB[0-9]_[0-9]+]]:{{[' ',\t]+}}; preds = %[[BRNCH]], %L.LB
! CHECK: br i1 {{.*}}, label %[[BRNCH]], label %L.LB
