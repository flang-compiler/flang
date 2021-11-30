!** Test checking attributes are set correctly
! RUN: %flang -S -emit-llvm -target aarch64-linux-gnu -march=armv8-a %s -o - | FileCheck %s -check-prefix=ATTRS1
! RUN: %flang -S -emit-llvm -target aarch64-linux-gnu -march=armv8-a+sve %s -o - | FileCheck %s -check-prefix=ATTRS2
! RUN: %flang -S -emit-llvm -target aarch64-linux-gnu -march=armv8-a+sve2 %s -o - | FileCheck %s -check-prefix=ATTRS3
! RUN: %flang -S -emit-llvm -target aarch64-linux-gnu -march=armv8-a+sve2-sha3 %s -o - | FileCheck %s -check-prefix=ATTRS4
! RUN: %flang -S -emit-llvm -target aarch64-linux-gnu -march=armv8-a+sve+nosve %s -o - | FileCheck %s -check-prefix=ATTRS5
      program tz
       integer :: i
       integer ::acc(100)
       do i=1,100
            acc(i) = 5
       end do
      print *, acc(100)
      end program
! ATTRS1: attributes{{.*}}"target-features"="+neon"
! ATTRS2: attributes{{.*}}
! ATTRS2-DAG:"target-features"="+neon,+sve"
! ATTRS2-DAG: "vscale-range"="1,16"
! ATTRS3: attributes{{.*}}
! ATTRS3-DAG:"target-features"="+neon,+sve2"
! ATTRS3-DAG: "vscale-range"="1,16"
! ATTRS4: attributes{{.*}}
! ATTRS4-DAG:"target-features"="+neon,+sve2-sha3"
! ATTRS4-DAG: "vscale-range"="1,16"
! ATTRS5: attributes{{.*}}"target-features"="+neon,-sve"
