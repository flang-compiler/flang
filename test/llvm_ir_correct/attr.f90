!** Test checking attributes are set correctly
! RUN: %flang -S -emit-llvm -march=armv8-a %s -o - | FileCheck %s -check-prefix=ATTRS1
! RUN: %flang -S -emit-llvm -march=armv8-a+sve %s -o - | FileCheck %s -check-prefix=ATTRS2
      program tz
       integer :: i
       integer ::acc(100)
       do i=1,100
            acc(i) = 5
       end do
      print *, acc(100)
      end program
! ATTRS1: attributes{{.*}}"target-features"="+neon"
! ATTRS2: attributes{{.*}}"target-features"="+neon,+sve"