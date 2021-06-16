! RUN: %flang -fopenmp -O2 -S -emit-llvm %s -o - | FileCheck %s
! RUN: %flang -fopenmp -S -emit-llvm %s -o - | FileCheck %s -check-prefix=METADATA

subroutine sum(myarr1,myarr2,ub)
  integer, pointer :: myarr1(:)
  integer, pointer :: myarr2(:)
  integer :: ub

  !$omp simd
  do i=1,ub
    myarr1(i) = myarr1(i)+myarr2(i)
  end do
end subroutine

! CHECK:  {{.*}} add nsw <[[VF:[0-9]+]] x i32>{{.*}}
! METADATA: load {{.*}}, !llvm.mem.parallel_loop_access ![[TAG1:[0-9]+]]
! METADATA: store {{.*}}, !llvm.mem.parallel_loop_access ![[TAG1]]
! METADATA: ![[TAG2:[0-9]+]] = !{!"llvm.loop.vectorize.enable", i1 true}
! METADATA: ![[TAG1:[0-9]+]] = distinct !{![[TAG1]], ![[TAG2]]}
