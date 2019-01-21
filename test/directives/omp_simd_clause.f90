! RUN: %flang -fopenmp -O2 -S -emit-llvm %s -o - | FileCheck %s
! RUN: %flang -fopenmp -S -emit-llvm %s -o - | FileCheck %s -check-prefix=METADATA
! RUN: %flang -fopenmp -O2 -c %s 2>&1 | FileCheck %s -check-prefix=WARNING

subroutine sum(myarr1,myarr2,ub)
  integer, pointer :: myarr1(:)
  integer, pointer :: myarr2(:)
  integer :: ub

  !$omp simd collapse(2)
  do i=1,ub
    myarr1(i) = myarr1(i)+myarr2(i)
  end do
end subroutine

! CHECK-NOT:  {{.*}} add nsw <[[VF:[0-9]+]] x i32>{{.*}}
! METADATA-NOT: load {{.*}}, !llvm.mem.parallel_loop_access ![[TAG1:[0-9]+]]
! METADATA-NOT: store {{.*}}, !llvm.mem.parallel_loop_access ![[TAG1]]
! METADATA-NOT: ![[TAG2:[0-9]+]] = !{!"llvm.loop.vectorize.enable", i1 true}
! METADATA-NOT: ![[TAG1:[0-9]+]] = distinct !{![[TAG1]], ![[TAG2]]}
! WARNING: F90-W-0604-Unsupported clause specified for the omp simd directive
