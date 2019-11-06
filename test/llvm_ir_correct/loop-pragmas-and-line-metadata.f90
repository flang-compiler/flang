! RUN: %flang -g -fopenmp -O2 -emit-flang-llvm %s -o /dev/stdout \
! RUN:   | FileCheck --check-prefixes=CHECK,CHECK-LINE %s

! RUN: %flang -g -fopenmp -O2 -emit-flang-llvm %s -o /dev/stdout \
! RUN:   | FileCheck --check-prefixes=CHECK,CHECK-DIRS %s

! Test that loop metadata is correct for a variety of loops.
!
! This file tests two properties of the metadata:
!
! * CHECK -LINE: Check the line number metadata associated with the loop refers
!               to the correct source lines.
! * CHECK -DIRS: Check that directives associated with the loop are the correct ones.
!
! This is done as two separate runs to avoid being sensitive to the order of
! metadata nodes within the loop ID.
!
! The strategy is to locate the DILocation nodes for the start and end of the
! loop using @LINE, then locate the !llvm.loop node on a branch, then check
! that the contents of the loop ID are as expected.
!
! The test also enforces that the expected number of !llvm.loop directives are
! present.

! CHECK: test_loop_line_md
subroutine test_loop_line_md(x, N)
  integer, intent(out) :: x

  ! CHECK-DIRS-DAG: ![[VEC_ON:[0-9]+]] = !{ !"llvm.loop.vectorize.enable", i1 1 }
  ! CHECK-DIRS-DAG: ![[VEC_OFF:[0-9]+]] = !{ !"llvm.loop.vectorize.enable", i1 0 }
  ! CHECK-DIRS-DAG: ![[UNROLL_ON:[0-9]+]] = !{ !"llvm.loop.unroll.enable" }
  ! CHECK-DIRS-DAG: ![[UNROLL_OFF:[0-9]+]] = !{ !"llvm.loop.unroll.disable" }

  !dir$ novector
  do i = 1, N
  ! CHECK-LINE-DAG: ![[L1Start:[0-9]+]] = !DILocation(line: [[@LINE-1]],
  ! CHECK-LINE-DAG: ![[L1End:[0-9]+]] = !DILocation(line: [[@LINE+7]],
    x = x + i + 1
  ! NOTE: The first one of these is redundant.
  ! CHECK-DAG: br {{.*}}, !llvm.loop !{{[0-9]+}}
  ! CHECK-DAG: br {{.*}}, !llvm.loop ![[L1:[0-9]+]]
  ! CHECK-LINE-DAG: ![[L1]] = !{ ![[L1]],{{.*}}![[L1Start]], ![[L1End]]
  ! CHECK-DIRS-DAG: ![[L1]] = !{ ![[L1]],{{.*}}![[VEC_OFF]]{{[, ]}}
  end do

  !dir$ vector always
  do i = 1, Nq
  ! CHECK-LINE-DAG: ![[L2Start:[0-9]+]] = !DILocation(line: [[@LINE-1]],
  ! CHECK-LINE-DAG: ![[L2End:[0-9]+]] = !DILocation(line: [[@LINE+7]],
    x = x + i + 1
  ! NOTE: The first one of these is redundant.
  ! CHECK-DAG: br {{.*}}, !llvm.loop !{{[0-9]+}}
  ! CHECK-DAG: br {{.*}}, !llvm.loop ![[L2:[0-9]+]]
  ! CHECK-LINE-DAG: ![[L2]] = !{ ![[L2]],{{.*}}![[L2Start]], ![[L2End]]
  ! CHECK-DIRS-DAG: ![[L2]] = !{ ![[L2]],{{.*}}![[VEC_ON]]{{[, ]}}
  end do

  !dir$ nounroll
  do i = 1, N
  ! CHECK-LINE-DAG: ![[L3Start:[0-9]+]] = !DILocation(line: [[@LINE-1]],
  ! CHECK-LINE-DAG: ![[L3End:[0-9]+]] = !DILocation(line: [[@LINE+7]],
    x = x + i + 1
  ! NOTE: The first one of these is redundant.
  ! CHECK-DAG: br {{.*}}, !llvm.loop !{{[0-9]+}}
  ! CHECK-DAG: br {{.*}}, !llvm.loop ![[L3:[0-9]+]]
  ! CHECK-LINE-DAG: ![[L3]] = !{ ![[L3]],{{.*}}![[L3Start]], ![[L3End]],
  ! CHECK-DIRS-DAG: ![[L3]] = !{ ![[L3]],{{.*}}![[UNROLL_OFF]]{{[, ]}}
  end do

  !dir$ unroll
  do i = 1, N
  ! CHECK-LINE-DAG: ![[L4Start:[0-9]+]] = !DILocation(line: [[@LINE-1]],
  ! CHECK-LINE-DAG: ![[L4End:[0-9]+]] = !DILocation(line: [[@LINE+7]],
    x = x + i + 1
  ! NOTE: The first one of these is redundant.
  ! CHECK-DAG: br {{.*}}, !llvm.loop !{{[0-9]+}}
  ! CHECK-DAG: br {{.*}}, !llvm.loop ![[L4:[0-9]+]]
  ! CHECK-LINE-DAG: ![[L4]] = !{ ![[L4]],{{.*}}![[L4Start]], ![[L4End]],
  ! CHECK-DIRS-DAG: ![[L4]] = !{ ![[L4]],{{.*}}![[UNROLL_ON]]{{[, ]}}
  end do

  !dir$ nounroll
  do i = 1, N
  ! CHECK-LINE-DAG: ![[L5Start:[0-9]+]] = !DILocation(line: [[@LINE-1]],
  ! CHECK-LINE-DAG: ![[L5End:[0-9]+]] = !DILocation(line: [[@LINE+7]],
    x = x + i + 1
  ! NOTE: The first one of these is redundant.
  ! CHECK-DAG: br {{.*}}, !llvm.loop !{{[0-9]+}}
  ! CHECK-DAG: br {{.*}}, !llvm.loop ![[L5:[0-9]+]]
  ! CHECK-LINE-DAG: ![[L5]] = !{ ![[L5]],{{.*}}![[L5Start]], ![[L5End]],
  ! CHECK-DIRS-DAG: ![[L5]] = !{ ![[L5]],{{.*}}![[UNROLL_OFF]]{{[, ]}}
  end do

  !dir$ unroll
  do i = 1, N
  ! CHECK-LINE-DAG: ![[L5Start:[0-9]+]] = !DILocation(line: [[@LINE-1]],
  ! CHECK-LINE-DAG: ![[L5End:[0-9]+]] = !DILocation(line: [[@LINE+7]],
    x = x + i + 1
  ! NOTE: The first one of these is redundant.
  ! CHECK-DAG: br {{.*}}, !llvm.loop !{{[0-9]+}}
  ! CHECK-DAG: br {{.*}}, !llvm.loop ![[L5:[0-9]+]]
  ! CHECK-LINE-DAG: ![[L5]] = !{ ![[L5]],{{.*}}![[L5Start]], ![[L5End]],
  ! CHECK-DIRS-DAG: ![[L5]] = !{ ![[L5]],{{.*}}![[UNROLL_ON]]{{[, ]}}
  end do

  !$omp simd
  do i = 1, N
  ! CHECK-LINE-DAG: ![[L7Start:[0-9]+]] = !DILocation(line: [[@LINE-1]],
  ! CHECK-LINE-DAG: ![[L7End:[0-9]+]] = !DILocation(line: [[@LINE+7]],
    x = x + i + 1
  ! NOTE: The first one of these is redundant.
  ! CHECK-DAG: br {{.*}}, !llvm.loop !{{[0-9]+}}
  ! CHECK-DAG: br {{.*}}, !llvm.loop ![[L7:[0-9]+]]
  ! CHECK-LINE-DAG: ![[L7]] = !{ ![[L7]],{{.*}}![[L7Start]], ![[L7End]]
  ! CHECK-DIRS-DAG: ![[L7]] = !{ ![[L7]],{{.*}}![[VEC_ON]]{{[, ]}}
  end do

  !dir$ novector
  do i = 1, N
  ! CHECK-LINE-DAG: ![[L8Start:[0-9]+]] = !DILocation(line: [[@LINE-1]],
  ! CHECK-LINE-DAG: ![[L8End:[0-9]+]] = !DILocation(line: [[@LINE+7]],
    x = x + i + 1
  ! NOTE: This test is missing the redundant llvm.loop instruction because the
  ! previous one uses the !$omp simd directive. This is weird.
  ! CHECK-DAG: br {{.*}}, !llvm.loop ![[L8:[0-9]+]]
  ! CHECK-LINE-DAG: ![[L8]] = !{ ![[L8]],{{.*}}![[L8Start]], ![[L8End]]
  ! CHECK-DIRS-DAG: ![[L8]] = !{ ![[L8]],{{.*}}![[VEC_OFF]]{{[, ]}}
  end do

  !dir$ vector always
  do i = 1, N
  ! CHECK-LINE-DAG: ![[L9Start:[0-9]+]] = !DILocation(line: [[@LINE-1]],
  ! CHECK-LINE-DAG: ![[L9End:[0-9]+]] = !DILocation(line: [[@LINE+7]],
    x = x + i + 1
  ! NOTE: The first one of these is redundant.
  ! CHECK-DAG: br {{.*}}, !llvm.loop !{{[0-9]+}}
  ! CHECK-DAG: br {{.*}}, !llvm.loop ![[L9:[0-9]+]]
  ! CHECK-LINE-DAG: ![[L9]] = !{ ![[L9]],{{.*}}![[L9Start]], ![[L9End]]
  ! CHECK-DIRS-DAG: ![[L9]] = !{ ![[L9]],{{.*}}![[VEC_ON]]{{[, ]}}
  end do

  ! There should be no further loop metadata.
  ! CHECK-NOT: !llvm.loop
end subroutine
