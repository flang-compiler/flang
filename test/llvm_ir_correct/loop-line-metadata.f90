! RUN: %flang -g -O2 -emit-flang-llvm %s -o /dev/stdout | FileCheck %s

! Test that with debug metadata enabled, in the absence of loop pragmas, loops
! get annotated with their start and end line numbers.

! CHECK: test_loop_line_md
subroutine test_loop_line_md(x, N)
  integer, intent(out) :: x

  do i = 1, N
  ! CHECK-DAG: ![[L1Start:[0-9]+]] = !DILocation(line: [[@LINE-1]],
  ! CHECK-DAG: ![[L1End:[0-9]+]] = !DILocation(line: [[@LINE+4]],
    x = x + i + 1
  ! CHECK-DAG: br {{.*}}, !llvm.loop ![[L1:[0-9]+]]
  ! CHECK-DAG: ![[L1]] = !{ ![[L1]],{{.*}}![[L1Start]], ![[L1End]]
  end do

  do i = 1, N
  ! CHECK-DAG: ![[L2Start:[0-9]+]] = !DILocation(line: [[@LINE-1]],
  ! CHECK-DAG: ![[L2End:[0-9]+]] = !DILocation(line: [[@LINE+11]],

    do j = 1, N
    ! CHECK-DAG: ![[L3Start:[0-9]+]] = !DILocation(line: [[@LINE-1]],
    ! CHECK-DAG: ![[L3End:[0-9]+]] = !DILocation(line: [[@LINE+4]],
      x = x + j + i + 1
    ! CHECK-DAG: br {{.*}}, !llvm.loop ![[L3:[0-9]+]]
    ! CHECK-DAG: ![[L3]] = !{ ![[L3]],{{.*}}![[L3Start]], ![[L3End]]
    end do
  ! CHECK-DAG: br {{.*}}, !llvm.loop ![[L2:[0-9]+]]
  ! CHECK-DAG: ![[L2]] = !{ ![[L2]],{{.*}}![[L2Start]], ![[L2End]]
  end do

  ! There should be no further loop metadata.
  ! CHECK-NOT: !llvm.loop
end subroutine

! CHECK: test_implicit_loop_intrinsic
subroutine test_implicit_loop_intrinsic(x, arr)
  integer :: arr(:)
  integer, intent(out) :: x
  x = sum(arr)
! NOTE: You might expect this to have an llvm.loop, but it does not at the
! moment, because the loop does not get marked with BIH_HEAD.
! CHECK-NOT: !llvm.loop
end subroutine

! CHECK: test_implicit_loop_array_op
subroutine test_implicit_loop_array_op(arr)
  integer :: arr(:)
  arr = 3
! NOTE: You might expect this to have an llvm.loop, but it does not at the
! moment, because the loop does not get marked with BIH_HEAD.
! CHECK-NOT: !llvm.loop
end subroutine
