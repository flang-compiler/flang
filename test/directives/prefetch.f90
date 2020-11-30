! RUN: %flang -O1 -S -emit-llvm %s -o - | FileCheck %s --check-prefix=CHECK-PREFETCH
! RUN: %flang -O1 -S -emit-llvm -Hy,59,0x4 %s -o - | FileCheck %s --check-prefix=CHECK-NOPREFETCH

subroutine prefetch_dir(a1, a2)
  integer :: a1(4096)
  integer :: a2(4096)

  do i = 128, (4096 - 128)
    !$mem prefetch a1, a2(i + 256)
    a1(i) = a2(i - 127) + a2(i + 127)
  end do
end subroutine prefetch_dir

!! Ensure that the offset generated for the prefetch of a2(i + 256) is correct.
! CHECK-PREFETCH: [[a1:%[0-9]+]] = bitcast i64* %a1 to i8*
! CHECK-PREFETCH: [[a2:%[0-9]+]] = bitcast i64* %a2 to i8*
! CHECK-PREFETCH: [[a2base:%[0-9]+]] = getelementptr i8, i8* [[a2]], i64 1020
! CHECK-PREFETCH: call void @llvm.prefetch{{.*}}(i8* [[a1]], i32 0, i32 3, i32 1)
! CHECK-PREFETCH: [[i:%[0-9]+]] = shl nuw nsw i64 %indvars.iv, 2
! CHECK-PREFETCH: [[a2elem:%[0-9]+]] = getelementptr i8, i8* [[a2base]], i64 [[i]]
! CHECK-PREFETCH: call void @llvm.prefetch{{.*}}(i8* [[a2elem]], i32 0, i32 3, i32 1)
! CHECK-PREFETCH: declare void @llvm.prefetch{{.*}}
! CHECK-NOPREFETCH-NOT: @llvm.prefetch
