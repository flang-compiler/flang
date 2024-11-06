! This test contians two files, tbaa_multimod_02.f90 and Inputs/tbaa_multimod_02_input.f90
! RUN: %flang -emit-llvm -c -O3 %s -o %t.bc
! RUN: %flang -emit-llvm -c -O3 %S/Inputs/tbaa_multimod_02_input.f90 -o %t2.bc
! RUN: llvm-link -o %t3.bc %t.bc %t2.bc
! RUN: opt -aa-trace -O3 -o - %t3.bc 2>&1 | FileCheck %s 
! CHECK-NOT: End ptr getelementptr inbounds (%struct.BSS1, ptr @.BSS1, i64 0, i32 0, i64 16) @ LocationSize::precise(4), ptr inttoptr (i64 56 to ptr) @ LocationSize::precise(8) = NoAlias

program main
  implicit none
  integer, parameter :: n = 5
  real :: arr(n)
  integer :: i
  arr = 1
  call to_load(arr)
  arr(0) = 4
  call to_load(arr)
end program main
