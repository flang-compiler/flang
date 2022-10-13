! RUN: %flang -S -emit-llvm %s -o - | FileCheck %s

subroutine test()
  integer(kind=1) :: pos
  character(len=10) :: arg

  pos = 1_1

  call getarg(pos, arg)
end subroutine
! CHECK: bitcast ptr @f90_getarga to ptr,
! CHECK-NOT: bitcast ptr @getarg_ to ptr,
