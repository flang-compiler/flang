! RUN: %flang -S -emit-llvm %s -o - | FileCheck %s

subroutine test()
  integer(kind=1) :: pos
  character(len=10) :: arg

  pos = 1_1

  call getarg(pos, arg)
end subroutine
! CHECK: bitcast void ({{.*}})* @f90_getarga to void (i8*, i8*, i8*, i64{{.*}})*,
! CHECK-NOT: bitcast void ({{.*}})* @getarg_ to void (i8, i8, i64{{.*}})*,
