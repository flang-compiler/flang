! Simple test to check for a failing case.

! RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s
! CHECK: NULL

subroutine foo
end subroutine
