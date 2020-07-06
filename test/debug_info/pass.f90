! Simple test to check a passing case.

! RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s
! CHECK: DISubroutine

subroutine foo
end subroutine
