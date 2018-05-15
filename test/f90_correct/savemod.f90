! RUN: %flang %s -S -emit-llvm -o - | FileCheck %s

module somemod
  integer, allocatable :: something(:)
end module somemod

! CHECK: %struct.BSS1
! CHECK: @.BSS1 = internal global %struct.BSS1 zeroinitializer
