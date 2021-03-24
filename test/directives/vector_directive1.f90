! RUN: %flang -S -emit-llvm %s 2>&1 -o - | FileCheck %s --check-prefix=CHECK-NO-CLAUSE

subroutine add(arr1,arr2,arr3,N)
  integer :: i,N
  integer :: arr1(N)
  integer :: arr2(N)
  integer :: arr3(N)

  !dir$ vector
  do i = 1, N
    arr3(i) = arr1(i) - arr2(i)
  end do
end subroutine
! CHECK-NO-CLAUSE-NOT: F90-S-0602
! CHECK-NO-CLAUSE-NOT: F90-S-0603.
