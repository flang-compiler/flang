! RUN: %flang -c %s 2>&1 | FileCheck %s --check-prefix=CHECK-NO-CLAUSE

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
! CHECK-NO-CLAUSE: F90-W-0602-No clause specified for the vector directive. Note: Only the always clause is supported.
