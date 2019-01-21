! RUN: %flang -c %s 2>&1 | FileCheck %s -allow-empty --check-prefix=CHECK

subroutine add(arr1,arr2,arr3,N)
  integer :: i,N
  integer :: arr1(N)
  integer :: arr2(N)
  integer :: arr3(N)

  !dir$ vector always
  do i = 1, N
    arr3(i) = arr1(i) - arr2(i)
  end do
end subroutine
! CHECK-NOT: F90-S-0602
! CHECK-NOT: F90-S-0603
