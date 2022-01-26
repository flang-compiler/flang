! RUN: %flang -O0 -c %s 2>&1 | FileCheck %s --check-prefix=CHECK-WRONG-CLAUSE
subroutine func1(a, b, m)
  integer :: i, m, a(m), b(m)
  !dir$ vector vectorlength(garbage)
  do i = 1, m
    b(i) = a(i) + 1
  end do
end subroutine func1
! CHECK-WRONG-CLAUSE: 90-W-0803-Unsupported clause specified for the vector vectorlength directive. Only fixed|scalable|num is supported
