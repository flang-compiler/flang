PROGRAM stress_test_nested_implied_do
  ! Test that a nested implied do loop can be used to initialize an array.
  ! Array constructors, and implicit DO loops are detailed in sec. 7.8
  ! of the Standard.
  IMPLICIT NONE
  INTEGER, PARAMETER :: n=4
  INTEGER, PARAMETER :: m=10
  INTEGER, PARAMETER :: o=2
  INTEGER, PARAMETER :: p=3
  INTEGER :: i, j, k, l

  ! 1 level of nesting
  INTEGER :: array1(n) = (/ (i, i=1, n) /)
  INTEGER :: expect1(n)
  data expect1/1, 2, 3, 4/

  ! 2 levels of nesting
  INTEGER :: array2(m) = (/ ((i, j=1, i), i=1, n ) /)
  INTEGER :: expect2(m)
  data expect2/1, 2, 2, 3, 3, 3, 4, 4, 4, 4/

  ! 3 levels of nesting
  INTEGER :: array3(m*o) = (/ (((i, j=1, i), i=1,n ), k=1, o) /)
  INTEGER :: expect3(m*o)
  data expect3/1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 1, 2, 2, 3, 3, 3, 4, 4, 4, 4/

  ! 4 levels of nesting
  INTEGER :: array4(m*o*p) = (/ ((((i, j=1, i), i=1,n ), k=1, o), l=1, p) /)
  INTEGER :: expect4(m*o*p)
  data expect4/1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 1, 2, 2, 3, 3, 3, 4, 4, 4, 4,&
               1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 1, 2, 2, 3, 3, 3, 4, 4, 4, 4,&
               1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 1, 2, 2, 3, 3, 3, 4, 4, 4, 4/

  ! 3 levels of nesting and multiple items
  INTEGER :: array5(3*m*o) = (/ (((i, i+10, i+100, j=1, i), i=1,n ), k=1, o) /)
  INTEGER :: expect5(3*m*o)
  data expect5/1, 10, 100,&
               2, 20, 200, 2, 20, 200,&
               3, 30, 300, 3, 30, 300, 3, 30, 300,&
               4, 40, 400, 4, 40, 400, 4, 40, 400, 4, 40, 400,&
               1, 10, 100,&
               2, 20, 200, 2, 20, 200,&
               3, 30, 300, 3, 30, 300, 3, 30, 300,&
               4, 40, 400, 4, 40, 400, 4, 40, 400, 4, 40, 400/

  call check(array1, expect1, n)
  call check(array2, expect2, m)
  call check(array3, expect3, m*o)
  call check(array4, expect4, m*o*p)
  call check(array5, expect5, 3*m*o)

END PROGRAM stress_test_nested_implied_do
