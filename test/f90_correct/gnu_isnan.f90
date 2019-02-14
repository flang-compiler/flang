! RUN: %flang -S -emit-llvm %s -o - | FileCheck %s

program test_nan
  use ieee_arithmetic
  implicit none
  real :: x2
  x2 = sqrt(-1.0)
  if (ieee_is_nan(x2)) print *,'"x2" is a NaN'
  if (isnan(x2)) print *, '"x2" is a NaN'
end program test_nan

! CHECK: ieee_arithmetic_ieee_is_nanr4_
! CHECK: call i32 @gnu_extensions_isnan_
