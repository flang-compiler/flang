! RUN: %flang -S -emit-llvm %s -o - | FileCheck %s

module nan_module
  implicit none
contains
  elemental real function test_isnan(y)
    real, intent(in) :: y
    test_isnan= isnan(y)
  end function test_isnan
end module nan_module

program test_gnu_nan2
  use nan_module
  implicit none
  real x
  x = sqrt(-1.0)
  print *, "Is NaN? : ", test_isnan(x)

end program test_gnu_nan2

! CHECK: call i32 @gnu_extensions_isnan_
