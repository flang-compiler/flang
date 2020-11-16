!RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s

!Verify the function return type is not of pointer type
!CHECK: !DIBasicType(name: "real"
!CHECK-NOT: !DIDerivedType(tag: DW_TAG_pointer_type

function square(x)
  real, intent(in) :: x
  real :: square
  square = x * x
end function
program main
  real :: a, b, square
  a = 2.0
  b = square(a)
end program main
