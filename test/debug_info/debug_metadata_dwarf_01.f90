! RUN: %flang -S -emit-llvm %s -o - | FileCheck %s
! CHECK-NOT: distinct !DICompileUnit({{.*}}, imports: ![[DBG_IMPORTS:[0-9]+]], nameTableKind: None

module mymod
  integer :: var1 = 11
  integer :: var2 = 12
  integer :: var3 = 13
end module mymod

Program test
  call use_renamed()
  contains
    subroutine use_renamed()
      use mymod, var4 => var1
      print *, var4
    end subroutine use_renamed
end program test
