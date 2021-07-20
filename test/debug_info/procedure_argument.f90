!RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s

! check function pointer bitcast instructions should not have debug location
!CHECK-NOT: bitcast i32* @.C{{[0-9]+}}_example_func to i64*, !dbg {{![0-9]+}}
!CHECK-NOT: bitcast i32* @.C{{[0-9]+}}_example_func1 to i8*, !dbg {{![0-9]+}}
!CHECK-NOT: bitcast [8 x i8]* @.C{{[0-9]+}}_example_func1 to i8*, !dbg {{![0-9]+}}
!CHECK-NOT: bitcast void ()* @f90io_src_info03a to void (i8*, i8*, i64)*, !dbg {{![0-9]+}}
!CHECK-NOT: bitcast i32 ()* @f90io_print_init to i32 (i8*, i8*, i8*, i8*)*, !dbg {{![0-9]+}}
!CHECK-NOT: bitcast i32 ()* @f90io_sc_i_ldw to i32 (i32, i32)*, !dbg {{![0-9]+}}

program example
  implicit none

  call func()
contains
  subroutine func()
    call func1()
    call func2(2)
  end
  subroutine func1()
    integer :: i = 1
    call func3()
    print *, i
  end
  subroutine func2(j)
    integer :: j, k
    k = j
  end
  subroutine func3()
  end
end
