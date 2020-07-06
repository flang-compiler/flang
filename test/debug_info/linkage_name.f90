
! RUN: %flang -gdwarf-4  -fpic -c  %s -o %t
! RUN: llvm-dwarfdump -debug-info %t | FileCheck %s
! CHECK:  DW_TAG_variable
! CHECK:  DW_AT_linkage_name ("_lib_8_")
module lib
        integer :: var_i = 1
contains
        subroutine lib_func
        var_i = 2
        end subroutine lib_func
end module lib
