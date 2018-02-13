! REQUIRES: llvm5
! RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s
module mixed_type_vars
logical(kind=1) :: var1 = .TRUE.
integer :: var2 = 19
double precision :: var3 = 3.14
character(len=5) :: var4 = "hello"
contains
subroutine check_val()
end subroutine
end module

program main
use mixed_type_vars
call check_val()
end program

! //CHECK: @_mixed_type_vars_9_ = global %struct_mixed_type_vars_9_ <{ [5 x i8] {{.*}} !dbg ![[DBG_VAR4_GVEXP:[0-9]+]]
! //CHECK: @_mixed_type_vars_10_ = global %struct_mixed_type_vars_10_ <{ [8 x i8] {{.*}} !dbg ![[DBG_VAR3_GVEXP:[0-9]+]]
! //CHECK: @_mixed_type_vars_8_ = global %struct_mixed_type_vars_8_ <{ [8 x i8] {{.*}} !dbg ![[DBG_VAR1_GVEXP:[0-9]+]], !dbg ![[DBG_VAR2_GVEXP:[0-9]+]]

! //CHECK: ![[DBG_VAR4_GVEXP]] = !DIGlobalVariableExpression(var: ![[DBG_VAR4:[0-9]+]], expr: ![[DBG_VAR1_EXP:[0-9]+]])
! //CHECK: ![[DBG_VAR4]] = distinct !DIGlobalVariable(name: "var4", linkageName: "_mixed_type_vars_9_", scope: ![[SCOPE:[0-9]+]], {{.*}}, type: ![[VAR4_TYPE:[0-9]+]]
! //CHECK: ![[SCOPE]] = !DIModule({{.*}}, name: "mixed_type_vars"{{.*}}
! //CHECK: ![[VAR4_TYPE]] = !DIBasicType({{.*}}name: "character", size: 40, align: 8, encoding: DW_ATE_signed)
! //CHECK: ![[DBG_VAR1_EXP]] = !DIExpression({{.*}})
! //CHECK: ![[DBG_VAR3_GVEXP]] = !DIGlobalVariableExpression(var: ![[DBG_VAR3:[0-9]+]], expr: ![[DBG_VAR1_EXP]])
! //CHECK: ![[DBG_VAR3]] = distinct !DIGlobalVariable(name: "var3", linkageName: "_mixed_type_vars_10_", scope: ![[SCOPE]], {{.*}}, type: ![[VAR3_TYPE:[0-9]+]]
! //CHECK: ![[VAR3_TYPE]] = !DIBasicType(name: "double precision", size: 64, align: 64, encoding: DW_ATE_float)
! //CHECK: ![[DBG_VAR1_GVEXP]] = !DIGlobalVariableExpression(var: ![[DBG_VAR1:[0-9]+]], expr: ![[DBG_VAR1_EXP]])
! //CHECK: ![[DBG_VAR1]] = distinct !DIGlobalVariable(name: "var1", linkageName: "_mixed_type_vars_8_", scope: ![[SCOPE]], {{.*}}, type: ![[VAR1_TYPE:[0-9]+]]
! //CHECK: ![[VAR1_TYPE]] = !DIBasicType(name: "logical*1", size: 8, align: 8, encoding: DW_ATE_boolean)
! //CHECK: ![[DBG_VAR2_GVEXP]] = !DIGlobalVariableExpression(var: ![[DBG_VAR2:[0-9]+]], expr: ![[DBG_VAR2_EXP:[0-9]+]])
! //CHECK: ![[DBG_VAR2]] = distinct !DIGlobalVariable(name: "var2", linkageName: "_mixed_type_vars_8_", scope: ![[SCOPE]], {{.*}}, type: ![[VAR2_TYPE:[0-9]+]]
! //CHECK: ![[VAR2_TYPE]] = !DIBasicType(name: "integer", size: 32, align: 32, encoding: DW_ATE_signed)
! //CHECK: ![[DBG_VAR2_EXP]] = !DIExpression({{.*}}4{{.*}})
