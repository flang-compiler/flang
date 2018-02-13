! REQUIRES: llvm5
! RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s
module mixed_vars
integer :: var1 = 17
integer :: var2
integer :: var3 = 37
integer :: var4
contains
subroutine change_val()
var2 = 27
var4 = 47
end subroutine
end module

program hello
use mixed_vars
call change_val()
end program hello

! //CHECK: @_mixed_vars_8_ = global %struct_mixed_vars_8_ <{ [8 x i8] {{.*}} !dbg ![[DBG_VAR1_GVEXP:[0-9]+]], !dbg ![[DBG_VAR3_GVEXP:[0-9]+]]
! //CHECK: @_mixed_vars_0_ = common global %struct_mixed_vars_0_ zeroinitializer, {{.*}} !dbg ![[DBG_VAR2_GVEXP:[0-9]+]], !dbg ![[DBG_VAR4_GVEXP:[0-9]+]]

! //CHECK: ![[DBG_VAR1_GVEXP:[0-9]+]] = !DIGlobalVariableExpression(var: ![[DBG_VAR1:[0-9]+]], expr: ![[DBG_VAR1_EXP:[0-9]+]])
! //CHECK: ![[DBG_VAR1]] = distinct !DIGlobalVariable(name: "var1", linkageName: "_mixed_vars_8_", scope: ![[SCOPE:[0-9]+]],
! //CHECK: ![[SCOPE]] = !DIModule({{.*}}, name: "mixed_vars"
! //CHECK: ![[DBG_VAR1_EXP]] = !DIExpression({{.*}})
! //CHECK: ![[DBG_VAR3_GVEXP:[0-9]+]] = !DIGlobalVariableExpression(var: ![[DBG_VAR3:[0-9]+]], expr: ![[DBG_VAR3_EXP:[0-9]+]])
! //CHECK: ![[DBG_VAR3]] = distinct !DIGlobalVariable(name: "var3", linkageName: "_mixed_vars_8_", scope: ![[SCOPE]],
! //CHECK: ![[DBG_VAR3_EXP]] = !DIExpression({{.*}}4{{.*}})

! //CHECK: ![[DBG_VAR2_GVEXP:[0-9]+]] = !DIGlobalVariableExpression(var: ![[DBG_VAR2:[0-9]+]], expr: ![[DBG_VAR1_EXP:[0-9]+]])
! //CHECK: ![[DBG_VAR2]] = distinct !DIGlobalVariable(name: "var2", linkageName: "_mixed_vars_0_", scope: ![[SCOPE:[0-9]+]],
! //CHECK: ![[DBG_VAR4_GVEXP:[0-9]+]] = !DIGlobalVariableExpression(var: ![[DBG_VAR4:[0-9]+]], expr: ![[DBG_VAR2_EXP:[0-9]+]])
! //CHECK: ![[DBG_VAR4]] = distinct !DIGlobalVariable(name: "var4", linkageName: "_mixed_vars_0_", scope: ![[SCOPE]],
