! REQUIRES: llvm5
! RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s
module init_vars
integer :: var1 = 37
integer :: var2 = 47
end module

program hello
use init_vars
print *, var1
end program hello

! //CHECK: @_init_vars_8_ = global %struct_init_vars_8_ <{ [8 x i8] {{.*}} !dbg ![[DBG_VAR1_GVEXP:[0-9]+]], !dbg ![[DBG_VAR2_GVEXP:[0-9]+]]
! //CHECK: ![[DBG_VAR1_GVEXP:[0-9]+]] = !DIGlobalVariableExpression(var: ![[DBG_VAR1:[0-9]+]], expr: ![[DBG_VAR1_EXP:[0-9]+]])
! //CHECK: ![[DBG_VAR1]] = distinct !DIGlobalVariable(name: "var1", linkageName: "_init_vars_8_", scope: ![[SCOPE:[0-9]+]],
! //CHECK: ![[SCOPE]] = !DIModule({{.*}}, name: "init_vars"
! //CHECK: ![[DBG_VAR1_EXP]] = !DIExpression({{.*}})
! //CHECK: ![[DBG_VAR2_GVEXP:[0-9]+]] = !DIGlobalVariableExpression(var: ![[DBG_VAR2:[0-9]+]], expr: ![[DBG_VAR2_EXP:[0-9]+]])
! //CHECK: ![[DBG_VAR2]] = distinct !DIGlobalVariable(name: "var2", linkageName: "_init_vars_8_", scope: ![[SCOPE]],
! //CHECK: ![[DBG_VAR2_EXP]] = !DIExpression({{.*}}4{{.*}})
