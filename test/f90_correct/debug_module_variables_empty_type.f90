! RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s

module vars
type empty
end type empty

type(empty) :: nothing
end module vars

! //CHECK: @_vars_0_ = common global %struct_vars_0_ zeroinitializer, align 64, !dbg ![[DBG_VAR1_GVEXP:[0-9]+]]
! //CHECK: ![[DBG_VAR1_GVEXP]] = !DIGlobalVariableExpression(var: ![[DBG_VAR1:[0-9]+]], expr: ![[DBG_VAR1_EXP:[0-9]+]])
! //CHECK: ![[DBG_VAR1]] = distinct !DIGlobalVariable(name: "nothing", linkageName: "_vars_0_", scope: ![[SCOPE:[0-9]+]], {{.*}}, type: ![[DBG_VAR1_TYPE:[0-9]+]]
! //CHECK: ![[SCOPE]] = !DIModule({{.*}}, name: "vars"
! //CHECK: ![[DBG_VAR1_TYPE]] = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", {{.*}}, elements: ![[DBG_VAR1_ELEMENTS:[0-9]+]])
! //CHECK: ![[DBG_VAR1_ELEMENTS]] = !{}
! //CHECK: ![[DBG_VAR1_EXP]] = !DIExpression({{.*}})
