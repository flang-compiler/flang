! REQUIRES: llvm5
! RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s
module mod_vars1
  type outpatch_list
    integer :: patch_val
    integer, allocatable, dimension(:) :: patch_list
  end type outpatch_list
  integer, parameter :: ndims = 2
  integer :: val1 = 20
end module

module mod_vars2
  use mod_vars1
  type(outpatch_list) :: outpatch_table
  integer, allocatable, dimension(:)   :: patch_count
  integer, dimension(ndims) :: dims
  integer :: val2
  contains
  function check_flang()
    print *, dims
    check_flang = val1
  end function
end module

program main
use mod_vars2
print *,check_flang()
end program

! //CHECK: @_mod_vars1_8_ = global %struct_mod_vars1_8_ {{.*}} !dbg ![[DBG_VAL1_GVEXP:[0-9]+]]
! //CHECK: @_mod_vars2_0_ = global %struct_mod_vars2_0_ zeroinitializer, {{.*}} !dbg ![[DBG_PATCH_COUNT_GVEXP:[0-9]+]], !dbg ![[DBG_PATCH_TABLE_GVEXP:[0-9]+]], !dbg !31, !dbg ![[DBG_DIMS_GVEXP:[0-9]+]], !dbg ![[DBG_VAL2_GVEXP:[0-9]+]]

! //CHECK: ![[MODULE_SCOPE1:[0-9]+]] = !DIModule({{.*}}, name: "mod_vars1"
! //CHECK: ![[DBG_PATCH_TABLE_TYPE:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "outpatch_list",
! //CHECK: ![[DBG_VAL1_TYPE:[0-9]+]] = !DIBasicType(name: "integer", size: 32, align: 32, encoding: DW_ATE_signed)
! //CHECK: ![[DBG_PATCH_COUNT_TYPE:[0-9]+]] = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: ![[DBG_VAL1_TYPE]], size: 64, align: 64)
! //CHECK: ![[DBG_VAL1_GVEXP]] = !DIGlobalVariableExpression(var: ![[DBG_VAL1:[0-9]+]], expr: ![[DBG_VAL1_VEXP:[0-9]+]])
! //CHECK: ![[DBG_VAL1]] = distinct !DIGlobalVariable(name: "val1", linkageName: "_mod_vars1_8_", scope: ![[MODULE_SCOPE1]], {{.*}}, type: ![[DBG_VAL1_TYPE]],
! //CHECK: ![[DBG_VAL1_VEXP:[0-9]+]] = !DIExpression({{.*}}96{{.*}})
! //CHECK: ![[DBG_PATCH_COUNT_GVEXP]] = !DIGlobalVariableExpression(var: ![[DBG_PATCH_COUNT:[0-9]+]], expr: ![[DBG_PATCH_COUNT_VEXP:[0-9]+]])
! //CHECK: ![[DBG_PATCH_COUNT]] = distinct !DIGlobalVariable(name: "patch_count{{.*}}", linkageName: "_mod_vars2_0_", scope: ![[MODULE_SCOPE2:[0-9]+]], {{.*}}, type: ![[DBG_PATCH_COUNT_TYPE]],
! //CHECK: ![[MODULE_SCOPE2]] = !DIModule({{.*}}, name: "mod_vars2"
! //CHECK: ![[DBG_PATCH_COUNT_VEXP]] = !DIExpression({{.*}}16{{.*}})
! //CHECK: ![[DBG_PATCH_TABLE_GVEXP]] = !DIGlobalVariableExpression(var: ![[DBG_PATCH_TABLE:[0-9]+]], expr: ![[DBG_PATCH_TABLE_VEXP:[0-9]+]])
! //CHECK: ![[DBG_PATCH_TABLE]] = distinct !DIGlobalVariable(name: "outpatch_table", linkageName: "_mod_vars2_0_", scope: ![[MODULE_SCOPE2]], {{.*}}, type: ![[DBG_PATCH_TABLE_TYPE]],
! //CHECK: ![[DBG_PATCH_TABLE_VEXP]] = !DIExpression({{.*}}88{{.*}})
! //CHECK: ![[DBG_DIMS_GVEXP]] = !DIGlobalVariableExpression(var: ![[DBG_DIMS:[0-9]+]], expr: ![[DBG_DIMS_VEXP:[0-9]+]])
! //CHECK: ![[DBG_DIMS]] = distinct !DIGlobalVariable(name: "dims", linkageName: "_mod_vars2_0_", scope: ![[MODULE_SCOPE2]], {{.*}}, type: ![[DBG_DIMS_TYPE:[0-9]+]],
! //CHECK: ![[DBG_DIMS_TYPE]] = !DICompositeType(tag: DW_TAG_array_type, baseType: ![[DBG_VAL1_TYPE]], size: 64, align: 32,
! //CHECK: ![[DBG_DIMS_VEXP]] = !DIExpression({{.*}}192{{.*}})
! //CHECK: ![[DBG_VAL2_GVEXP]] = !DIGlobalVariableExpression(var: ![[DBG_VAL2:[0-9]+]], expr: ![[DBG_VAL2_VEXP:[0-9]+]])
! //CHECK: ![[DBG_VAL2]] = distinct !DIGlobalVariable(name: "val2", linkageName: "_mod_vars2_0_", scope: ![[MODULE_SCOPE2]], {{.*}}, type: ![[DBG_VAL1_TYPE]],
! //CHECK: ![[DBG_VAL2_VEXP]] = !DIExpression({{.*}}200{{.*}})
