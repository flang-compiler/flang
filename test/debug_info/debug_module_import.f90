!
! Copyright (c) 2018, Arm Ltd.  All rights reserved.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!

! RUN: %flang -g -S -emit-llvm %s -o - | FileCheck %s
module first
integer :: var1 = 37
end module

module second
use first
integer :: var2 = 47
contains
function init()
  var2 = var2 - var1
end function
end module

program hello
use second
print *, var1
print *, var2
end program hello

! //CHECK: ![[DBG_MOD1:[0-9]+]] = !DIModule({{.*}}, name: "first"
! //CHECK: ![[DBG_DIC:[0-9]+]] = distinct !DICompileUnit({{.*}}, imports: ![[DBG_IMPORTS:[0-9]+]])
! //CHECK: ![[DBG_MOD2:[0-9]+]] = !DIModule({{.*}}, name: "second"
! //CHECK: ![[DBG_IMPORTS]] = !{![[DBG_IE1:[0-9]+]], ![[DBG_IE2:[0-9]+]], ![[DBG_IE3:[0-9]+]]}
! //CHECK: ![[DBG_IE1]] = !DIImportedEntity(tag: DW_TAG_imported_module, scope: ![[DBG_SP1:[0-9]+]], entity: ![[DBG_MOD1]],
! //CHECK: ![[DBG_SP1]] = distinct !DISubprogram(name: "init", scope: ![[DBG_MOD2]]
! //CHECK: ![[DBG_IE2]] = !DIImportedEntity(tag: DW_TAG_imported_module, scope: ![[DBG_SP2:[0-9]+]], entity: ![[DBG_MOD1]],
! //CHECK: ![[DBG_SP2]] = distinct !DISubprogram(name: "hello", scope: ![[DBG_DIC]]
! //CHECK: ![[DBG_IE3]] = !DIImportedEntity(tag: DW_TAG_imported_module, scope: ![[DBG_SP2]], entity: ![[DBG_MOD2]],
