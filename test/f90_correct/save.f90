!
! Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
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

! RUN: %flang -Msave %s -S -emit-llvm -o - | FileCheck %s -check-prefix=SAVE
! RUN: %flang -Mnosave %s -S -emit-llvm -o - | FileCheck %s -check-prefix=NOSAVE

! SAVE: %struct.BSS1 = type <{ [4 x i8] }>
! SAVE: @.BSS1 = internal global %struct.BSS1 zeroinitializer, align 32

program msave
  implicit none
! NOSAVE: alloca i32
  integer :: x
! NOSAVE: store i32 5, i32* %x
! SAVE: bitcast %struct.BSS1* @.BSS1 to i32*
! SAVE: store i32 5, i32*
  x = 5
end program


