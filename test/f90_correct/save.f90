!
! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
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


