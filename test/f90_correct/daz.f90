!
! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
!

! RUN: %flang -target x86_64-unknown-unknown -Mdaz %s -S -emit-llvm -o - | FileCheck %s

! CHECK: @llvm.global_ctors = appending global [1 x { i32, void ()* }] [{ i32, void ()* } { i32 65535, void ()* @__daz }]

program daz
end program

! CHECK: declare void @__daz()
