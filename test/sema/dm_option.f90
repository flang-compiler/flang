!
! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
! Just check some of the macros - if they are printed
! RUN: %flang -dM -E %s | FileCheck %s --check-prefix=CHECK-MACROS
!
! CHECK-MACROS: __amd64__
! CHECK-MACROS: __SIZE_TYPE__
! CHECK-MACROS: __FLANG_MAJOR__
! CHECK-MACROS: __FLANG_MINOR__
! CHECK-MACROS: __FLANG_PATCHLEVEL__
program hello
  print *, "hello world"
end program hello
