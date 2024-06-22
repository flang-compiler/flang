!
! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
!

! RUN: %flang -c -Mbackslash %s
! RUN: %flang -c -fno-backslash %s
! RUN: not %flang -c -Mnobackslash %s 2>&1 | FileCheck %s
! RUN: not %flang -c -fbackslash %s 2>&1 | FileCheck %s
! RUN: %flang -c -Mnobackslash -Mbackslash %s
! RUN: %flang -c -fbackslash -fno-backslash %s
! RUN: not %flang -c -Mbackslash -Mnobackslash %s 2>&1 | FileCheck %s
! RUN: not %flang -c -fno-backslash -fbackslash %s 2>&1 | FileCheck %s
! RUN: not %flang -c -Mnobackslash -Mbackslash -Mnobackslash %s 2>&1 | FileCheck %s
! RUN: not %flang -c -fbackslash -fno-backslash -fbackslash %s 2>&1 | FileCheck %s

write (*,*) "\" ! CHECK: Unmatched quote
end
