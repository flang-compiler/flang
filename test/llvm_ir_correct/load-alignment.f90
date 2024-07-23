! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

! Check that load instructions are emitted with correct alignment.

! RUN: %flang -S -emit-flang-llvm %s -o %t.ll
! RUN: FileCheck %s < %t.ll

! CHECK: load i8, ptr %in1, align 1
function bpass(in1)
  logical(kind=1) :: in1, bpass
  bpass = in1
end function bpass

! CHECK: load i32, ptr %in1, align 4
function ipass(in1)
  integer(kind=4) :: in1, ipass
  ipass = in1
end function ipass

! CHECK: load i64, ptr %in1, align 8
function lpass(in1)
  integer(kind=8) :: in1, lpass
  lpass = in1
end function lpass

! CHECK: load float, ptr %in1, align 4
function fpass(in1)
  real(kind=4) :: in1, fpass
  fpass = in1
end function fpass

! CHECK: load double, ptr %in1, align 8
function dpass(in1)
  real(kind=8) :: in1, dpass
  dpass = in1
end function dpass

! CHECK: load <{float, float}>, ptr %in1, align 8
function cfpass(in1)
  complex(kind=4) :: in1, cfpass
  cfpass = in1
end function cfpass

! CHECK: load <{double, double}>, ptr %in1, align 16
function cdpass(in1)
  complex(kind=8) :: in1, cdpass
  cdpass = in1
end function cdpass
