!
! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
!

! Check that short-circuit evaluation of logical expressions work as expected.
! The two operands should not be reordered if neither is constant.
!
! RUN: %flang -S %s -emit-llvm -o - | FileCheck %s
!
! CHECK: [[y8:%[0-9]+]] = bitcast i64* %y to i8*
! CHECK: [[LOP:%[0-9]+]] = icmp eq i8* [[y8]], null
! CHECK: br i1 [[LOP]], label %L.[[SHORTCUT:.*]], label %L.[[ROP:.*]],
! CHECK: L.[[ROP]]:
! CHECK: [[y32:%[0-9]+]] = bitcast i64* %y to i32*
! CHECK: load i32, i32* [[y32]]
! CHECK: L.[[SHORTCUT]]:

subroutine setoptional(x, y)
  integer :: x
  integer, intent(inout), optional ::y
  if (present(y) .and. y == 10) then
    y = x
  end if
end subroutine setoptional
