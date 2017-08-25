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

! RUN: %flang -S -emit-llvm %s -o - 2>&1 | FileCheck %s

program shift64
  integer(8) :: x, y

  ! CHECK: sext i32 3 to i64
  ! CHECK-NEXT: shl
  x = ishft(x, 3)
  ! CHECK: sext i32 5 to i64
  ! CHECK-NEXT: lshr
  x = ishft(x, -5)
  ! CHECK: @ftn_i_kishft
  y = ishft(x, y)
end program
