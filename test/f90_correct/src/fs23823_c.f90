! Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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

MODULE test_c_ptr
USE,INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE

type :: handle_type
  integer :: iptr
contains
  procedure :: getptr
end type

CONTAINS

FUNCTION getptr(a)
class(handle_type),INTENT(in) :: a
TYPE(c_ptr) :: getptr
integer :: rslts, expect
expect = 4
rslts = a%iptr
call check(rslts, expect, 1)

getptr = c_null_ptr

END FUNCTION getptr

END MODULE test_c_ptr

PROGRAM main
USE test_c_ptr
USE,INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE

TYPE(c_ptr) :: dummy
type(handle_type) :: input
input%iptr = 4
dummy = input%getptr()

END PROGRAM main
