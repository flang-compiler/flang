!** Copyright (c) 2019, Arm Ltd.  All rights reserved.

!** Licensed under the Apache License, Version 2.0 (the "License");
!** you may not use this file except in compliance with the License.
!** You may obtain a copy of the License at
!**
!**     http://www.apache.org/licenses/LICENSE-2.0
!**
!** Unless required by applicable law or agreed to in writing, software
!** distributed under the License is distributed on an "AS IS" BASIS,
!** WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!** See the License for the specific language governing permissions and
!** limitations under the License.

!* Test fix for kind mismatch when intrinsic len is used in a function call

module str_len_test_mod
 implicit none
 integer :: GLOBAL_SIZE = 100
contains
subroutine caller()
  implicit none
  character(len=GLOBAL_SIZE) :: st
  call called(st, len(st))
end subroutine caller

subroutine called(string, str_len)
  implicit none
  integer, intent(in) :: str_len
  character(len=str_len), intent(inout) :: string
  write(*,*) string, str_len
end subroutine called
end module str_len_test_mod

program test
  use str_len_test_mod
  integer, parameter :: num = 1
  integer rslts(num), expect(num)
  data expect / 1 /

  call caller()

  rslts(1) = 1
  call check(rslts, expect, num)
end program
