! Copyright (c) 2012, NVIDIA CORPORATION.  All rights reserved.
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

module mod
type :: objects(k1,k2,v1)
integer, kind :: k2 = selected_char_kind("ASCII")
integer, kind :: k1 = selected_int_kind(4) + 6
integer, len :: v1 = 99
end type
end module

program p
use mod
type(objects(1,2,3)) :: x
logical expect(3), rslt(3)

rslt(1) = x%k1 .eq. 1
rslt(2) = x%k2 .eq. 2
rslt(3) = x%v1 .eq. 3

expect = .true.
call check(rslt,expect,3)


end
