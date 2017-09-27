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
type :: objects(k1,k2)
integer, kind :: k1 = 3 
integer, kind :: k2 = selected_char_kind("ASCII")
character(kind=k2,len=3) :: c
end type
end module

program p
use mod
logical expect(3), rslt(3)
type(objects) :: x

x%c = 'abc'

expect = .true.

!print *, x%c, len(x%c)
rslt(1) = kind(x%c) .eq. selected_char_kind("ASCII")
rslt(2) = len(x%c) .eq. 3
rslt(3) = x%c .eq. 'abc'

call check(rslt,expect,3)

end
