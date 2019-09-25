! Copyright (c) 2019, NVIDIA CORPORATION.  All rights reserved.
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
logical rslt(1), expect(1)
contains
subroutine foo()
!print *, 'in foo'
end subroutine

subroutine foo2(i)
integer :: i
!print *, 'in foo2', i
rslt(1) = i .eq. -99
end subroutine


end module

subroutine bar()
use mod
!block
type t
contains
procedure, nopass :: foo
procedure, nopass :: foo2
generic :: func => foo, foo2 
end type
type(t) :: o

call o%func(-99)
!end block
end subroutine

use mod
expect = .true.
rslt = .false.
call bar()
call check(rslt, expect, 1)
end
