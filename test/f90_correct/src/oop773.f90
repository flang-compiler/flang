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
type obj
contains
procedure :: bar
end type 

contains
subroutine foo()
rslt(1) = .true.
end subroutine

subroutine bar(this)
class(obj) :: this

!block
type t
contains
procedure, nopass :: foo
end type
type(t) :: o

call o%foo()
!end block
end subroutine

end module


use mod
type(obj) :: x
expect = .true.
rslt = .false.
call x%bar()
call check(rslt, expect, 1)
end
