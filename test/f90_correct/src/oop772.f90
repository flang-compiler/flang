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

type t
  integer :: i
  contains
  procedure :: func1
  procedure :: func2
  generic :: func => func1, func2
end type
contains
integer function func1(this, x)
class(t) :: this
integer :: x
func1 = x 
end function

integer function func2(this)
class(t) :: this
func2 = this%i
end function

end module

use mod
logical rslt(1), expect(1)
integer z
type(t) :: o
o%i = -99

z = o%func(o%func())
!print *, z
rslt(1) = z .eq. -99
expect = .true.
call check(rslt,expect,1)

end
