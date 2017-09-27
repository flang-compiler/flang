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

module flang3
use, intrinsic :: iso_c_binding

type, bind(c) :: bar
integer(c_int) :: bar1 = -1
end type bar

type, bind(c) :: zip
type(bar) :: zip1
end type zip

type, bind(c) :: roo
integer(c_int),dimension(99) :: roo1 = 0
type(zip) :: roo2(0:98)
end type roo

type(roo), public, target, dimension(0:98), bind(c) :: foo
save :: foo

end module

SUBROUTINE MOO
USE flang3
DO m=1,2 
write (*,*) foo(m)%roo2
END DO
END SUBROUTINE

print *, "PASS"
end
