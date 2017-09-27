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

module flang4
use, intrinsic :: iso_c_binding

type, bind(c) :: bar
integer(c_int) :: bar1
end type bar

type, public :: car
type(c_ptr) :: car1
type (bar) :: car2
end type car

type, bind(c) :: zip
type(bar) :: zip1
type(bar) :: zip2
end type zip

type, bind(c) :: roo
integer(c_int),dimension(99) :: roo1
type(zip) :: roo2(0:98)
end type roo

type(roo), public, target, bind(c) :: foo

end module

module yoo4
use, intrinsic :: iso_c_binding
use flang4
contains
integer function new_yoo (t1, t2) result (res)
type (car), intent(out) :: t1
type (bar), intent(in) :: t2
res = 0
end function
end module

module gar4
use yoo4
contains
subroutine gar1
INTEGER :: g1
type (car) :: g12(1)
g1 = new_yoo (g12(1), foo%roo2(1)%zip1)
end subroutine gar1
subroutine gar2
INTEGER :: g2
type (car) :: g22(1)
g1 = new_yoo (g22(1), foo%roo2(1)%zip2)
end subroutine gar2
end module

print *, "PASS"
end
