! Copyright (c) 2011, NVIDIA CORPORATION.  All rights reserved.
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

module mod_gen
implicit none
private
type, public :: v
integer, allocatable :: r(:)
class(v), pointer :: cpy
contains
procedure :: my_status
end type
type, public, extends(v) :: w
integer,allocatable :: tag(:)
end type

type, public :: t
class(w),allocatable :: comp
end type

contains
logical function my_status(this)
class(v) :: this
!print *, 'status = ',allocated(this%r)
my_status = allocated(this%r)
end function

end module

program p
USE CHECK_MOD
use mod_gen
class(t), allocatable :: obj
class(t), allocatable :: obj2
real rr(10)
integer tag2(1)
logical rslt(5)
logical expect(5)

tag2 = 1234
rslt = .false.
expect = .true.

allocate(obj)
allocate(w::obj%comp)

allocate(obj%comp%r(10))
do i=1,10
obj%comp%r(i) = i
!print *, obj%comp%r(i)
rr(i) = i+10
enddo

select type(o=>obj%comp)
type is (w)
allocate(o%tag(1))
o%tag(1) = 999
rslt(1) = .true.
!print *, o
class default
!print *, o
rslt(1) = .false.
end select

allocate(obj%comp%cpy,source=obj%comp)
allocate(obj2,source=obj)

do i=1,10
obj2%comp%cpy%r(i) = obj2%comp%cpy%r(i) + 10
enddo

do i=1,10
obj%comp%r(i) = obj%comp%r(i) + 20
enddo

deallocate(obj%comp%r)
deallocate(obj%comp)


rslt(2) = obj2%comp%my_status()
rslt(3) = obj2%comp%cpy%my_status()

select type(o=>obj2%comp%cpy)
type is (w)
o%tag(1) = 1234
!print *, o%tag
rslt(4) = all(o%tag .eq. tag2)
rslt(5) = all(o%r .eq. rr)
end select

call check(rslt,expect,5)

end
