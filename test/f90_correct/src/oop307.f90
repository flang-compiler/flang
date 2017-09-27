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
contains
procedure :: my_status
end type
type, public, extends(v) :: w
integer tag
end type

type, public :: t
class(v),allocatable :: comp
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
logical rslt(5)
logical expect(5)

rslt = .false.
expect = .true.

allocate(obj)
!print *, allocated(obj%comp)
rslt(1) = .not. allocated(obj%comp)
allocate(w::obj%comp)
!print *, allocated(obj%comp)
rslt(2) = allocated(obj%comp)

allocate(obj%comp%r(10))
do i=1,10
obj%comp%r(i) = i
rr(i) = i
enddo

select type(o=>obj%comp)
type is (w)
o%tag = 999
rslt(3) = .true.
!print *, o
type is (v)
!print *, o
end select

allocate(obj2,source=obj)
deallocate(obj%comp%r)
deallocate(obj%comp)
rslt(4) = obj2%comp%my_status()
select type(o=>obj2%comp)
type is (w)
!print *, o
rslt(5) = all(rr .eq. o%r)
type is (v)
!print *, o
end select

call check(rslt,expect,5)


end
