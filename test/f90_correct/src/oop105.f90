! Copyright (c) 2010, NVIDIA CORPORATION.  All rights reserved.
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

module my_mod
private
public :: mytype
type mytype
   integer y
contains
   private
   procedure,non_overridable :: mysub
   procedure,public,non_overridable :: mysub2
end type mytype
contains

integer function mysub(this)
class(mytype) :: this
type(mytype) :: m

print *, 'mysub called'
mysub = extends_type_of(this,m)
end function mysub

integer function mysub2(this)
class(mytype) :: this
type(mytype) :: m
print *, 'mysub2 called'
mysub2 = this%mysub
end function mysub2

end module my_mod


program prg
USE CHECK_MOD
use my_mod


type, extends(mytype) :: mytype2
   real u
end type mytype2

type, extends(mytype) :: mytype3
   real t 
end type mytype3

integer results(3)
integer expect(3)
class(mytype2),allocatable :: my2
class(mytype2),allocatable :: my
class(mytype3),allocatable :: my3

results = .false.
expect = .true.


allocate(my)
allocate(my2)
allocate(my3)

my%y = 1050
my2%u = 3.5
my3%t = 1.4

results(1) = my%mysub2
results(2) = my2%mysub2()
results(3) = my2%mysub2

!print *, all(results .eq. expect)
call check(results,expect,3)

end


