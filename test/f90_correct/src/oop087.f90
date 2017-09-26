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
type mytype
   integer y
contains
   procedure :: mysub => mysub1
end type mytype
interface
integer function mysub1(this) RESULT(R)
import mytype
class(mytype) :: this
type(mytype) :: m
end function
end interface
end module my_mod

integer function mysub1(this) RESULT(R)
use my_mod
class(mytype) :: this
type(mytype) :: m
R = extends_type_of(this,m)
end function mysub1

program prg
USE CHECK_MOD
use my_mod

type, extends(mytype) :: mytype2
   real u
end type mytype2

type, extends(mytype2) :: mytype3
   real t 
end type mytype3

integer results(6)
integer expect(6)
class(mytype2),allocatable :: my2
class(mytype),allocatable :: my
class(mytype3),allocatable :: my3

results = .false.
expect = .true.


allocate(my)
allocate(my2)
allocate(my3)

my%y = 1050
my2%u = 3.5
my3%t = 1.4

results(1) = my%mysub
results(2) = my2%mysub()
results(3) = my3%mysub
results(4) = mysub1(my)
results(5) = mysub1(my2)
results(6) = my2%mysub

call check(results,expect,6)

end


