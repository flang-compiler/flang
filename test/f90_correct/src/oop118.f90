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
type,abstract ::  mytype
   integer y
contains
   procedure (mysub1), nopass,deferred :: mysub
   procedure, nopass :: f
end type mytype
abstract interface
integer function mysub1(this)
import :: mytype
class(mytype) :: this
class(mytype),allocatable :: m
end function
end interface
contains
recursive integer function f(i)
 integer i

 !print *,i,' called'
 if( i .eq. 1 ) then
   ! base case
   f = 1
 else if( (i/2) * 2 .eq. i )then
   ! divide by two
   f = f(i/2)+1
 else
   ! multiply by three, add one
   f = f(i*3+1)+1
 endif
end function

end module my_mod

integer function mysub2(this)
use my_mod
class(mytype) :: this
class(mytype),allocatable :: m
mysub2 = extends_type_of(this,m)
end function 

program prg
USE CHECK_MOD
use my_mod

interface
integer function mysub2(this)
use my_mod
class(mytype) :: this
end function
end interface

type, extends(mytype) :: mytype2
   real u
   contains
   procedure,nopass :: mysub => mysub2
end type mytype2

type, extends(mytype2) :: mytype3
   real t 
end type mytype3

integer results(7)
integer expect(7)
class(mytype2),allocatable :: my2
class(mytype),allocatable :: my
class(mytype3),allocatable :: my3

results = .false.
expect = .true.


allocate(mytype2::my)
allocate(my2)
allocate(my3)

my%y = 1050
my2%u = 3.5
my3%t = 1.4

!results(1) = my%f(20) .eq. 8
results(1) = my%mysub(my)
results(2) = my2%mysub(my2)
results(3) = my3%mysub(my3)
results(4) = mysub2(my)
results(5) = mysub2(my2)
results(6) = my2%mysub(my3)
results(7) = my2%f(10) .eq. 7

call check(results,expect,7)

end


