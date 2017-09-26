! Copyright (c) 2012, NVIDIA CORPORATION.  All rights reserved.
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
logical expect(10), rslt(10)
type :: objects(l1,k1,k2)
integer, kind :: k1 = 3
integer, kind :: k2 = selected_char_kind("ASCII")
integer, len :: l1
character(kind=k2,len=l1) :: c
!integer  :: a(1+l1)
!integer   a(l1+1)
end type
contains
integer function check_dt(aaa)
type(objects(k2=1,k1=3,l1=:)),allocatable:: aaa
!type(objects(k2=1,k1=3,l1=*)):: aaa
!print *, 'char=',aaa%c
rslt(8)= aaa%c .eq. 'abcd'
!print *, 'size = ',size(aaa%a)
!print *, 'l1=',aaa%l1, len(aaa%c)
rslt(9) = aaa%l1 .eq. 5
rslt(10) = len(aaa%c) .eq. 5
check_dt = len(aaa%c)
end function

end module

program p
use mod
integer i
type(objects(k2=1,k1=3,l1=6)) :: y
type(objects(l1=:)),allocatable :: x
type(objects(k2=1,k1=3,l1=4)) :: z

allocate(objects(k1=3,k2=1,l1=5) :: x)

!do i=1,size(x%a)
!x%a(i) = i
!enddo

x%c = 'abcd'
!print *, x%c,x%l1
rslt(1) = x%c .eq. 'abcd'
y%c = 'stuv'
!print *, y%c,y%l1
rslt(2) = y%c .eq. 'stuv'
z%c = 'wxyz'
rslt(3) = z%c .eq. 'wxyz'
!print *, z%c,z%l1

expect = .true.

!print *, x%c, kind(x%c), len(x%c), 'l1=',x%l1
rslt(4) = kind(x%c) .eq. 1
rslt(5) = len(x%c) .eq. 5
rslt(6) = x%l1 .eq. 5
i = check_dt(x)
!print *, 'len =',i,len(x%c)
rslt(7) = i .eq. len(x%c)

call check(rslt,expect,10)
end
