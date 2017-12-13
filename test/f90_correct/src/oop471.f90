! Copyright (c) 2012-2017, NVIDIA CORPORATION.  All rights reserved.
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
logical expect(6), rslt(6)
type :: objects(l1,k1)
integer, kind :: k1 = selected_char_kind("ASCII")
integer, len :: l1
character(kind=k1,len=l1) :: c
end type
contains
integer function check_dt(xx)
!type(objects(l1=:)),allocatable:: xx
type(objects(l1=*)):: xx
check_dt = len(xx%c)
print *, xx%l1
end function

end module

program p
use mod
integer i
type(objects(l1=:)),allocatable :: x
type(objects(l1=6)) :: y

allocate(objects(l1=4) :: x)

x%c = 'abcd'

expect = .true.

!print *, x%c, kind(x%c), len(x%c)
rslt(1) = kind(x%c) .eq. selected_char_kind("ASCII")
rslt(2) = len(x%c) .eq. 4 
rslt(3) = x%c .eq. 'abcd'

y%c = x%c
rslt(1) = y%c .eq. 'abcd'
rslt(2) = kind(y%c) .eq. 1
rslt(3) = len(y%c) .eq. 4
rslt(4) =  y%l1 .eq. 6
!print *, y%c, kind(y%c), len(y%c), y%l1
rslt(5) = x%l1 .eq. 4
!print *, x%l1
!i = check_dt(x)
i = len(x%c)
rslt(6) = i .eq. 4
!print *, 'len =',i
call check(rslt,expect,6)

end
