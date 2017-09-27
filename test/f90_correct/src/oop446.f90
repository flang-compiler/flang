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
logical expect(8),rslt(8)
type :: stuff(k11,k22)
integer(k22) :: i
integer,kind :: k22 = 2 
integer,kind :: k11 = 4
integer(k22) :: j
end type
end module

program p
use mod


type(stuff) :: y

y = stuff(2,4)(i=3,j=4)

rslt(1) = y%i .eq. 3
rslt(5) = y%j .eq. 4
rslt(2) = kind(y%i) .eq. 2
rslt(3) = y%k11 .eq. 4
rslt(4) = y%k22 .eq. 2

expect = .true.
call check(rslt,expect,5)

end
