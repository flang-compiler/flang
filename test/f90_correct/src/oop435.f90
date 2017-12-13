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

program p
implicit none
logical rslt(2),expect(2)
type :: objects(l1)
integer, kind :: l1 = 10
character(len=l1) :: c
end type


type(objects(5)) :: x

expect = .true.

x%c = '12345'

rslt(1) = x%c .eq. '12345'
rslt(2) = len_trim(x%c) .eq. x%l1

call check(rslt,expect,2)
end



