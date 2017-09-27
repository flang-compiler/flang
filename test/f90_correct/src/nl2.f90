!
! Copyright (c) 2015, NVIDIA CORPORATION.  All rights reserved.
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

program t
integer,pointer::xptr(:)
integer,target::x(1:5)
integer result,expect

x = 7
xptr=>x
!xptr=>x(2:4)
namelist/xx/ xptr

namelist/xx/ xptr
open(11, file='out', action='write')
write(11, '(A)') "    &xx xptr(3)=1/"
close(11)

open(17,file='out', action='read')
read(17, nml=xx)

expect=1
result=xptr(3)
call check(expect,result, 1)

end
