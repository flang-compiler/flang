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
! tests errmsg clause

integer, allocatable, dimension(:) :: a
character(80) :: msg
integer :: status
integer, dimension(6) :: res
integer, dimension(6) :: exp = (/ 0, 0, 0, 0, 1, -1 /)

msg = ""
status = 99
allocate(a(1000), stat=status, errmsg=msg)
!print *, status, trim(msg)
res(1) = status
res(2) = (msg .ne. "")
deallocate(a, stat=status, errmsg=msg)
!print *, status, trim(msg)
res(3) = status
res(4) = (msg .ne. "")
deallocate(a, stat=status, errmsg=msg)
!print *, status, trim(msg)
res(5) = status
res(6) = (msg .ne. "")

call check(res, exp, 6)

end
