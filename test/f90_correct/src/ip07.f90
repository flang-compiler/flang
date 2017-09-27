!
! Copyright (c) 1998, NVIDIA CORPORATION.  All rights reserved.
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
! Host array parameter used by an internal subprogram
!
program test
integer, dimension(20) :: result, expect
call z(result, 20)
data expect/1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5/
call check(result, expect, 20)
end

subroutine z(result, n)
integer result(n)
integer, dimension(5), parameter :: idxatt = (/ 1, 2, 3, 4, 5 /)
integer, dimension(5) :: jjj
    jjj = idxatt
    result(1:5) = idxatt
    call zsub1
    contains
	subroutine zsub1
	result(6:10)  = idxatt
	result(11:15) = jjj
	call zsub2(idxatt)
	endsubroutine
	subroutine zsub2(ii)
	integer ii(:)
	result(16:20) = ii
	endsubroutine
end
