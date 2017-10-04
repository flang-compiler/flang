!* Copyright (c) 1998, NVIDIA CORPORATION.  All rights reserved.
!*
!* Licensed under the Apache License, Version 2.0 (the "License");
!* you may not use this file except in compliance with the License.
!* You may obtain a copy of the License at
!*
!*     http://www.apache.org/licenses/LICENSE-2.0
!*
!* Unless required by applicable law or agreed to in writing, software
!* distributed under the License is distributed on an "AS IS" BASIS,
!* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!* See the License for the specific language governing permissions and
!* limitations under the License.
 
!       OpenMP Reductions
!       Array reduction variables (extension)

module data
integer ir(10,10),nr(10)
integer, parameter :: NTESTS=10
integer,dimension(NTESTS) :: expect = (/10,20,30,40,50,60,70,80,90,100/)
contains
    subroutine init
	do i = 1, 10
	   do j = 1, 10
	       ir(i,j) = j
	   enddo
	   nr(i) = 0
	enddo
    end subroutine
endmodule

program red01
use data
call init
!$omp parallel do reduction(nr)
do i = 1, 10
    nr = nr + ir(i,:)
enddo
!$omp end parallel do
!print *, nr
call check(nr, expect, NTESTS)
end
