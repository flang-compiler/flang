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

!	OpenMP do ordered


	program p
	 implicit none
	 integer n
	 parameter(n=10)
	 integer a(n)
	 integer result(n)
	 integer expect(n)
	 data expect/1,2,3,4,5,6,7,8,9,10/
	 integer i
	 do i = 1,n
	  a(i) = -1
	 enddo
	 call sp2(a,n)
	 !print *,a
	 do i = 1,n
	  result(i) = a(i)
	 enddo
	 call check(result,expect,n)
	end

	subroutine sp2(a,n)
	 implicit none
	 integer n
	 integer a(n)
	 integer i
!$omp    parallel
!$omp	  do ordered
	   do i = 1,n
	     call work(a, i)
	   enddo
!$omp    end parallel 
	end
	subroutine work(a,ii)
	integer a(*)
	integer idx
	save idx
	data idx/1/
!$omp ordered
!$omp critical
	a(idx) = ii
	idx = idx + 1
!$omp endcritical
!$omp endordered
	end
