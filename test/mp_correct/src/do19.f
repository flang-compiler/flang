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

!	parallel do with cyclic scheduling, ordered

	program p
	 implicit none
	 integer n
	 parameter(n=10)
	 integer a(n),b(n)
	 integer result(2*n)
	 integer expect(2*n)
	 data expect/0,1,2,3,0,1,2,3,0,1,
     x		10,21,32,43,50,61,72,83,90,101/
	 integer i
	 do i = 1,n
	  a(i) = -1
	  b(i) = -1
	 enddo
	 call sp2(a,b,n)
	 !print *,a
	 !print *,b
	 do i = 1,n
	  result(i) = a(i)
	  result(i+n) = b(i)
	 enddo
	 call check(result,expect,2*n)
	end

	subroutine sp2(a,b,n)
	 implicit none
	 include 'ompf.h'
	 integer n
	 integer a(n),b(n)
	 integer iam, i, j
	 j = 0
!$omp    parallel private(iam)
	  iam = omp_get_thread_num()
!$omp	  do schedule(static,1) ordered
	  ! block scheduling
	   do i = 1,n
	    a(i) = iam
!$omp	    ordered
	     j = j + 1
	     b(i) = j*10+iam
!$omp	    end ordered
	   enddo
!$omp    end parallel 
	end
