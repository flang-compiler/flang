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

c	Simple OpenMP Parallel Region
c	private atomic

	program p
	call t1
	end

	subroutine t1
	 parameter(n=5)
	 integer a(0:n)
	 integer result(n+3)
	 integer expect(n+3)
	 data expect/-1,0,1,2,3,-1,-1,6/
	 integer iam, i, omp_get_thread_num, atomic, f
	 external f
	 do i = 0,n
	  a(i) = -1
	 enddo
	 iam = -1
	 atomic = 0
c$omp	parallel private(iam)
	 iam = omp_get_thread_num()
	 if( iam .ge. 0 .and. iam .le. n ) a(iam) = iam
c$omp	atomic
	 atomic = atomic + f(iam)
c$omp	end parallel
c	t1/iam should be unmodified
c	t1/a should be modified for as many threads as there are
	!print *,'iam is ',iam
	!print *,'  a is ',a
	!print *,'atomic is ',atomic
	result(1) = iam
	do i = 0,n
	 result(i+2) = a(i)
	enddo
	result(n+3) = atomic
	call check(result,expect,n+3)
	end

	integer function f(i)
	f = i
	end
