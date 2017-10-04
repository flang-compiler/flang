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

c	OpenMP Parallel Region
c	default(none) private shared if

	program p
	call t1(1,0)
	call t1(0,0)
	end

	subroutine t1(p,off)
	 parameter(n=5)
	 integer a(0:n),p,off
	 integer result(n+2)
	 integer expect1(n+2)
	 integer expect2(n+2)
	 data expect1/-1,0,1,2,3,-1,-1/
	 data expect2/-1,0,-1,-1,-1,-1,-1/
	 integer iam, i, omp_get_thread_num
	 do i = 0,n
	  a(i) = -1
	 enddo
	 iam = -1
c$omp	parallel default(none) private(iam) shared(a,off) 
c$ompx		if(p.gt.0.and.p.lt.9)
	 iam = omp_get_thread_num()+off
	 if( iam .ge. 0 .and. iam .le. n ) a(iam) = iam
c$omp	end parallel
c	t1/iam should be unmodified
c	t1/a should be modified for as many threads as there are
	!print *,'iam is ',iam
	!print *,'  a is ',a
	result(1) = iam
	do i = 0,n
	 result(i+2) = a(i)
	enddo
     	if(p.gt.0.and.p.lt.9)then
	 call check(result,expect1,n+2)
	else
	 call check(result,expect2,n+2)
	endif
	end
