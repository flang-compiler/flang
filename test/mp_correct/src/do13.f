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

!	OpenMP parallel do with firstprivate, lastprivate

	program p
	 implicit none
	 integer n
	 parameter(n=10)
	 real a(0:n),b(n),x(n),y(n)
	 real result(2*n+3)
	 real expect(2*n+3)
	 data expect/1.,3.,5.,7.,9.,11.,13.,15.,17.,19.,
     &		     2.,6.,10.,14.,18.,22.,26.,30.,34.,38.,
     &		     1.,2.,38./
	 integer i
	 real p1,p2,p3
	 do i = 0,n
	  a(i) = 2*i
	 enddo
	 do i = 1,n
	  x(i) = i*i
	 enddo
	 p1 = 1.0
	 p2 = 2.0
	 p3 = 3.0
	 call sp2(a,b,x,y,n,p1,p2,p3)
	 do i = 1,n
	  result(i) = b(i)
	  result(n+i) = y(i)
	 enddo
	 result(n+n+1) = p1
	 result(n+n+2) = p2
	 result(n+n+3) = p3
!	print *,p1,p2,p3
!	print *,b
!	print *,y
	 call check(result,expect,2*n+3)
	end

	subroutine sp2(a,b,x,y,n,p1,p2,p3)
	 implicit none
	 integer n
	 real a(0:n),b(n),x(n),y(n)
	 integer i
	 real p1,p2,p3
!$omp   parallel do private(p1) firstprivate(p2) lastprivate(p3)
	do i = 1,n
	 p1 = a(i) + a(i-1)
	 p2 = p2 + 1
	 b(i) = (p1)/2.0
	 y(i) = p1
	 p3 = y(i)
	enddo
	end
