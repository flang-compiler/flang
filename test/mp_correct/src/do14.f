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

!	OpenMP parallel do with reduction

	program p
	 implicit none
	 integer n
	 parameter(n=10)
	 real a(0:n),b(n)
	 real result(n+3)
	 real expect(n+3)
	 data expect/1.,3.,5.,7.,9.,11.,13.,15.,17.,19.,
     &		     1.,38.,203./
	 integer i
	 real p1,p2,p3
	 do i = 0,n
	  a(i) = 2*i
	 enddo
	 p1 = 1.0
	 p2 = 2.0
	 p3 = 3.0
	 call sp2(a,b,n,p1,p2,p3)
	 do i = 1,n
	  result(i) = b(i)
	 enddo
	 result(n+1) = p1
	 result(n+2) = p2
	 result(n+3) = p3
!	print *,p1,p2,p3
!	print *,b
	 call check(result,expect,n+3)
	end

	subroutine sp2(a,b,n,p1,p2,p3)
	 implicit none
	 integer n
	 real a(0:n),b(n)
	 integer i
	 real p1,p2,p3
!$omp   parallel do private(p1) reduction(max:p2) reduction(+:p3)
	do i = 1,n
	 p1 = a(i) + a(i-1)
	 p2 = max(p2,p1)
	 p3 = p3 + p1
	 b(i) = (p1)/2.0
	enddo
	end
