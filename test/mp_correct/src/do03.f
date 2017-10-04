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

!	parallel do with static schedule

	program p
	 implicit none
	 integer n
	 parameter(n=10)
	 real a(0:n),b(n),x(n),y(n)
	 real result(2*n)
	 real expect(2*n)
	 data expect/1.,3.,5.,7.,9.,11.,13.,15.,17.,19.,
     &		     1.,2.,3.,4.,5.,6.,7.,8.,9.,10./
	 integer i
	 do i = 0,n
	  a(i) = 2*i
	 enddo
	 do i = 1,n
	  x(i) = i*i
	 enddo
	 call sp2(a,b,x,y,n)
	 !print *,b
	 !print *,y
	 do i = 1,n
	  result(i) = b(i)
	  result(n+i) = y(i)
	 enddo
	 call check(result,expect,n)
	end

	subroutine sp2(a,b,x,y,n)
	 implicit none
	 integer n
	 real a(0:n),b(n),x(n),y(n)
	 integer i
!$omp   parallel
!$omp   do schedule(static)
	do i = 1,n
	 b(i) = (a(i) + a(i-1)) / 2.0
	enddo
!$omp   end do
!$omp   end parallel
	end
