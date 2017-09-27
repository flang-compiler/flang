!* Copyright (c) 2005, NVIDIA CORPORATION.  All rights reserved.
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

! allocate temp for the assignment in 's' of the proper size

	subroutine s(a,n,m)
	 real a(:)
	 a(n:n-m+1:-1) = a(1:m)
	end

	program p
	 interface
	  subroutine s(a,n,m)
	   integer n,m
	   real a(:)
	  end subroutine
	 end interface
	 real a(10)
	 real exp(10)
	 data exp /1.,2.,3.,5.,4.,3.,2.,1.,9.,10./
	 do i = 1,10
	  a(i) = i
	 enddo
	 call s( a, 8, 5 )
	 !print *,a
	 call check(a,exp,10)
	end program
