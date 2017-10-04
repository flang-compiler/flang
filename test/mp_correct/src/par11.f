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

!	OpenMP Parallel Region
!	parallel private subroutine call

	program p
	parameter(n=10)
	integer result(n)
	integer expect(n)
	data expect/101,201,301,401,102,202,302,402,103,203/
	call sp1(result,n)
	!print *,result
	call check(result,expect,n)
	end

	subroutine sp1(x,n)
	 integer n
	 integer x(n)
	 integer omp_get_thread_num
	 integer omp_get_num_threads
!$omp   parallel private(iam,np,ipoints)
	 iam = omp_get_thread_num()+1
	 np = omp_get_num_threads()
	 call subdomain(x,iam,n,np)
!$omp   end parallel
	end

	subroutine subdomain(x,iam,n,np)
	integer n
	integer x(n),iam,np
	integer i,j
	j = 0
	do i = iam,n,np
	 j = j + 1
	 x(i) = iam*100 + j
	enddo
	end
