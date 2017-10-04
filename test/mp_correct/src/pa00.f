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

*   Parallel/endparallel directives
*   Lexically nested
	program test
	common/result/result
	integer result(21), expect(21)
	result(1) = 1
	call sub(10)
!	print *, result
	data expect /
     +      1,    2,    6,   12,   20,   30,
     +     42,   56,   72,   90,  110,  108,
     +    104,   98,   90,   80,   68,   54,
     +     38,   20,  100
     + /
	call check(result, expect, 21)
	end
	subroutine sub(n)
!$omp parallel default(shared)
!$omp    do
 	 do i = 1, n
!$omp       parallel shared (i, n)
!$omp          do
               do j = 1, n
		   call work(i,j)
	       enddo
!$omp       endparallel
	 enddo
!$omp endparallel
	end
	subroutine work(i,j)
	common /result/result
	integer result(21)
!$omp critical
	result(i+j) = result(i+j) + i + j
	result(21) = result(21) + 1
!$omp endcritical
	end
