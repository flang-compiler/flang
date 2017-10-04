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

*   Scope of private variables
*   DO - similar to ca00, but DO is within the dynamic extent of a parallel
*        region.

	program test
        implicit none
        integer omp_get_num_threads, omp_get_thread_num
	integer i, n
	parameter(n = 10)
	integer j, k, aa(n), nthreads
	integer result(n+2), expect(n+2)
	j = 99
	k = 1
!$omp parallel shared(nthreads)
        if (omp_get_thread_num() .eq. 0) nthreads=omp_get_num_threads()
	call sub(j, k, n, aa)
!$omp endparallel
	result(1) = j
	result(2) = k
	do i = 1, n
	    result(i+2) = aa(i)
	enddo

	data expect/99, 1,			! j & k after call
     +     1, 2, 3, 4, 5, 6, 7, 8, 9, 10/	! aa after call
        expect(1) = expect(1) + nthreads
	call check(result, expect, n+2)
	end
	subroutine sub(j, k, n, aa)
	integer aa(n)
!$omp do, private(j), firstprivate(k)
	do k = k, n
	    j = k		! j & k are not the dummy variables
	    aa(k) = j
	enddo
!$omp enddo
!$omp critical
	j = j + 1
!$omp endcritical
	end
