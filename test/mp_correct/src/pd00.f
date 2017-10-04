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

*   Parallel do - static schedule, with/without chunk size
	program test
	parameter (NTESTS=9)
	parameter (N=37)
	integer iarr(N)
	integer result(NTESTS), expect(NTESTS)

	call fill(iarr, N)

	result(1) = isum1(iarr, N, 1, 1, 1)
	result(2) = isum2(iarr, N, 1, 1, 1)
	result(3) = isum3(iarr, N, 1, 1, 1)
	result(4) = isum4(iarr, N, 1, 1, 7)
	result(5) = isum5(iarr, N, 1, 1, 11)
	result(6) = isum6(iarr, N, 1, 1, 13)
	result(7) = isum7(iarr, N, 1, 1, 17)
	result(8) = idecsum8(iarr, N, 1, -1, 23) ! decreasing stride
	result(9) = idecsum9(iarr, N, 1, -1, 5)  ! decreasing stride

	data expect /
     +    703, 703, 703, 703, 703,
     +    703, 703, 703, 703
     + /
	call check(result, expect, NTESTS)
	end
	subroutine fill(iarr, n)
	integer iarr(n)
	do i = 1, n
	    iarr(i) = i
	enddo
	end

	integer function isum1(iarr, n, init, istride, ichunk)
	integer iarr(n), ichunk, init, istride
	isum = 0
!$omp parallel
!$omp do		! static is the default
	do i = 1, n
!$omp critical
	    isum = isum + iarr(i)
!$omp endcritical
	enddo
!$omp endparallel
	isum1 = isum
	end

	integer function isum2(iarr, n, init, istride, ichunk)
	integer iarr(n), ichunk, init, istride
	isum = 0
!$omp parallel do schedule(static, 1)    ! static cyclic
	do i = 1, n
!$omp critical
	    isum = isum + iarr(i)
!$omp endcritical
	enddo
!$omp endparalleldo
	isum2 = isum
	end

	integer function isum3(iarr, n, init, istride, ichunk)
	integer iarr(n), ichunk, init, istride
	isum = 0
!$omp parallel do schedule(static, 7)
	do i = 1, n
!$omp critical
	    isum = isum + iarr(i)
!$omp endcritical
	enddo
!$omp endparalleldo
	isum3 = isum
	end

	integer function isum4(iarr, n, init, istride, ichunk)
	integer iarr(n), ichunk, init, istride
	isum = 0
!$omp parallel
!$omp do schedule (static, 7)
	do i = init, n, istride
!$omp critical
	    isum = isum + iarr(i)
!$omp endcritical
	enddo
!$omp endparallel
	isum4 = isum
	end

	integer function isum5(iarr, n, init, istride, ichunk)
	integer iarr(n), ichunk, init, istride
	isum = 0
!$omp parallel
!$omp do schedule (static, ichunk)
	do i = init, n
!$omp critical
	    isum = isum + iarr(i)
!$omp endcritical
	enddo
!$omp endparallel
	isum5 = isum
	end

	integer function isum6(iarr, n, init, istride, ichunk)
	integer iarr(n), ichunk, init, istride
	isum = 0
!$omp parallel
!$omp do schedule (static, ichunk)
	do i = init, n, istride
!$omp critical
	    isum = isum + iarr(i)
!$omp endcritical
	enddo
!$omp endparallel
	isum6 = isum
	end

	integer function isum7(iarr, n, init, istride, ichunk)
	integer iarr(n), ichunk, init, istride
	isum = 0
!$doacross mp_schedtype=interleave, chunk=ichunk
	do i = init, n, istride
!$omp critical
	    isum = isum + iarr(i)
!$omp endcritical
	enddo
	isum7 = isum
	end

	integer function idecsum8(iarr, n, init, istride, ichunk)
	integer iarr(n), ichunk, init, istride
	isum = 0
!$omp parallel
!$omp do schedule (static, ichunk)
	do i = n, init, -1		! decreasing stride
!$omp critical
	    isum = isum + iarr(i)
!$omp endcritical
	enddo
!$omp endparallel
	idecsum8 = isum
	end

	integer function idecsum9(iarr, n, init, istride, ichunk)
	integer iarr(n), ichunk, init, istride
	isum = 0
!$omp parallel
!$omp do schedule (static, ichunk)
	do i = n, init, istride		! decreasing stride
!$omp critical
	    isum = isum + iarr(i)
!$omp endcritical
	enddo
!$omp endparallel
	idecsum9 = isum
	end
