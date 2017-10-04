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

!       OpenMP Library Routines
!       omp_set_num_threads, omp_get_thread_num, omp_in_parallel

	integer num(8), is, n, NPR
	parameter(NPR = 3)
	include 'ompf.h'

	if (omp_in_parallel()) then
	     print *, 'error - should be serial'
	     stop 1
	endif
	call omp_set_num_threads(NPR)
!$omp parallel, private(n)
	n = omp_get_thread_num() + 1
	num(n) = n
!$omp endparallel
	if (omp_in_parallel()) then
	     print *, 'error - should be serial'
	     stop 2
	endif
	is = 0
	do i = 1, NPR
	   is = is + num(i)
	enddo
!	print *, is
	call check(is, 6, 1)
	end
