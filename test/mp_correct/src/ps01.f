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

*   SECTIONS/ENDSECTIONS
*   Dynamically enclosed within a parallel region
	program test
	common /result/result
	integer result(5), expect(5)
	call sub
!$omp   parallel
	call sub
!$omp   endparallel
!	print *, result
!	print *, expect
	data expect /2, 2, 2, 2, 2/
	call check(result, expect, 5)
	end
	subroutine sub
	common /result/result
	integer result(5)
!$omp   sections
        result(1) = result(1) + 1
	call print(0)
!$omp   section
        result(2) = result(2) + 1
	call print(1)
!$omp   section
        result(3) = result(3) + 1
	call print(2)
!$omp   section
        result(4) = result(4) + 1
	call print(3)
!$omp   section
        result(5) = result(5) + 1
	call print(4)
!$omp   endsections
	end
	subroutine print(n)
	integer omp_get_thread_num
!	print *, 'section:', n, ' thread:', omp_get_thread_num()
	end
