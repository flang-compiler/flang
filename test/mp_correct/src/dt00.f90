! Copyright (c) 1990-2008, NVIDIA CORPORATION.  All rights reserved.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

! local derived types with allocatable components are not
! recursive-/thread- safe

	integer ii
	integer omp_get_thread_num
	integer results(4)
	integer expect(4)
	data expect /1, 2, 1, 2/
	call omp_set_num_threads(2)
!$omp parallel, private(ii), shared(results)
	ii = omp_get_thread_num()+1
	call sub(ii, results)
!$omp endparallel
	call check(results, expect, 4)
	end
	subroutine sub(n, r)
	integer r(4)
	type aa
	    integer mem
	    real, dimension(:,:),allocatable :: array
	endtype aa
	type bb
	    integer mem
	    real, dimension(:,:),pointer :: array
	endtype bb
	type (aa)xx
	type (bb)yy
	xx%mem = n
	yy%mem = n
!$omp barrier
!	print 99, '3 identical values:',n,xx%mem,yy%mem
!99	format(a, 3i4)
	r(n) = xx%mem
	r(n + 2) = yy%mem
	end
