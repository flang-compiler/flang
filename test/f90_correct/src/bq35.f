** Copyright (c) 1989, NVIDIA CORPORATION.  All rights reserved.
**
** Licensed under the Apache License, Version 2.0 (the "License");
** you may not use this file except in compliance with the License.
** You may obtain a copy of the License at
**
**     http://www.apache.org/licenses/LICENSE-2.0
**
** Unless required by applicable law or agreed to in writing, software
** distributed under the License is distributed on an "AS IS" BASIS,
** WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
** See the License for the specific language governing permissions and
** limitations under the License.

*   Allocatable arrays

	program bq35
	parameter (NT=24)
	common m,mm,n,nn
	data m/1/, mm/3/, n/1/, nn/5/
	integer result(3,8), expect(3,8)
	common /result/result
	data expect /
     +   1, 2, 3, 2, 4, 6, 3, 6,
     +   9, 4, 8, 12, 5, 10, 15, 6,
     +   12, 18, 7, 14, 21, 8, 16, 24 /

	call sub()
	call check(result, expect, NT)
	end
	subroutine sub()
	common m,mm,n,nn
	allocatable ia(:,:)
	allocate (ia(2,2))
	nn = 8
	deallocate (ia)
	allocate (ia(m:mm,n:nn))
	do i = m, mm
	    do j = n, nn
		ia(i,j) = j*i
	    enddo
	enddo
	call pr(ia)
	end
	subroutine pr(ia)
	dimension ia(3,8)
	integer result(3,8)
	common /result/result
	do i = 1, 3
	    do j = 1, 8
		result(i,j) = ia(i,j)
	    enddo
	enddo
	end
