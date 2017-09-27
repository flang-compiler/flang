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

*--- Induction alias use bug,
*    

	program p
	parameter (N=6)
	integer i, j, result(N), expect(N)
	common i, j, result, expect
	common /k/k
	data k/5/

	data expect /
     +   2, 3, 4, 5, 1, 5
     +  /

	call sub(result, N-1)
	result(6) = k

	call check(result, expect, N)
	end
	subroutine sub(ia, n)
	dimension ia(n)
	common /k/k
	j = 0
	do i = 1, n
	    ia(k) = i
	    j = j + 1	! def between use of k and def of k
	    k = j
	enddo
	end
