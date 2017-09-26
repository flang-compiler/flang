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

*--- Induction uses, removal of initval def bug
*    

	program p
	parameter (N=2)
	integer i, j, result(N), expect(N)
	common i, j, result, expect

	data expect /
     +   11, 1
     +  /

	result(1) = 99
	result(2) = 99
	call sub(10, result)
	call sub(0, result)
	call check(result, expect, N)
	end
	subroutine sub(n, ir)
	dimension ir(*)
	do i = 1, n	! can't delete init def of i (i = 1)
	enddo
	call foo(i, ir)
	end
	subroutine foo(i, ir)
	dimension ir(*)
	common /k/k
	data k /1/

	ir(k) = i
	k = k + 1
	end
