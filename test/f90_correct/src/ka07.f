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

*   Optimizer bug: flowgraph altered due to zerotrip do statement

	program ka20
	parameter (N=1)
	integer result(N), expect(N)
	data expect/11/
	integer a(100),b(100)
	integer zloop1
	a = 0
	b = 0
	result(1) = zloop1(a,b,100)
	call check(result, expect, N)
	end
	integer function zloop1(a,b,n)
	integer a(n), b(n)
	call copy(0, m)
	it = 11			! should have a use after next do loop
	do i = 1, m		! loop isn't executed
	    it = a(i) + 1
	    b(i) = it + 1
	enddo
	zloop1 = it
	end
	subroutine copy(n,m)
	m = n
	end
