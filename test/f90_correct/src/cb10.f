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

*   Adjustable arrays and ENTRY statements.

C   NONSTANDARD:
C      Use of character constants to initialize numeric variable.

	parameter(n = 4)
	integer rslts(n), expect(n)
	character*4 c(3, 2), d(4, 3)
	equivalence (c(2, 1), rslts(3)), (d(1, 1), rslts(4))

	call sub1(rslts, 100, 10)
	call trash
	call e1(11, 12, rslts(2))

	call sub2(c, 3, 2)
	call trash
	call e2(d, 3, 4)

c --- check results:

	data expect / 10, 11, 'abcd', 'x   ' /

	call check(rslts, expect, n)
	end

ccccccccccccccccccccccccccccccccc

	subroutine sub1(a, n, m)
	entry e1(m, n, a)
	integer a(m:n)
	save i
	data i / 10 /

	a(i) = i
	i = i + 1
	end

ccccccccccccccccccccccccc

	subroutine sub2(a, n, m)
	character*(*) a(n, m)
	
	a(n-1, m-1) = 'abcdjksdhfkasdjfalfd'
	return

	entry e2(a, m, n)
	a(1, 1) = 'x'
	end

	subroutine trash()
	integer stk(20)
	do 10 i = 1, 20
	    stk(i) = -100
10	continue
	end
