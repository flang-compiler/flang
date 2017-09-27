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

*   SAVE statements in conjunction with EQUIVALENCES.

C   NONSTANDARD:
C      Use of character strings to initialize numeric variable.

	program p
	parameter(n = 10)
	save
	integer rslts(n), expect(n)

c --- tests 1 - 6:

	call sub1(.true.,  rslts(1))
	call sub1(.false., rslts(4))

c --- tests 7 - 10:

	call sub2(rslts(7))
	call sub2(rslts(9))

c --- check results:

	call check(rslts, expect, n)

	data expect / 12, 'abcd', 2, 13, 'axcd', 5,
     +                'abcd', 7, 23, 'xyzw'        /
	end

c ccccccccccccccccccccccccccccccccccccccc

	subroutine sub1(flag, ir)
	save
	equivalence (a, b), (i, j), (c(2:2), d), (c, k)
	logical flag
	integer ir(*)
	character*4 c, d*1
	integer*2 i, j

	if (flag) then
	    m = 2
	    i = 10
	    c = 'abcd'
	    b = 2.6
	else
	    i = j + 1
	    d = 'x'
	    a = a * 2.0
	endif

	ir(1) = j + m
	ir(2) = k
	ir(3) = int(a)
	end

ccccccccccccccccccccccccccccccccccccc

	subroutine sub2(ir)
	integer b, ir(*)
	equivalence (a, b), (c, d), (i, j), (m, n)
	save b, i
	character*5 j, a, c, n

	data a / 'abcde'/, i / 7 /

	ir(1) = b
	ir(2) = i
	j = 'xyzwv'
	b = 23
	end
