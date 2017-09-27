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

*   Nested function calls.


	function iadd4(j, k, l, m)
c       (this function shows a certain bug which only happens if this is
c        the first function in the source file.)
	iadd4 = j + k
	iadd4 = iadd(l + m, iadd4)
	end


	program p
	parameter(n = 9)
	common rslts(n)
	integer rslts, expect(n)

	data i1, i3, i7, i20 / 1, 3, 7, 20 /
	data expect / 127, 129, -126, 351, 22,
     +                7, 160, 161, -2          /

c -- tests 1 - 5:

	rslts(1) = iadd(i1, iadd(i20, iadd(-i7, 
     +               iadd(-i7, iadd(100, i20))     )))
	rslts(2) = iadd(iadd(iadd(iadd(iadd(100, i20), -i7), -i7),
     +               i20), i3)
	rslts(3) = iadd4(iadd(-1, -2), iadd(-1, -2), -i20, -100)
	rslts(4) = iadd(iadd(iadd(3,i20), iadd(3,i20)),
     +                  iadd(iadd(i1+i3, 100), iadd(100+i1, 100)) )
	rslts(5) = iadd(2, i20) + iadd(0,0)

c --- tests 6 - 9:

c      use rslts(6)  to count number of calls actually made to isub:
	rslts(6) = 0
	rslts(7) = isub(100, i20) + isub(100, i20)
	rslts(8) = isub(isub(100, i20), isub(i20,100)) + 1
	rslts(9) = isub(i1 + 100, i20) - isub(i3 + 100, i20)

c --- check results:

	call check(rslts, expect, n)
	end


	integer function iadd(j, k)
	iadd = j + k
	end


	function isub(i, j)
	common ir(9)

	isub = i - j
	ir(6) = ir(6) + 1
	end
