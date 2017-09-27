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

*   Assumed size arrays.

	program p
	integer rslts(7), expect(7), f

	real a(2,2,2,2,2,2,10:11)
	common b(3:5, -5:-2)

	call sub(rslts, a)
	rslts(2) = a(1,1,1,1,1,1,10)
	rslts(3) = a(2,2,2,2,1,2,11)

	b(2, -4) = 3.1
	rslts(4) = f(3,5,b)
	rslts(5) = b(4, -4)
	rslts(6) = b(3, -2)

	call check(rslts, expect, 7)
	data expect / 2, 8, -1, 6, -2, 4, 4 /
	end
c--------------
	subroutine sub(ar, aa)
	dimension ar(*)
	integer ar
	real aa(2,2,2,2,2,2,10:*)

	data i1, i11 / 1, 11 /

	ar(1) = 2
	ar(7) =  4

	aa(1,1,1,1,1,1,10) = 8.0
	aa(2,2,2,2,i1,2,i11) = -1.0

	return
	end
c--------------
	integer function f(n, m, b)
	real b(n:m, -m:*)

	b(4, -4) = -2.1
	b(3, -2) = 4.01
	f = b(2, -4) + b(2, -4)
	end
