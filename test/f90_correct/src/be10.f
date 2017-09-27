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

*   EQUIVALENCE statements - common block equivalences.

	implicit integer (a-z)

	common /c/ i, j, k, l
	equivalence (k, b(2))
	integer b(5)

	equivalence (d1, d(3))
	common /d/ d(6)
	equivalence (d2, d1), (d3, d(6)), (d4, d3), (d5, d)
	equivalence (d(6), d(6))

	common /e/ x
	common /f/ y
	equivalence (xx, x), (d6, d5), (y, yy)

c  -- tests 1, 2:  set 2nd and 6th elements of /c/ to 2 and 3:

	j = 11
	b(1) = b(1) - 9
	b(5) = 3

c  -- tests 3, 4, 5:  set 1st, 3rd, and 6th elements of /d/
c                     to 4, 5, and 6:

	d5 = 4
	d2 = 4
	d(3) = d1 + 1
	d4 = 6

c  -- tests 6, 7:  set /e/ and /f/ to 7 and 8:

	xx = 7
	yy = xx + 1

c  -- check results:

	call mycheck
	end


	subroutine mycheck
	implicit integer (a-z)
	integer rslts(7), expect(7)

	common /c/ c(6)
	common /d/ d(6)
	common /e/ e(1)
	common /f/ f(1)

	rslts(1) = c(2)
	rslts(2) = c(6)
	rslts(3) = d(1)
	rslts(4) = d(3)
	rslts(5) = d(6)
	rslts(6) = e(1)
	rslts(7) = f(1)

	call check(rslts, expect, 7)
	data expect / 2, 3, 4, 5, 6, 7, 8 /
	end
