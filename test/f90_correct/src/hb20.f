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

*   DO loops - special cases:
*      (1) control flow statements as last statement.

	parameter(n = 6)
	integer rslts(n), expect(n)

	do 10 j = 1, 1
		do 10 i = 1, 4
	data (expect(i), i = 1, n) / 2, -2, 4, -4, 101, 104 /
			if (and(i, 1)) then
				rslts(i) = i + 1
			else
				rslts(i) = - i
10			endif


	rslts(5) = 0
	do 20 i = 1, '7fffffff'x
		rslts(5) = rslts(5) + 1
20		goto(30, 40) i + 1
30	rslts(5) = rslts(5) + 10
40	rslts(5) = rslts(5) + 100


	rslts(6) = 0
	do 50 i = -999999, -999991
		rslts(6) = rslts(6) + 1
50		call sub(*60, *70)
60	rslts(6) = rslts(6) + 10
70	rslts(6) = rslts(6) + 100


	call check(rslts, expect, n)
	end

c-------------

	subroutine sub(*, *)
	save c
	integer c
	data c / 0 /
	c = c + 1
	if (c .eq. 4)  return 2
	end
