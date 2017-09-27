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

*   Miscellaneous Optimizer bugs.

	program p
	parameter(n = 1)
	integer rslts(n), expect(n)
	data rslts / n * 0 /

	data i1 / 1 /

C   test 1:  top of while loop (10) target of goto:

	data expect(1) / 3 /
*if (i1 .eq. 1)  goto 10
*	return
10	if (i1 .gt. 1)  goto 20
		rslts(1) = rslts(1) + 3
		i1 = i1 + 1
		goto 10
20	continue

c   check results:

	call check(rslts, expect, n)
	end

