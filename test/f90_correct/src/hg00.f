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

*   ASSIGN statements and assigned GOTO's.
*   (ASSIGN of FORMAT labels is not tested).

	program p
	common j, rslts(6)
	integer j, rslts, expect(6)
	data i3 / 3 /

	assign 10 to int
	if (i3 .eq. 3)  assign 20 to int
10	goto int
	rslts(1) = 1
20	rslts(2) = 1

	assign 20 to j
	if (i3 .lt. 3)  goto j (20)
	assign 99999 to j
	if (.not. (i3 .ne. 3))  goto j, (99999, 20)
	rslts(3) = 1
99999	rslts(4) = 1

	call sub(k)

	call check(rslts, expect, 6)
	data expect / 0, 1, 0, 1, 0, 1 /

	end


	subroutine sub(k)
	common j, rslts(6)
	integer rslts

1	assign 1 to k
	assign 10 to k
	goto k (1, 10)
	rslts(5) = 1
10	rslts(6) = 1

	end
