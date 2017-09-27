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

*   Induction processing bug.
*   tests an opt 2 induction bug fix (addition of def_in_out)

	program ka37
	integer i, j, k, r, h, res(0:299), a
	integer result(1), expect(1)
	data expect /24/

	h = 0
	do while (h .lt. 300)
	    res(h) = h
	    h = h + 1
	enddo
	k = 0
	i = 0
	r = 0
99	continue
	j = 0
	if (r .eq. 2) goto 1000

	do while (j .lt. 10)
	    k = 0
990	    continue
		j = j + 1
	    if (j .lt. 4) goto 990
991         continue
		k = k + 1
992             continue
		    j = j + 1
		if (j .lt. 4) goto 992
		i = i + 1
		a = res(i)
	    if (k .lt. 12) goto 991
	    j = j + 1
	enddo

	r = r + 1
	goto 99

1000	continue
	result(1) = a
	call check(result,expect,1)
	end
