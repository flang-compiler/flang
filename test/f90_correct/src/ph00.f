** Copyright (c) 1992, NVIDIA CORPORATION.  All rights reserved.
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

*   KANJI - NCHARACTER declarations, used in IMPLICIT, NLEN intrinsic.

	implicit ncharacter*17 (i-k)

	ncharacter a, b*2, c
	ncharacter *171e, f*6
	ncharacter*(16) g*3, h(10), i
	ncharacter*10,j

	integer i3, i6
	integer expect(11), rslts(11)

	data i3, i6 / 3, 6/
	data expect /	1, 2, 1, 1000, -171,
     +			12, 3002, 16, 16, 34,
     +			4 /

	rslts(1) = len(a) 
	rslts(2) = len(b)
	rslts(3) = nlen(c)
	rslts(4) = nlen(j) * 100
	rslts(5) = -nlen(e)

	rslts(6) = nlen(f // f)
	rslts(7) = (1000 * nlen(g)) + nlen(g(2:3))
	rslts(8) = len( h(nlen(b)) )
	rslts(9) = max(len(i), -20)
	rslts(10) = nlen(ii) + nlen(k)

	rslts(11) = nlen( h(i3)(i3:i6) )

	call check(rslts, expect, 11)

	end
