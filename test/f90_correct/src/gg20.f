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

*   Exponentiation (** operator) to double complex powers.

	program p
	implicit double complex (c)

	parameter (N=6)
	integer rslts(N), expect(N)

	integer ctoi
	ctoi(c) = real(c) + dimag(c)

	data x2 / 2.0/
	data c2, c23, c11, c12/ (2, 0), (2, 3), (1, 1), (1, 2) /

	rslts(1) = ctoi( (2.1d0, 0.0) ** (2.0d0, 0.0) )
	rslts(2) = ctoi( (2.1, 0.0d0) ** c2)
	rslts(3) = ctoi( c23 ** x2 + .001 )
	rslts(4) = c23 ** c11 + 10
	rslts(5) = ctoi( 10 ** c12 )
	rslts(6) = ctoi(x2 ** (-1.0d0, 2.0) * 10)

c --- check results:

	call check(rslts, expect, N)

	data expect /
     +                4, 4, 7, 9, -11, 5      /

	end
