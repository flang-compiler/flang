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

*   Exponentiation (** operator) to real, double, or complex powers.

	program p
	implicit complex (c), real*8 (d)

	integer rslts(18), expect(18)

	integer ctoi
	ctoi(c) = real(c) + aimag(c)

	data x2 / 2.0/, d2 / 2.0d0/, i2 / 2 /
	data c2, c23, c11, c12/ (2, 0), (2, 3), (1, 1), (1, 2) /

c --- tests 1 - 6:  exponentiation to real power:

	rslts(1) = 2.1 ** 2.0
	rslts(2) = 2.1 ** x2
	rslts(3) = (3 ** x2) * 100.001
	rslts(4) = (i2 + 1) ** 3.01
	rslts(5) = i2 ** (x2 + 1) + .01
	rslts(6) = x2 ** 0.0 * 100

c --- tests 7 - 12: exponentiation to double prec. power:

	rslts(7) = 2.1d0 ** 2.0d0
	rslts(8) = 2.1d0 ** x2
	rslts(9) = (3 ** d2) * 100.001
	rslts(10) = (x2 + 1) ** 3.01d0
	rslts(11) = i2 ** (d2 + 1) + .01
	rslts(12) = d2 ** 0.0 * 100

c --- tests 13 - 18:  exponentiation to complex power:

	rslts(13) = ctoi( (2.1, 0.0) ** (2.0, 0.0) )
	rslts(14) = ctoi( (2.1, 0.0) ** c2)
	rslts(15) = ctoi( c23 ** x2 + .001 )
	rslts(16) = c23 ** c11 + 10
	rslts(17) = ctoi( 10 ** c12 )
	rslts(18) = ctoi(x2 ** (-1.0, 2.0) * 10)

c --- check results:

	call check(rslts, expect, 18)

	data expect / 4, 4, 900, 27, 8, 100,
     +                4, 4, 900, 27, 8, 100,
     +                4, 4, 7, 9, -11, 5      /

	end
