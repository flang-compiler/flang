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

*   VMS old-style PARAMETER statements.

C   - parens must be omitted,
C   - type of the parameter must not be declared,
C   - constant determines the type, not the identifier.

	program bp20
	implicit character*26 (a-i)

	parameter i3 = 3, x25 = 2.5
	parameter in25 = -2.5, x6 = 6
	parameter true = .true.
	parameter f = .not. true
	parameter a = 'b', b = 'ABCDEFGHIJKLMNOP' //
     +			'QRSTUVWXYZ'
	parameter j = 1D-9   ! watch exponent wrt Newton's method
	parameter x = 'FFFFFFFF'x

	parameter XX = 10
	integer rslts(XX), expect(XX)

	rslts(1) = i3			! 3
	rslts(2) = 2.01 * x25		! 5
	rslts(3) = 2 * in25		! -5
	rslts(4) = (x6/4)*4		! 4

	if (true) rslts(5) = 2		! 2
	if (.not. f) rslts(6) = 3	! 3

	rslts(7) = ICHAR(a)		! 98
	c = b			! (c is implicitly declared)
	rslts(8) = ichar(c(25:25)) - ichar('Z')		! -1

	rslts(9) = nint(j * (1 / j ))	! 1.0  -->  1
	if (x) rslts(10) = x		! -1

	call check(rslts, expect, 10)
	data expect / 3, 5, -5, 4, 2, 3, 98, -1, 1, -1 /
	end
