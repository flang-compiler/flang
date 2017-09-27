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

*   Intrinsics and generics: AIMAG and CONJG.

	program p
	implicit complex (c)
	parameter (n = 17 )
	real rslts(n), expect(n)
	complex crslts(4)
	equivalence (crslts, rslts(10))

	complex ca(3)
	data c12 / (1.0, 2.0) /
	data x3, i2 / 3.0, 2 /
	data ca / (1.0, 2.0), (3.0, 4.0), (5.0, 6.0) /

	rslts(1) = aimag(c12)
	rslts(2) = aimag( (3.5, -5.1e26) )
	rslts(3) = aimag(c12 + c12)
	rslts(4) = 2.0 * aimag(c12 * 5.0)
	rslts(5) = aimag( ca(i2) ) + x3
	rslts(6) = aimag( conjg(c12) )
	rslts(7) = conjg(c12 + c12)

c      complex function cf returns successive values:
c        (10,-10), (20,-20), (30,-30), ...

	rslts(8) = aimag(cf())
	rslts(9) = aimag( conjg(cf()) )

	crslts(1) = conjg((1.2, -2.3))
	crslts(2) = -conjg(c12 + c12)
	crslts(3) = c12 * conjg(ca(1))
	crslts(4) = conjg(cf()) + c12

c ---- check results:

	call check(rslts, expect, n)
	data expect / 2.0, -5.1e26, 4.0, 20.0, 7.0, -2.0, 2.0,
     +                -10.0, 20.0,
     +                1.2, 2.3,        -2.0, 4.0,
     +                5.0, 0.0,        31.0, 32.0             /
	end


	complex function cf()
	common c
	complex c
	data c / (10.0, -10.0) /
	cf = c
	c = c + (10.0, -10.0)
	return
	end
