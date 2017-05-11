*
* Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*

! RUN: %clang -c %S/check.c -o %t1
! RUN: %flang -c -I%S -lm %s -o %t2
! RUN: %flang -I%S -lm %t2 %t1 -o %t3 
! RUN: %t3 | tee %t4 &&  grep '  6 tests completed. 6 tests PASSED. 0 tests failed.' %t4

*   Intrinsics and generics: trigonometric and hyperbolic functions.

	program p
	implicit complex (c), double precision (d), double complex(z)
	parameter(n=6)
	integer rslts(n), expect(n), ctoi, dtoi
	parameter (d_dr=0.174532925199432957692D-1)
	parameter (r_dr=0.174532925199432957692E-1)
	parameter (d_rd=0.572957795130823208769D+2)
	parameter r_rd=0.572957795130823208769E+2

	dtoi(d) = d * 1000 + .499
	ctoi(c) = 1000 * (real(c) + aimag(c))
	ztoi(z) = 1000 * (real(z) + dimag(z))
	d_dtor(d) = d_dr*d
	d_rtod(d) = d_rd*d
	r_dtor(r) = r_dr*r
	r_rtod(r) = r_rd*r

	data x3, xx3 / 2 * 3.0 /,  d1, cx / 1.0d0, (1.0,2.0) /
	data zx/(1.0d0,2.0d0)/


	rslts(1) = nint(acos(1.0) * 100)
	rslts(2) = 100 * dacos(.93969 * 1d0) + .2
	rslts(3) = acos(1.0d0) + x3 + .01
	rslts(4) = nint(r_dtor(acosd(1.0)) * 100)
	rslts(5) = 100 * d_dtor(dacosd(.93969 * 1d0)) + .2
	rslts(6) = d_dtor(acosd(1.0d0)) + x3 + .01


c --- check results:

	call check(rslts, expect, n)

	data expect / 0, 35, 3, 0, 35, 3 /
	end
