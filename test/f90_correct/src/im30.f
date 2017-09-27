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

*   Troublesome statement functions

	program p
	parameter(N=1)
	common /exp/result, expect
	integer result(N)
	integer expect(N)

	data expect / 1 /

	result(1) = if1()

c  --- check the results:

	call check(result, expect, n)
	end

	integer function if1()
	common /fast/ a(100)
	real*8 a
	real*8 x1wxyz
	integer istuff
	locf(x1wxyz) = ishft(%loc(x1wxyz),-3)
	istuff = locf(a(1))
	if (istuff .ne. ishft(%loc(a(1)),-3)) then
	    if1 = 99
	else
	    if1 = 1
	endif
	end
