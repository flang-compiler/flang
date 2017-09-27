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

*   Uses of integer*2 which are endian-dependent

	integer N
	parameter (N = 2)
	integer result(N), expect(N)
	integer*2 a(2)
	integer fun
	data a/1, 2/

	result(1) = fun(a(1) + a(2))

	result(2) = fun(iint(2))	! need special intrin (int2) ?


c ******* check results:

	call check(result, expect, N)

	data expect /
     +  3, 2
     +  /
	end
	integer function fun(arg)
	integer*2 arg	! addr of correct half of arg passed ?
	fun = arg
	end
