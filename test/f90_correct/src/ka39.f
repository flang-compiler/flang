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

*--- Induction uses which are of the form <invar> - i and -i
*    

	program p
	parameter (N=8)
	integer i, j, result(N), expect(N)
	common i, j, result, expect

	data expect / 1, 2, 3, 4, -4, -3, -2, -1/

	call t1(result, 4)

	call t2(result(5), 4)

	call check(result, expect, N)
	end

	subroutine t1(iarr, iup)
	dimension iarr(iup)
	do i = 1, iup
	    iarr((iup + 1) - i) = (iup + 1) - i
	enddo
	end

	subroutine t2(iarr, iup)
	dimension iarr(-4:-1)
	do i = 1, iup
	    iarr(-i) = -i
	enddo
	end
