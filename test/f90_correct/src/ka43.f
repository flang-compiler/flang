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

*--- Induction alias uses
*    

	program p
	parameter (N=27)
	integer i, j, result(N), expect(N)
	common i, j, result, expect

	data expect /
     +  1, 2, 3, 4,			! t1 (1-4)
     +  1, 2, 3, 4, 3,			! t2 (5-9)
     +  2, 3, 6, 9,			! t3 (10-13)
     +  2, 3, 6, 9, 3,			! t4 (14-18)
     +  2, 3, 6, 9, 3,			! t5 (19-23)
     +  1, 2, 3, 4			! t6 (24-27)
     +  /

	call t1(result, 4)

	call t2(result(5), 4)

	call t3(result(10), 4)

	call t4(result(14), 4)

	call t5(result(19), 4)

	call t6(result(24), 4)

	call check(result, expect, N)
	end

	subroutine t1(iarr, iup)
	integer iarr(*)
	do i = 1, iup, 2
	    k = i
	    iarr(k) = i
	    iarr(k+1) = i + 1
	enddo
	end

	subroutine t2(iarr, iup)
	integer iarr(*)
	do i = 1, iup, 2
	    k = i
	    iarr(k) = i
	    iarr(k+1) = i + 1
	enddo
	iarr(iup+1) = k
	end

	subroutine t3(iarr, iup)
	integer iarr(*)
	do i = 0, iup - 1, 2
	    k = i + 1
	    iarr(k) = 2*k
	    iarr(k+1) = 3*k
	enddo
	end

	subroutine t4(iarr, iup)
	integer iarr(*)
	do i = 0, iup - 1, 2
	    k = i + 1
	    iarr(k) = 2*k
	    iarr(k+1) = 3*k
	enddo
	iarr(iup + 1) = k
	end

	subroutine t5(iarr, iup)
	integer iarr(*)
	do i = 0, iup - 1, 2
	    k = i + 1
	    iarr(k) = 2*k
	    iarr(k+1) = 3*k
	enddo
	call set(iarr(iup + 1), k)
	end

	subroutine set(ir, iv)
	ir = iv
	end

	subroutine t6(iarr, iup)
	integer iarr(*)
	do i = 0, iup - 1, 2
	    k = i + 1
	    iarr(i + 1) = k
	    iarr(i + 2) = k + 1
	enddo
	end

