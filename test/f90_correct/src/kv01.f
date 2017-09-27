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

* Vectorizer - decremented induction variables and streaming
*              iscbugs 911004g.f & 911004h.f

	program p
	parameter (N=4)

	real result(N)
	real expect(N)

	dimension c(N)
	common pad1(100), c, pad2(100)
	data result /N*0.0/
	data c /N*1.0/
	data expect / N*4.0/

	call svpoly(N, result, c, 1, n+1, 1.0)
	call check(result, expect, N)
	end

	subroutine svpoly(m, z, c, incc, iic, s)
	dimension z(1), c(1)
	do iz = 1, m
	    ic = iic
	    do j = m, 1, -1
		ic = ic - incc
		z(iz) = s*z(iz) + c(ic)
	    enddo
	enddo
	end
