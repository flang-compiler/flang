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

* Constant prop. - call in path problems

	program p
	parameter (N=1)
	common iresult(N)
	dimension iexpect(N)
	data iexpect /
     +    9			! t0
     +  /

	iresult(1) = 0
	call t0(iresult(1), 5)

	call check(iresult, iexpect, N)
	end

	subroutine t0(ii, n)
	jj = 1			!jj cannot be copied
	do i = 1, n
	    ii = jj + ii
	    call set(jj)	!jj potentially modified
	enddo
	end
	subroutine set(jj)
	jj = 2
	end
