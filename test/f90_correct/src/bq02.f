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

*   adjustable POINTER object and ALLOCATE statement

	parameter (N=1)
	integer result(N)
	integer expect(N)

  	maxdat=1000
	call glim3(maxdat, result)

	data expect /1/
	call check(result, expect, N)

	end
	subroutine glim3(imxdat, ires)
	double precision sspmat
	integer imxdat
	pointer (l_sspmat, sspmat(imxdat))

c  ensure ilms are written for sspmat's array descriptor 

	data kount/0/, l2dir/0/	 ! cause ilms to be erased

	maxdat=imxdat
	allocate(sspmat, stat=jallo)
	if ( jallo .eq. 0 ) goto 100
	ires = 2
	goto 9999
 100    do 18 i=1,maxdat
	   sspmat(i)=0.d0
  18    continue
	ires = 1
	deallocate(sspmat)
 9999   continue
	return
	end
