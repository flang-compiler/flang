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

*   Optimizer tests of converting while loops to "do while"

	program ka01
	parameter(N=7)
	common iresult(N), iexpect(N)
	data iresult /N*0/
	call test1_2
        call test3_7
        call check(iresult, iexpect, N)
	data iexpect /
     +    4, 5,                         ! tests 1-2
     +    4, 4, 4, 4, 4                 ! tests 3-7
     +  /
	end

	subroutine incr(i)
	parameter(N=7)
	common iresult(N), iexpect(N)
	iresult(i) = iresult(i) + 1
	end

	subroutine test1_2()
	i = 1
	do 100 while (i .le. 4)
	    call incr(1)
	    i = i + 1
100	continue
	do while (i .gt. 0)
	    call incr(2)
	    i = i - 1
	enddo
	end

	subroutine test3_7()
	i = 4
	do 100 while (i .ge. 1)
	    k = 4
	    call incr(3)
	    do while (k .gt. 0)
		j = k
		call incr(3 + j)
		k = k - 1
	    enddo
	    i = i - 1
100     continue
	end
