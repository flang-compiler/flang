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

*   POINTER statements, character objects

	parameter(N = 16)
	integer result(N), expect(N)
	character*8 buf
	pointer(p1, idum)
	pointer(p2, jdum)

	p1 = loc(buf)
	call sub1(p1)
	do i = 1, 8
	    result(i) = ichar(buf(i:i))
	enddo

	p2 = loc(buf)
	call sub2(p2)
	do i = 1, 8
	    result(i+8) = ichar(buf(i:i))
	enddo

	data expect/
     +    97, 98, 99,100,101,102,103,104,
     +    49, 50, 51, 52, 53, 54, 55, 56
     +  /
	call check(result, expect, N)
	end
	subroutine sub1(p1)
	character*8 ch
	pointer(p1, ch)
	ch = 'abcdefgh'
	end
	subroutine sub2(p2)
	character*4 ach(2)
	pointer(p2, ach)
	ach(2) = '5678'
	ach(1) = '1234'
	end
