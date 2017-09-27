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

*   VMS STRUCTURE/RECORD

c  test structure with and without initializations
c  test record references

	program br10
	parameter (N=6)
	integer expect(N), result(N)

	structure /stra/
	    integer a1 /1/
	    character*1 a2 /'a'/
	    integer a3 /2/
	endstructure
	
	structure /strb/
	    integer b1 /11/
	    record /stra/ attr
	    character*1 b3
	    integer b4
	endstructure

	record /strb/ recb

	result(1) = recb.attr.a1
	result(2) = ichar(recb.attr.a2)
	result(3) = recb.attr.a3
	result(4) = recb.b1

	recb.b4 = recb.attr.a1 + recb.attr.a3
	call cset(recb.b3)

	result(5) = recb.b4
	result(6) = ichar(recb.b3)

	call check(result, expect, N)

	data expect /1, 97, 2, 11, 3, 98/

	end
	subroutine cset(c)
	character*(*) c
	c = 'b'
	end
