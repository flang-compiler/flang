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

*   CHAR and ICHAR intrinsics.

C   NONSTANDARD:
C     Use of char constants to initialize numeric.
C     Hex and octal constants (VMS).

	program p
	parameter(n = 11)
	integer rslts(n), expect(n)
	character*4 crslts(n)
	equivalence (rslts, crslts)

	character*1 c1, c2*2, c3*3, c4(2)*4, cfunc

	data c1, c2, c3, c4 / 'a', 'ab', 'abc', 'xyzw', 'XYZW' /
	data i2, i3, i3x    / 2, 3, 3 /

c ----- tests 1 - 8:  ICHAR intrinsic:

	rslts(1) = ichar('\01')
	rslts(2) = ichar( char('377'o) )
	rslts(3) = ichar( char(i2) ) + ichar('a')
	rslts(4) = ichar(c1)
	rslts(5) = ichar( c2(2:2) )
	rslts(6) = ichar( c3(i2:i2) )
	rslts(7) = ichar( c4(i2)(i3:i3x) ) * 2
	rslts(8) = ichar( cfunc(3) )

c ----- tests 9 - 11:  CHAR intrinsic:

	crslts(9) = char('141'o)
	crslts(10) = char(ichar('a'))
	crslts(11) = char(0) // char(i3+i3x) // char(-1) //
     +                char(ifunc(3))

c ----- check results:

	call check(rslts, expect, n)
	data expect / 1, 255, '143'o, '141'o,
     +                '142'o, '142'o, '264'o, 4,
c BIG ENDIAN
c     +                'a   ', 'a   ', '0006FF04'x  /
c LITTLE ENDIAN
     +                'a   ', 'a   ', '04FF0600'x  /
	end
CCCCCCCCCCCCCCCCCC
	character*1 function cfunc(i)
	cfunc = char(i + 1)
	return
	end
CCCCCCCCCCCCCCCCCC
	integer function ifunc (i)
	ifunc = i + 1
	end
