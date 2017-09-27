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

*   EXTERNAL statements.

c  Items tested include:
c  (1) blockdata name in EXTERNAL statement.
c  (2) redefinition of an intrinsic using EXTERNAL statement.
c  (3) name of function which is to be passed as a dummy
c      argument in EXTERNAL stmt.
c  (4) name of dummy function in EXTERNAL statement.
c  (5) use of a dummy function as a subroutine (CALLed) and
c      as a function.

	EXTERNAL blockdat, ifunc
	integer rslts(4), expect(4)
	common rslts, expect
	external iabs
	external sub3

	rslts(1) = iabs(3)
	call sub(ifunc, ifunc, sub3)

	call check(rslts, expect, 4)
	end
c-----------------------------------------c
	blockdata blockdat
	integer rslts(4), expect(4)
	common rslts, expect

	data expect / 4, 19, 99, -1 /
	end
c-----------------------------------------c
	integer function iabs(j)
	iabs = j + 1
	return
	end
c-----------------------------------------c
	integer function ifunc(i)
	ifunc = i - 1
	return
	end
c-----------------------------------------c
	subroutine sub(if, jf, kf)
	integer rslts(4), expect(4)
	common rslts, expect
	external if, jf, kf

	rslts(2) = if(20)
	call sub2(jf)
	call kf(-1)
	end
c-----------------------------------------c
	subroutine sub2(jf)
c  -- external stmt should not be required to call jf:
	integer rslts(4), expect(4)
	common rslts, expect

	rslts(3) = jf(100)
	return
	end
c-----------------------------------------c
	subroutine sub3(jf)
	integer rslts(4), expect(4)
	common rslts, expect

	rslts(4) = jf
	return
	end
