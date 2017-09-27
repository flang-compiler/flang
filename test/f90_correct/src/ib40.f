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

*   Subprograms with many arguments

	parameter(n = 4)
	integer result(n), expect(n)
	data expect/11,12,13,14/

	call t0(result, 1,2,3,4,5,6,7,8,9,10,11,12,13,14)
	call check (result, expect, n)
	end
	subroutine t0(a,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
	integer a(*)
	call t1		! to force memory arg ptr to memory
	a(1) = i11
	a(2) = i12
	a(3) = i13
	a(4) = i14
	end
	subroutine t1
	end
