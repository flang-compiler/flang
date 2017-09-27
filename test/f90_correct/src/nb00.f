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

**---  Z edit descriptor for double precision (run-time is endian dependent)

	parameter(N=16)
	integer result(N)
	integer expect(N)
	character*16 buf
	double precision d
	data d /1.0/    ! 3FF0000000000000

	write(buf, 99) d
99	format(z16)
	do i = 1, N
	    result(i) = ichar(buf(i:i))
	enddo

	data expect/'33'x, '46'x, '46'x, 13*'30'x/
	call check(result, expect, N)
	end
