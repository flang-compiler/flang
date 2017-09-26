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

**---   O format, double precision

	parameter(N=22)
	integer result(N), expect(N)
	data result/N*99/
	character*22 buf
	double precision d
	data d /'0400441616155074102142'o/
	write(buf, 99)d
99	format(o22)
	do i = 1, N
	    result(i) = ichar(buf(i:i)) - ichar('0')
	enddo

	data expect /
     +  -16, 4, 0, 0, 4, 4, 1, 6, 1, 6, 1, 5,
     +  5, 0, 7, 4, 1, 0, 2, 1, 4, 2 /
	call check(result, expect, N)
	end
