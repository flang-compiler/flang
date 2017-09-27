** Copyright (c) 1989-2017, NVIDIA CORPORATION.  All rights reserved.
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

**---	INF & NAN output

	parameter (N=16)
	character*28 out
	logical result(N), expect(N)
	double precision d(4), c(4)
	character*12 buf
	character*12 ch(4)

c  the third and fourth values below have been changed to the i387 'quiet'
c  NAN so that this test will work
	data d/'7ff0000000000000'x,
     +         'fff0000000000000'x,
     +         '7ff8000000000001'x,
     +         'fff8000000000001'x
     +        /

	data ch/'         Inf', '        -Inf',
     +          '         NaN', '         NaN'
     +         /

	ii = 1
	do i=1, 4
	    write(buf,90) d(i)
d	write(6,*) buf
	    result(ii) = buf .eq. ch(i)
	    ii = ii + 1
	    write(buf,91) d(i)
	    result(ii) = buf .eq. ch(i)
	    ii = ii + 1
	    write(buf,92) d(i)
	    result(ii) = buf .eq. ch(i)
	    ii = ii + 1
	    write(buf,93) d(i)
	    result(ii) = buf .eq. ch(i)
	    ii = ii + 1
	enddo


	data (expect(i),i=1,N)/N*.true./

	call check(result, expect, N)

90	format(e12.3)
91	format(f12.3)
92	format(g12.3)
93	format(d12.3)

	end
