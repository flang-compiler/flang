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

**---  z format - complex array
	parameter(N=4)
	integer result(N), expect(N)
	complex c(2)
	character*16 buf(2)
	data buf /'3ff00000345abcde', 'abcdef987654321f'/
	read(buf,99) c
99	format(2z8)
	read(buf(1)(1:8),  100) result(1)
	read(buf(1)(9:16), 100) result(2)
	read(buf(2)(1:8),  100) result(3)
	read(buf(2)(9:16), 100) result(4)
100	format(z8)

	data expect /'3ff00000'x, '345abcde'x, 'abcdef98'x, '7654321f'x/
	call check(result, expect, N)
	end
