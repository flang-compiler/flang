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

**---   O format, complex array

	parameter(N=4)
	character*22 buf(2)
	data buf /'3535353535336767676767', '1111111111122222222222'/

	complex c(2)
	integer result(N), expect(N)
	equivalence(result, c)

	read(buf,99) c
99	format(2o11)

	data expect /'35353535353'o, '36767676767'o,
     +  '11111111111'o, '22222222222'o /
	call check(result, expect, N)
	end
