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

*   Non-deletable induction variables.
*   Optimizer tests of induction variables that should not
*   be deleted from a loop

	subroutine prin(p, result)
	integer p, result
	result = p
	end

	program ka36
	parameter(N=2)
	integer result(N), expect(N)
	integer i, a(0:9), j

	do j = 0, 2, 1         ! this j has an explict use later on
	    a(j) = 0
	enddo

	do i = 0, 1, 1         ! this i has an implicit use due to addrtkn
	    a(i) = 0
	enddo
	call prin(i, result)
	result(2) = j
        call check (result, expect, N)
	data expect /2, 3/
	end
