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

*--- Induction last value bugs
*    

	program ka44
	parameter (N=1)
	integer i, j, result(N), expect(N)
	common i, j, result, expect

	data expect /21/
	call pas4f(10, 0, result(1))
	call check(result, expect, N)
	end
      SUBROUTINE PAS4F (LA, nd4, ires)
C
      INTEGER*4    LA
      INTEGER*4    K,L
      INTEGER*4    I2,J2,ND4
         J2 = 1
         DO 40 K = 0, nd4

            DO 30 L = 1, LA
               J2 = J2 + 2
   30       CONTINUE
	    ires = j2	! bug - last val incr considered invariant
   40    CONTINUE
      RETURN
      END
