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

*   Optimizer bug in loop processing - arithmetic ifs

	program ka05
	parameter (N=1)
	integer result(N), expect(N)

      integer kfl(6)
      data kfl/0,0,1,1,1,2/
      call pdcprf(6,kfl, result(1))

      data expect /
     +  -1 /

      call check(result, expect, N)

      end
      SUBROUTINE PDCPRF(N, KFL, AREA)
      IMPLICIT  INTEGER (A-Z)
      DIMENSION KFL(*)

	AREA = 0
        II = 1
   20 continue		! so that have an outer loop

* three-way arithmetic if to distinct labels => a temp is need for
* the arith-if expression.  Multiple blocks are generated for the branching
* to the 2nd and 3rd labels.  Ensure that the temp store and its loads are
* properly handled.
 
   51          IF( KFL(II) - 1 ) 52, 60, 901
   52             AREA = AREA + 1
                  II = II + 1
               GO TO 51
   60          area = area - 1
	      II = II + 1
      goto 20

  901 continue
C
      RETURN
      END
