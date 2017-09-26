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

*   Optimizer bug in loop processing

	program ka02
	integer expect(209), np(209)

	data expect/
     + 15, -5, -20, -10, -25, -1, -16, -6, -21, -11,
     + -26, -2, -17, -7, -22, -12, -27, -3, -18, -8,
     + -23, -13, -28, -4, -19, -9, -24, -14, -29, 180*0
     + /

	data np/
     + 15, 5, 20, 10, 25, 1, 16, 6, 21, 11,
     + 26, 2, 17, 7, 22, 12, 27, 3, 18, 8,
     + 23, 13, 28, 4, 19, 9, 24, 14, 29, 180*0
     + /

	j = 0
	nn = 29
	goto 914

910   continue
      K=KK
      KK=NP(K)
      NP(K)=-KK
      IF(KK .NE. J) GO TO 910
      K3=KK
914   J=J+1
      KK=NP(J)
      IF(KK .LT. 0) GO TO 914
      IF(KK .NE. J) GO TO 910
      NP(J)=-J
      IF(J .NE. NN) GO TO 914


	call check(np, expect, 29)
	end
