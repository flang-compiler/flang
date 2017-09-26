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

*   Check expressions used as subprogram arguments 

C Written by James Craig Burley, 93-03-06, modified 93-04-06.
C Contact via Internet email: burley@gnu.ai.mit.edu
C Make sure expressions not ever passed as just vars, as in (A) as A,
C A+0 as A, A*1 as A, or INT(A) as A.  

      LOGICAL RSLTS(20), EXPECT(20)
      DATA RSLTS / 20 * 0 /
      DATA EXPECT / 20 * 0 /
      COMMON /X/ K
      INTEGER I,J,K
      EQUIVALENCE (K,J)
      INTEGER TRYME,TRYMEC
      I = 3
      J = 4
C     tests 1 - 4, parenthesis 
      IF (TRYME((I),I).NE.3) RSLTS(1) = 1
      IF (TRYMEC((J)).NE.4) RSLTS(2) = 1
      IF (I.NE.999) RSLTS(3) = 1
      IF (J.NE.999) RSLTS(4) = 1
      I = 3
      J = 4
C     tests 5 - 8, add
      IF (TRYME(I+0,I).NE.3) RSLTS(5) = 1
      IF (TRYMEC(J+0).NE.4) RSLTS(6) = 1
      IF (I.NE.999) RSLTS(7) = 1
      IF (J.NE.999) RSLTS(8) = 1
      I = 3
      J = 4
C     tests 9 - 12, mult 
      IF (TRYME(I*1,I).NE.3) RSLTS(9) = 1
      IF (TRYMEC(J*1).NE.4) RSLTS(10) = 1
      IF (I.NE.999) RSLTS(11) = 1
      IF (J.NE.999) RSLTS(12) = 1
      I = 3
      J = 4
C     tests 13 - 16, int
      IF (TRYME(INT(I),I).NE.3) RSLTS(13) = 1
      IF (TRYMEC(INT(J)).NE.4) RSLTS(14) = 1
      IF (I.NE.999) RSLTS(15) = 1
      IF (J.NE.999) RSLTS(16) = 1
      I = 3
      J = 4
C     tests 17 - 20, mult * 2 
      IF (TRYME(I*2,I).NE.6) RSLTS(17) = 1
      IF (TRYMEC(J*2).NE.8) RSLTS(18) = 1
      IF (I.NE.999) RSLTS(19) = 1
      IF (J.NE.999) RSLTS(20) = 1
      CALL CHECK (RSLTS, EXPECT, 20)
      END
      INTEGER FUNCTION TRYME(RTNME,HITME)
      INTEGER RTNME,HITME
      HITME = 999
      CALL INSURE(TRYME,RTNME,HITME)
      TRYME = RTNME
      END
      INTEGER FUNCTION TRYMEC(RTNME)
      INTEGER RTNME,HITME
      COMMON /X/ HITME
      HITME = 999
      CALL INSURE(TRYMEC,RTNME,HITME)
      TRYMEC = RTNME
      END
      SUBROUTINE INSURE(I,J,K)
C DUMMY SUBR TO INSURE OPTIMIZING DOESN'T GET TOO CLEVER!
C IF YOUR COMPILER GLOBALLY OPTIMIZES THIS AWAY, TRY MOVING
C IT INTO ANOTHER SOURCE FILE OR OTHERWISE KEEP IT FROM
C DOING SO, TO MAKE THE TEST USEFUL.
      END

