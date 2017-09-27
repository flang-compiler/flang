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

*   Miscellaneous software pipelining bugs

      program test
      common ires(4), iexp(4)
      data iexp/2,3,2,-1/
      double precision x, j, dum
      x = 3
      dum = j(x)
      call check(ires, iexp, 4)
      end
      DOUBLE PRECISION FUNCTION j(X)
      DOUBLE PRECISION                 X
      common ires(4), iexp(4)
      DOUBLE PRECISION                 G, GBIG, LNR2PI, T, XBIG, XSMALL,
     *                                 XVBIG, Y
      INTEGER                          I, M
      INTRINSIC                        LOG, DBLE
      DATA XSMALL,XBIG,LNR2PI/
     *1.0D-17,7.7D+7,9.18938533204672742D-1/
      DATA XVBIG,GBIG/2.55D+305,1.79D+308/
      j = 0.0
      IF (X.GT.XSMALL) GO TO 20
      IF (X.LE.0.0D0) GO TO 160
      j = -LOG(X)
      GO TO 200
   20 IF (X.GT.15.0D0) GO TO 120
      M = X
      T = X - DBLE(M)
      M = M - 1
      G = 1.0D0
      IF (M) 40, 100, 60
   40 G = G/X
      GO TO 100
   60 DO 80 I = 1, M
         G = (X-DBLE(I))*G
   80 CONTINUE
  100 T = 2.0D0*T - 1.0D0
c      print *,'g,x,m,t=',g,x,m,t
      ires(1) = g
      ires(2) = x
      ires(3) = m
      ires(4) = t
c      print *, ires
  120 IF (X.GT.XBIG) GO TO 140
      GO TO 200
  140 IF (X.GT.XVBIG) GO TO 180
      GO TO 200
  160 continue
      GO TO 200
  180 continue
  200 RETURN
      END
