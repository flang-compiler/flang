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
	parameter (NUM=2)
	common result(NUM), expect(NUM)
	logical result, expect
	doubleprecision x(10),d(10)

	do 1 i = 1,10
	    call noswpipe
	    d(i) = 1d0*i+1d0*dble(i)/10d0
	    x(i) = 3d0*i+3d0*dble(i)/10d0
1	continue

	call f06fcft(10,d,0,x,0)

*       Changing the tolerance here because of IPA.  12/14/2010
	result(1) = abs(d(1) - 1.1d0) .lt. 1.d-15
	result(2) = abs(x(1) - 8.559350118330004d0) .lt. 1.d-14
c	    print *,d(1),x(1)

	data expect/ .true., .true. /
	call check(result, expect, NUM)
	end

	subroutine noswpipe
	end

      SUBROUTINE f06fcft( N, D, INCD, X, INCX )
      INTEGER            INCD, INCX, N
      DOUBLE PRECISION   D( * ), X( * )
      INTEGER            I, ID, IX
c      EXTERNAL           DSCAL
      INTRINSIC          ABS
c      IF( N.GT.0 )THEN
               IX = 1
               ID = 1 - ( N - 1 )*INCD
               DO 30, I = 1, N
                  X( IX ) = D( ID )*X( IX )
                  ID      = ID              + INCD
                  IX      = IX              + INCX
   30          CONTINUE
c      END IF
      RETURN
      END
