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

* Vectorizer - distribution, interchange, split
*              same as kv04.f + use of pointers

      parameter (N=10)
      pointer (p, result(N,N))
      dimension sresult(N,N)

      integer iexpect(N,N), iresult(N,N)

      integer*8 set_addr

      p = set_addr(sresult)

      DO 210 I=1,N
      DO 200 J=1,N
      result(I,J)=FLOAT(I+J)     ! /1000.
200   CONTINUE
      result(I,I)=2520./FLOAT(I) ! /FLOAT(220+I)
210   CONTINUE

	do ii = 1, N
cpgi$l novector
	    do jj = 1, N
		iresult(ii,jj) = result(ii,jj) + .000001
	    enddo
	enddo
	call check(iresult, iexpect, N*N)

	data iexpect/
     + 2520,3,4,5,6,7,8,9,10,11,
     + 3,1260,5,6,7,8,9,10,11,12,
     + 4,5,840,7,8,9,10,11,12,13,
     + 5,6,7,630,9,10,11,12,13,14,
     + 6,7,8,9,504,11,12,13,14,15,
     + 7,8,9,10,11,420,13,14,15,16,
     + 8,9,10,11,12,13,360,15,16,17,
     + 9,10,11,12,13,14,15,315,17,18,
     + 10,11,12,13,14,15,16,17,280,19,
     + 11,12,13,14,15,16,17,18,19,252 /
      end

      integer*8 function set_addr(x)
      dimension x(*)
      set_addr = %loc(x)
      return
      end
