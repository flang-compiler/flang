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

*   Data initialization using Hollerith constants and multi-word
*   variables.

c  Ensures that when a multi-word variable is traversed as a "char *",
c  the lexical order of the hollerith constant is maintained.


      program p
      parameter (N=32)
      integer rslts(N), expect(N)

      complex cp(1)
      double complex dcp(1)
      double precision dp(1)
      data cp/8habcdefgh/
      data dp/8habcdefgh/
      data dcp/16habcdefgh12345678/

      data expect /
     + 97,98,99,100,101,102,103,104,  !cp
     + 97,98,99,100,101,102,103,104,  !dp
     + 97,98,99,100,101,102,103,104,49,50,51,52,53,54,55,56 !dcp
     +/

      call fill(rslts(1), cp, 8)
      call fill(rslts(9), dp, 8)
      call fill(rslts(17), dcp, 16)

      call check(rslts, expect, N)
      end
      subroutine fill(r, bt, n)
      integer r(*)
      byte bt(*)
      do i=1, n
	r(i) = bt(i)
      enddo
      end
