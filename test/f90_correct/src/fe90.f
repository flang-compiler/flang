C Copyright (c) 2005, NVIDIA CORPORATION.  All rights reserved.
C
C Licensed under the Apache License, Version 2.0 (the "License");
C you may not use this file except in compliance with the License.
C You may obtain a copy of the License at
C
C     http://www.apache.org/licenses/LICENSE-2.0
C
C Unless required by applicable law or agreed to in writing, software
C distributed under the License is distributed on an "AS IS" BASIS,
C WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
C See the License for the specific language governing permissions and
C limitations under the License.
C
C PIC offset computed for zm(3) in line 30 is ignored in line 35


       program bidon
       call gllvls (66)
       stop
       end

       subroutine gllvls (nk)
       implicit none
       integer nk
       integer maxdynlv
       integer expect(3)
       integer result(3)
       parameter (maxdynlv = 1000)
       real zt(maxdynlv),ztr(maxdynlv),zm(maxdynlv)
       common /levelsr/ zt,ztr,zm
        integer k

       expect(1) = loc(zm(1))
       expect(2) = loc(zm(2))
       expect(3) = loc(zm(3))
C      print*, loc(zm(1)),loc(zm(2)),loc(zm(3))

       zm=0.
       ztr(1)=  zm(3)*0.5

C      print*, loc(zm(1)),loc(zm(2)),loc(zm(3))
       result(1) = loc(zm(1))
       result(2) = loc(zm(2))
       result(3) = loc(zm(3))
       call check(expect, result, 3)

         stop
       return
       end
