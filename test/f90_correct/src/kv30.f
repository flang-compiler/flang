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
c
c Test for illegal loop interchange.  Originally found in MOLPRO.

      integer nshlr(11)
      integer nshl(11,8)
      integer expect
      data nshl / 0, 1, 9*0, 1, 0, 9*0, 66*0 /
      data nshell / 11 /
      data expect / 2 /
c
      nshlx=0
      nshlrx=0
      do 120 i=1,nshell
      if(nshlr(i).ne.0) nshlrx=i
      do 120 isy=1,8
      if(nshl(i,isy).ne.0) nshlx=i
120   continue
      call check(nshlx, expect, 1)
      end
