c Copyright (c) 2005, NVIDIA CORPORATION.  All rights reserved.
c
c Licensed under the Apache License, Version 2.0 (the "License");
c you may not use this file except in compliance with the License.
c You may obtain a copy of the License at
c
c     http://www.apache.org/licenses/LICENSE-2.0
c
c Unless required by applicable law or agreed to in writing, software
c distributed under the License is distributed on an "AS IS" BASIS,
c WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
c See the License for the specific language governing permissions and
c limitations under the License.
c
c Test case from MOLPRO.  The inner loop below should not be vectorized.

      double precision binom(100), expect(100)
      data expect /100 * 1.0d0/
      call cortab(binom, 10, 10)
      call checkd(binom, expect, 46)
      end

      subroutine cortab(binom,maxj,maxi)
      implicit double precision (a-h,o-z)
      dimension binom(*)
c
      inew=2
      binom(1)=1.0d0
      do 150 j=1,maxj
        do 140 i=1,j-1
c       do 140 i=1,j+1
          binom(inew)=binom(inew-1)
          inew=inew+1
  140   continue
  150 continue
      print *,inew
      end

