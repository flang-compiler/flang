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

*   Integer DO-loop tests - special cases:
*      (1) real addition used for step expression,
*      (2) do index variable used in init, upper, and step exprs,
*      (3) change values of init, upper, and step exprs
*          within the loop.

      program p
   
      integer rslts(11), expect(11)
c                         tests 1 - 4:
      data expect /   2, 7, 6, -4,
c                         tests 5 - 8:
     +                10, -6, -10, -8,
c                         tests 9 - 11:
     +                9, 1013, 11      /

C     tests 1, 3, 5, 2:   Real addition for step expression:

      data x, y / 5.3, -3.1 /
      do 10 i = 1, 6, x+y
          rslts(i) = 2 * i
10    continue
      rslts(2) = i

C     tests 4, 6, 8, 7:  DO index var used in DO expressions:

      data i6 /6/
      i = i6
      do 20 i = i-10, i-14, 2*i-14
          rslts(-i) = i
20    continue
      rslts(7) = i

C     tests 9, 11, 10:  Changing value of DO exprs within loop:

      j = 9
      k = 11
      m = 2
      do 30 i = j, k, m
          j = 100
          k = 1000
          m = 3
          rslts(i) = i
30    continue
      rslts(10) = i + k

C     check results:

      call check(rslts, expect, 11)
      end
