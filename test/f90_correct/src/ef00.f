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

*   Data initializations of arrays and use of
*   repetition counts in DATA statements.

      parameter (N = 34)
      common /rslts/ rslts(N)
      common /expect/ expect(N)

      call check(rslts, expect, N)
      end


c  -- block data to initialize result array:

      block data aaa
      character*1 ch1, ch4(0:3)*4, ch3*3, chpad(1)
      integer a(0:1, -2:-1, 5:6)
      real b(55555:55559)
      logical c(6), d, e(2)
      complex cc(-99999: -99998)

      common /rslts/ a, b, c, d, e, cc, ch1(3), ch4, ch3(4), chpad

      data c, d, e/ .true., 1*.false., 3*.true., .false., 3*.true./
      data b /2*1.0, 3*-2.0/
      data a(0, -2, 5) /2/,
     1     a(1, -2, 5) /1 * -3/,
     1     a(1, -1, 6) /3/
      data a(0, -1, 5), a(1, -1, 5), a(0, -2, 6),
     +     a(1, -2, 6), a(0, -1, 6) / 2 * 8, 3 * 7/

      data cc(-99999), cc(-99998) / 2*(1.0, 2.0) /
      data ch1 /'a', 'ab', 1Hc/,   ch4/'de', 4hfghi, 2*'jklm'/,
     /     ch3(1)(1:3), ch3(2)(1:1), ch3(2)(2:3)/ '123', '4', '56'/
     /     ch3(3)(:), ch3(4)(:2), ch3(4)(3:)/ '789', '01', '2x' /
     /     chpad / 1 * 'p' /

      END

c  --- block data to set up expected results array:

      blockdata
      parameter (N = 34)
      common /expect/ expect(N)
      integer expect
      logical lexpect(N)
      real    rexpect(N)
      character*4 chexpect(N)
      equivalence (expect, lexpect, rexpect, chexpect)

      data (expect(i), i = 1, 8) / 2, -3, 8, 8, 7, 7, 7, 3 /
      data (rexpect(i), i = 9, 13) /1.0, 1.0, -2.0, -2.0, -2.0/
      data (lexpect(i), i = 14, 22) / .true., .false., .true.,
     +         .true., .true., .false., .true., .true., .true./
      data (rexpect(i), i = 23, 26) / 1.0, 2.0, 1.0, 2.0 /

      data (chexpect(i), i = 27, 34) / 'aacd', 'e  f', 'ghij',
     +                                 'klmj', 'klm1', '2345',
     +                                 '6789', '012p'         /

      end
