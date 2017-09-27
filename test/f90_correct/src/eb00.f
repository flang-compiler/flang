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

*   Test data initialization of real, double, and
*   complex scalar variables.

      block data b

      real x, y, z
      double precision dx, dz
      real *8 dy
      real zz/10.2/
      common /s/ x, y, z, zz, dx, dy, dz, c1, c2, c3
      complex c1, c2, c3

      data x, y /0.0, 2.3e0/,  z / -5.6E20 /
      data dx, dy, dz / -0D0, 5d1, -4.0D-3 /
      data c1/(0.1, 1.0)/
     +     c2/(-1.0, 0.0)/
     +     c3/-(2.3, -4.5)/

      END

C   MAIN PROGRAM -
      common /s/ results(16)
      parameter (x = 0.0)
      real expect(16)
      double precision d(2)
      equivalence (expect(7), d(1))
      
      data expect /
c            tests 1 - 4:  real numbers
     +                     0.0, 2.3, -5.6E20, 10.2,
c            tests 5 - 10: double precision 
     +                     0.0, -0.0, x, x, x, x,
c            tests 11 - 16: complex
     +                     0.1, 1.0, -1.0, 0.0, -2.3, 4.5 /

      d(1) = 50.0D0
      d(2) = -4.0D-3

      call  check(results, expect, 16)
      end
