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

*   EQUIVALENCE statements - data dependencies

      program p
      parameter (n=3)
      integer results(n)
      integer expect(n)

c   the variables in the following equivalences cannot be used as
c   arguments so that they will not have their addtkn flag set.
c   this gives the register allocator the opportunity to rule them
c   out as candidates due to their "equivalenced" attribute.

      EQUIVALENCE (RVOE01, RVOE02)
      EQUIVALENCE (IVOE12, IVOE13), (IVOE13, IVOE14)
      EQUIVALENCE (IVOE15, IVOE16)
      EQUIVALENCE (IVOE16, IVOE17)

      RVCOMP = 0.0
      RVOE01 = 45.
      RVOE02 = 12.
      RVCORR = 12.
      RVCOMP = RVOE01
      call fset(results(1), rvcomp)	! test 1

      IVCOMP = 0
      IVOE12 = 12
      IVOE13 = 13
      IVOE14 = 14
      IVCORR = 14
      IVCOMP = IVOE13
      call iset(results(2), ivcomp)	! test 2

      IVCOMP = 0
      IVOE15 = 15
      IVOE16 = 16
      IVOE17 = 17
      IVCORR = 17
      IVCOMP = IVOE16
      call iset(results(3), ivcomp)	! test 3

      call check(results, expect, n)

	data  expect /12, 14, 17/

      end
      subroutine fset (i, x)	! integer <- single
      i = x
      end
      subroutine iset (i, j)	! integer <- integer
      i = j
      end
