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

*   POINTER statements

      parameter(N = 5)
      integer result(N), expect(N)
      double precision dum(2)
      double precision d
      pointer(ptr, d)
      double complex cd
      pointer(ptr, cd)
      pointer (p1, ib)
      pointer (p2, p1)

      data expect /
     +  2,
     +  4,
     +	2, 4,
     +  10
     + /

      p2 = loc(result(5))
      ptr = loc(dum)
      dum(1) = 2.0
      dum(2) = 4.0

      result(1) = d

      ptr = ptr + 8
      result(2) = d

      ptr = ptr - 8
      result(3) = real(cd)

      result(4) = dimag(cd)

      ib = 10		! two levels of pointer. should be result(5)

      call check(result, expect, N)
      end
