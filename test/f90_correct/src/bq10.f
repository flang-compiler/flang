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

*   POINTER statements - adjustable arrays, loc intrinsic

      parameter(N = 13)
      integer result(N), expect(N)
      pointer (ptr, idum)

      data expect /
     +  2, 3, 3, 4, 			! tests 1-4, ment
     + 2, 3, 4, 3, 4, 5, 4, 5, 6	! tests 5-13, ent
     + /

      ptr = loc(result(1))
      call ment(ptr, 2)

      ptr = loc(result(5))
      call ent(ptr, 3)

      call check(result, expect, N)
      end

      subroutine ment(p, n)
      entry foo(p, n)
      dimension iarr(n, n)
      pointer (p, iarr)	! object refd by common arg declared after entry
      do i = 1, n
	  do j = 1, n
	      iarr(i, j) = i + j
	  enddo
      enddo
      return
      end
      subroutine bar(p, n)
      pointer (p, iarr)	! object refd by common arg declared before entry
      integer iarr(n, n)
      entry ent(p, n)
      do i = 1, n
	  do j = 1, n
	      iarr(i, j) = i + j
	  enddo
      enddo
      return
      end
