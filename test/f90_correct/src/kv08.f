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

* Vectorizer - zero strides & streaming data
*              same as kv03.f + use of pointers

      parameter (N=4)
      double precision result(N), expect(N)
      double precision a(2), b(2)
      common /dat/a, b
      equivalence(result(1), a(1))

      pointer (px,idum)
      pointer (py,jdum)

      a(1) = 1.0
      a(2) = 2.0
      b(1) = 3.0
      b(2) = 4.0

      px = %loc(a)
      py = %loc(b)

      call dswap(2, px, 0, py, 0)   ! swaps the first elements 2 times => nochange

      data expect/1.0, 2.0, 3.0, 4.0/
	call checkd(result, expect, N)
      end


      subroutine dswap (n,px,incx,py,incy)
      integer n, incx, incy
      pointer (px, x(*))
      pointer (py, y(*))
      double precision x, y
c
c     Swap vectors.
c     y <==> x
c
      integer i, ix, iy
      double precision t
c
   20 ix = 1
      iy = 1
	  do i = 1, n
	     t = y(iy)
	     y(iy) = x(ix)
	     x(ix) = t
	     ix = ix + incx
	     iy = iy + incy
	  enddo
	  return
      return
      end
