!
! Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!
!   Check functions that return pointers

      module  mod
      contains
      function  vector  (a,b,c)
      integer a,b,c
      integer,  pointer  ::  vector(:)
      integer j
      allocate  (vector  (a))
      j = b
      do i = 1,a
      vector(i) = j
      j = j + c
      enddo
      end function
      function v2 (a,b,c)
      integer:: a,b,c
      integer:: v2(a)
      integer j
      j = b
      do i = lbound(v2,1),ubound(v2,1)
       v2(i) = j
       j = j + c
      enddo
      end function
      end module mod
      program tt
      use mod
      integer, pointer :: v(:)
      integer, parameter::n=37
      integer result(n),expect(n)
      data expect/0,2,4,6,8,10,12,1,2,3,4,2, &
        4,6,8,22,20,18,16,14,12,10,8,10,&
        20,30,40,50,60,70,80,90,100,2,4,6,0/
      result = 0
      result(2:7) = vector(6,2,2)
      v => vector(4,1,1)
      result(8:11) = v
      result(12:15) = vector(4,2,2)
      deallocate(v)
      result(16:23) = vector(8,22,-2)
      allocate(v(1:10))
      v = v2(10,10,10)
      result(24:33) = v
      result(34:36) = v2(3,2,2)
        ! print *,result
      call check(result,expect,n)
      end
