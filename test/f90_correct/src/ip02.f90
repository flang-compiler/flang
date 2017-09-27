! Copyright (c) 1998, NVIDIA CORPORATION.  All rights reserved.
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
! Test that nested interfaces work as expected
program p
implicit none

 interface
  subroutine a(b,i)
  implicit none
  integer i
  interface
   integer function b(c)
   integer c
   end function b
  end interface
  end subroutine a
  integer function d(e)
   integer e
  end function
 end interface
 integer j
 integer result(1), expect(1)
 data expect/11/
 j = 1
 call a(d,j)
 result(1) = j
! print *,result,expect
 call check(result,expect,1)
end

subroutine a(b,i)
 implicit none
 integer i
 interface
  integer function b(c)
  integer c
  end function b
 end interface
 i = b(i)
end subroutine
integer function d(e)
 integer e
 d = 11*e
end function
