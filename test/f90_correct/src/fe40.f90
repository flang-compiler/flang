!* Copyright (c) 1998, NVIDIA CORPORATION.  All rights reserved.
!*
!* Licensed under the Apache License, Version 2.0 (the "License");
!* you may not use this file except in compliance with the License.
!* You may obtain a copy of the License at
!*
!*     http://www.apache.org/licenses/LICENSE-2.0
!*
!* Unless required by applicable law or agreed to in writing, software
!* distributed under the License is distributed on an "AS IS" BASIS,
!* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!* See the License for the specific language governing permissions and
!* limitations under the License.

!   ACHAR with array arguments

module mod
  implicit none
contains
  subroutine sub(map,res)
    integer, dimension(:,:), intent(in) :: map
    integer, dimension(:) :: res
    call sub3(achar(map),res)
  end
  subroutine sub2(map,res)
    real, dimension(:,:), intent(in) :: map
    integer, dimension(:) :: res
    call sub3(achar(int(map)),res)
  end
  subroutine sub3(map,res)
    character (len=1), dimension(:,:), intent(in) :: map
    integer, dimension(:) :: res
    integer i,j,k
    k = 1
    do i = 1,ubound(map,1)
     do j = 1,ubound(map,2)
      res(k) = ichar(map(i,j))
      k = k + 1
     enddo
    enddo
  end
end module

use mod

integer a(2,2),res(8),exp(8)
data exp/97,98,65,66,99,100,67,68/
real c(2,2)
a(1,1) = ichar('a')
a(1,2) = ichar('b')
a(2,1) = ichar('A')
a(2,2) = ichar('B')
c(1,1) = ichar('c')
c(1,2) = ichar('d')
c(2,1) = ichar('C')
c(2,2) = ichar('D')
call sub(a,res(1:4))
call sub2(c,res(5:8))
!print *,res
!print *,exp
call check(res,exp,8)
end
