! Copyright (c) 2012, NVIDIA CORPORATION.  All rights reserved.
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

module mod
logical expect(12), rslt(12)
type :: objects(k1,k2,l1)
integer, kind :: k2 = selected_char_kind("ASCII")
integer, kind :: k1 = selected_int_kind(4)
integer,len :: l1 = 250
integer(k1) :: p(l1,l1)
end type
contains
subroutine foo(n)
integer n
logical err
type(objects(k1=4)) :: x

rslt(5) = (x%k1 .eq. 4) 
rslt(6) = x%k2 .eq. selected_char_kind("ASCII") 
rslt(7) = x%l1 .eq. n 
rslt(8) = size(x%p,dim=1) .eq. x%l1
rslt(9) = size(x%p,dim=2) .eq. x%l1

k=0
do i=1,size(x%p,dim=1)
do j=1, size(x%p,dim=2)
  x%p(i,j) = k
  k = k + 1
enddo
enddo

k=0
err = .false.
do i=1,size(x%p,dim=1)
do j=1, size(x%p,dim=2)
  if (x%p(i,j) .ne. k) then 
       if (.not. err) then 
        print *, 'element ',i,',',j,' equals ',x%p(i,j),' but expecting ',k
       endif
       err = .true.
  endif
  k = k + 1
enddo
enddo
rslt(10) = .not. err
rslt(11) = i .ne. size(x%p,dim=1)
rslt(12) = j .ne. size(x%p,dim=2)

end subroutine

end module

program p
use mod
integer y 
type(objects(1,20,30)) :: x

rslt = .false.
rslt(1) = x%k1 .eq. 1
rslt(2) = x%k2 .eq. 20
rslt(3) = x%l1 .eq. 30
rslt(4) = size(x%p, dim=1) .eq. x%l1

call foo(250)

expect = .true.
call check(rslt,expect,12)



end
