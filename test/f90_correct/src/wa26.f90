! Copyright (c) 2008-2017, NVIDIA CORPORATION.  All rights reserved.
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

! TRANSFER function with assumed-shape character string
! in concatenation of TRANSFER argument

subroutine s(a,b,n,y)
 character*10 :: a
 character*(*) :: b
 integer :: n
 integer :: y(6)

 y = transfer(a // b // repeat(' ',n) // char(0), y )
!y = transfer(a // b // repeat(' ',n), y )
end

program p
 integer y(6)
 integer r(6)
 data r/ 4habcd, 4hefgh, 4hijkl, 4hmnop, 4hqQ  , z'00202020'    /
 call s('abcdefghij', 'klmnopqQ', 5, y )

! do i = 1,6
!  print '(z8.8,2x,z8.8)',y(i), r(i)
!  if( y(i).ne.r(i) ) print *, 'no match'
! enddo
  call check(y,r,6)
end program

