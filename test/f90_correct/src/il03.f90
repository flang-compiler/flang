!
! Copyright (c) 2015, NVIDIA CORPORATION.  All rights reserved.
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

 subroutine s( a )
  integer a(10)
  do i = 1,10
   a(i) = a(i) + 1
  enddo
 end subroutine

module m
 type dt
  integer,pointer :: x(:)
 end type
 type(dt)::f
end module
 
subroutine t
 use m
 allocate(f%x(10))
 do i = 1,10
  f%x(i) = i+1
 enddo
 call s(f%x)
end subroutine

program p
 use m
 integer res(10),exp(10)
 data exp/3,4,5,6,7,8,9,10,11,12/
 call t()
 do i = 1,10
  res(i) = f%x(i)
 enddo
 call check(res,exp,10)
end program
