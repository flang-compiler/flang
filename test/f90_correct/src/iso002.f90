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

program test 

   use  iso_c_binding 

   implicit none 

   integer, pointer :: ptr(:,:) => NULL() 
   integer, pointer :: uin(:,:) => NULL() 
   integer, pointer::pp

   integer, parameter :: nx = 4 
   integer, parameter :: ny = 5 
   type(c_ptr)::foo

   integer :: i, j 

   allocate(ptr(nx,ny)) 

   forall (i=1:nx,j=1:ny) ptr(i,j) = 10*i+j 

    pp=>ptr(1,1)
   call c_f_pointer(c_loc(pp),uin,[nx,ny]) 

   write (*,*) 'shape(uin): ', shape(uin) 
   write (*,'(a,x,z32)') ' loc(uin(1,1)): ', loc(uin(1,1)) 
   write (*,*) 'uin: ', uin 



   deallocate(ptr) 
   print *, "PASS"

end program test 

subroutine dummy(i)
integer i
print *, i

end

