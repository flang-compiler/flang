! Copyright (c) 2010, NVIDIA CORPORATION.  All rights reserved.
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

program unlimited_poly
USE CHECK_MOD

  type my_type
  class(*),allocatable :: a
  class(*),pointer :: p
  end type

  integer z 
  logical,target :: l 
  logical results(4)
  logical expect(4)
  type(my_type) :: obj
  type(my_type),allocatable :: obj2

  class(*), allocatable :: u2(:,:)
  integer sa(5,5)
  integer, allocatable :: sa2(:,:)
  integer i,j,k

  k = 0
  do i=1,5
   do j=1, 5
      sa(i,j) = k
      k = k + 1
   enddo
 enddo

  results = .false.
  expect = .true.
  
  allocate(sa2(5,5))
   sa2 = sa

    ALLOCATE (u2(lbound(sa2,1):ubound(sa2,1),lbound(sa2,2):ubound(sa2,2)),SOURCE=sa)

   select type(qq=>u2)
   type is (integer)
   print *, sa2
   sa2 = qq
   i = size(qq)
   results(1) = i .eq. 25
   results(2) = all(sa2 .eq. qq)
   results(3) = all(qq .eq. sa)
   end select
   results(4) = all(sa2 .eq. sa)

  call check(results,expect,4)
  
end program unlimited_poly


