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
  
  logical,target :: l 
  logical results(5)
  logical expect(5)
  integer*8,target:: z
  class(*),pointer :: lp
  
  type my_type
     class(*),allocatable :: a
     class(*),pointer :: p
  end type my_type
  
  type(my_type) :: obj

  results = .false.
  expect = .true.
   
  l = .true.
  lp => l 
  obj%p => l

  results(1) = same_type_as(lp,obj%p)

  select type (p=>lp)
  type is (logical)
  !print *, lp
  results(4) = p
  end select

  select type (p=>obj%p)
  type is (logical)
  !print *, p
  results(2) = p
  end select

  select type (lp)
  type is (logical)
  !print *, lp
  results(3) = lp
  end select

  call check(results,expect,4)
  
end program unlimited_poly


