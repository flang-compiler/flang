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
  integer z 
  logical,target :: l 
  logical results(4)
  logical expect(4)

  class(*),allocatable :: a
  class(*),pointer :: p
  logical, pointer :: lp

  results = .false.
  expect = .true.
  

  l = .false.

  allocate(integer*8::a)

  p => l

  select type(p)
  type is (logical)
  results(1) = .true.
  p = .true.
  end select 

  select type(a)
  type is (integer*8)
  results(2) = .true.
  type is (integer)
  results(2) = .false.
  end select

  results(3) = l

  z = 777
  deallocate(a)
  allocate(a,source=z)
  select type(a)
  type is(integer)
  results(4) = a .eq. z
  end select
 
  call check(results,expect,4)
  
end program unlimited_poly


