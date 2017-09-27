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

logical function check_logical(p) result (RSLT)
  class(*),pointer :: p
  select type(p)
  type is (logical)
     RSLT = .true.
     class default
     RSLT = .false.
  end select
end function check_logical

logical function check_alloc(a) result (RSLT)
  class(*),allocatable :: a
  select type(a)
  type is (integer*8)
     RSLT = .true.
     class default
     RSLT = .false.
   end select
end function check_alloc


program unlimited_poly
USE CHECK_MOD
  
  logical,target :: l 
  logical results(7)
  logical expect(7)
  class(*),pointer :: lp
  
  interface
     logical function check_logical(p) result (RSLT)
       class(*),pointer :: p
     end function check_logical
     
     logical function check_alloc(a) result (RSLT)
       class(*),allocatable :: a
     end function check_alloc
  end interface
  
  type my_type
     class(*),allocatable :: a
     class(*),pointer :: p
  end type my_type
  
  type(my_type) :: obj

  results = .false.
  expect = .true.
  
  
  l = .false.

  allocate(integer*8::obj%a)

  obj%p => l

  select type(p=>obj%p)
  type is (logical)
     results(1) = .true.
     p = .true.
  end select 

  select type(a=>obj%a)
  type is (integer*8)
     results(2) = .true.
  type is (integer)
     results(2) = .false.
  end select

  results(3) = l

  lp => l
  results(6) = same_type_as(obj%p,lp)
  results(7) = extends_type_of(obj%p,lp)
  
  results(4) = check_logical(obj%p)
  results(5) = check_alloc(obj%a)
  
  call check(results,expect,7)
  
end program unlimited_poly


