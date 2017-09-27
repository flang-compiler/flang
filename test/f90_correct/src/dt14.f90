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

program p
 type dt
  integer n
  integer m
  real r
 end type
 type(dt),parameter:: alpha = dt(8,9,2.0)
 integer n,x
 integer result(2), expect(2)
 data expect/100,101/

 n = 5
 select case(n)
 case(alpha%m)
  result(1) = 99
 case(:alpha%n)
  result(1) = 100
 case default
  result(1) = 101
 end select
  
 n = -2
 select case(n)
 case(alpha%m)
  result(2) = 99
 case(-1:alpha%n)
  result(2) = 100
 case default
  result(2) = 101
 end select
  
 call check(result,expect,2)
end program
