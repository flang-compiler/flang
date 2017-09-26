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
 integer, dimension(3), parameter:: a = (/3,5,7/)
 integer n
 integer result(1), expect(1)
 data expect/100/

 n = 5
 select case(n)
 case(a(1))
  result(1) = 99
 case(a(2))
  result(1) = 100
 case default
  result(1) = 101
 end select
  
 call check(result,expect,1)
end program
