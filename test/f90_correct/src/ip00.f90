! Copyright (c) 1997, NVIDIA CORPORATION.  All rights reserved.
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
!   Internal functions

program p
 integer a,b
 parameter(n=1)
 integer result(n), expect(n)
 data expect/2/
 b = 1
 result(1) = f(b)
 call check( result, expect, n )
 
contains
 function f(x)
  integer f,x
  f = x + 1
 end function
end program
