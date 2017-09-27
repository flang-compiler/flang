! Copyright (c) 1998, NVIDIA CORPORATION.  All rights reserved.
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
! test that PRIVATE/PUBLIC can be used on names that aren't defined until later

module m1
 real max,min
 public:: max
 private:: min
 public:: sin, setmin
contains
 subroutine setmin(i)
  real i
  min = i
!print *,'min=',min
 end subroutine
 real function sin(x)
  real x
  sin = min*max*x
!print *,'sin=',sin
 end function
end module

program p
 use m1
 real t, u, v
 real expect(3), result(3)
 data expect/5.0,90.0,5.0/
 max = 9.0
 call setmin(2.0)
 t = 5.0
 u = sin(t)
 v = min(t,u)
 result(1) = t
 result(2) = u
 result(3) = v
! print *,t,u,v
 call check(result,expect,3)
end program
