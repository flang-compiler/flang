!* Copyright (c) 2000-2017, NVIDIA CORPORATION.  All rights reserved.
!*
!* Licensed under the Apache License, Version 2.0 (the "License");
!* you may not use this file except in compliance with the License.
!* You may obtain a copy of the License at
!*
!*     http://www.apache.org/licenses/LICENSE-2.0
!*
!* Unless required by applicable law or agreed to in writing, software
!* distributed under the License is distributed on an "AS IS" BASIS,
!* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!* See the License for the specific language governing permissions and
!* limitations under the License.
!
! test whether common, module, dummy variables can appear
! in implied do loops in array constructors

module m
 implicit none
 integer mi
end module

subroutine s( di )
 use m
 implicit none
 integer di
 common/ji/ ci
 integer ci

 integer, dimension(18):: res, exp
 data exp/1,2,3,4,5,10,20,30,40,50,100,200,300,400,500,-1,-2,-3/

 res(1:5) = (/ (mi,mi=1,5) /)
 res(6:10) = (/ (di,di=10,50,10) /)
 res(11:15) = (/ (ci,ci=100,500,100) /)
 res(16:18) = (/ mi,di,ci /)

 !print *,res
 call check(res,exp,18)

end

program p
 use m
 implicit none
 integer di
 common/ji/ ci
 integer ci
 mi = -1
 di = -2
 ci = -3
 call s( di )
 !print *,mi,di,ci
end
