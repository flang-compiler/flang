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

program zieee14
use ieee_arithmetic
real*4 a,b,c,c2
real*8 x,y,z,z2
logical lexp(4), lres(4)

a = sqrt(5.0)
x = sqrt(5.0d0)

b = exp(1.0) / 2.0
y = exp(1.0d0) / 2.0d0

c = ieee_rem(a,b)
z = ieee_rem(x,y)

c2 = a - b*2.0
z2 = x - y*2.0d0

lres(1) = (abs(c2 - c) .lt. 0.00001)
lres(2) = (abs(z2 - z) .lt. 0.000000001d0)
z = ieee_rem(a,y)
lres(3) = (abs(z2 - z) .lt. 0.00001d0)
z = ieee_rem(x,b)
lres(4) = (abs(z2 - z) .lt. 0.00001d0)

lexp = .true.
call check(lres, lexp, 4)

end
