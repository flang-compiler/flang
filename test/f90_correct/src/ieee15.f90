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

program zieee15
use ieee_arithmetic
real*4 a,b,c
real*8 x,y,z
logical la(94), lb(94)
i = 0

la = .false.
lb = .true.

i = i + 1
la(i) = ieee_support_datatype()
i = i + 1
la(i) = ieee_support_datatype(a)
i = i + 1
la(i) = ieee_support_datatype(x)

i = i + 1
la(i) = ieee_support_divide()
i = i + 1
la(i) = ieee_support_divide(a)
i = i + 1
la(i) = ieee_support_divide(x)

i = i + 1
la(i) = ieee_support_inf()
i = i + 1
la(i) = ieee_support_inf(c)
i = i + 1
la(i) = ieee_support_inf(x)

i = i + 1
la(i) = ieee_support_nan()
i = i + 1
la(i) = ieee_support_nan(b)
i = i + 1
la(i) = ieee_support_nan(x)

i = i + 1
la(i) = ieee_support_rounding(ieee_nearest)
i = i + 1
la(i) = ieee_support_rounding(ieee_nearest,a)
i = i + 1
la(i) = ieee_support_rounding(ieee_nearest,z)

i = i + 1
la(i) = ieee_support_rounding(ieee_to_zero)
i = i + 1
la(i) = ieee_support_rounding(ieee_to_zero,b)
i = i + 1
la(i) = ieee_support_rounding(ieee_to_zero,y)

i = i + 1
la(i) = ieee_support_rounding(ieee_up)
i = i + 1
la(i) = ieee_support_rounding(ieee_up,c)
i = i + 1
la(i) = ieee_support_rounding(ieee_up,x)

i = i + 1
la(i) = ieee_support_rounding(ieee_down)
i = i + 1
la(i) = ieee_support_rounding(ieee_down,a)
i = i + 1
la(i) = ieee_support_rounding(ieee_down,z)

i = i + 1
la(i) = ieee_support_sqrt()
i = i + 1
la(i) = ieee_support_sqrt(a)
i = i + 1
la(i) = ieee_support_sqrt(z)

i = i + 1
la(i) = ieee_support_standard()
i = i + 1
la(i) = ieee_support_standard(b)
i = i + 1
la(i) = ieee_support_standard(y)

call check(la,lb,i)

100 format(e22.12,2x,z8.8)
end
