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

program z
use ieee_arithmetic
real a,b,c
logical la(16), lb(16)
logical lfsav(5), lfset(5)

lfset = .false.
call ieee_get_halting_mode(ieee_all, lfsav)
call ieee_set_halting_mode(ieee_all, lfset)

a = huge(c)
b = ieee_next_after(a,ieee_value(a,ieee_positive_inf))
write (6,100) b, b
la(1) = b .gt. a

c = ieee_next_after(a,0.0)
write (6,100) c, c
la(2) = a .gt. c

a = 1.0
b = ieee_next_after(a,ieee_value(a,ieee_positive_inf))
write (6,100) b, b
la(3) = b .gt. a

c = ieee_next_after(a,0.0)
write (6,100) c, c
la(4) = a .gt. c

a = tiny(c)
b = ieee_next_after(a,ieee_value(a,ieee_positive_inf))
write (6,100) b, b
la(5) = b .gt. a

c = ieee_next_after(a,0.0)
write (6,100) c, c
la(6) = a .gt. c

a = 0.0
b = ieee_next_after(a,ieee_value(a,ieee_positive_inf))
write (6,100) b, b
la(7) = b .gt. a

c = ieee_next_after(a,ieee_value(a,ieee_negative_inf))
write (6,100) c, c
la(8) = a .gt. c

a = -tiny(c)
b = ieee_next_after(a,0.0)
write (6,100) b, b
la(9) = b .gt. a

c = ieee_next_after(a,ieee_value(a,ieee_negative_inf))
write (6,100) c, c
la(10) = a .gt. c

a = -1.0
b = ieee_next_after(a,0.0)
write (6,100) b, b
la(11) = b .gt. a

c = ieee_next_after(a,ieee_value(a,ieee_negative_inf))
write (6,100) c, c
la(12) = a .gt. c

a = -huge(c)
b = ieee_next_after(a,0.0)
write (6,100) b, b
la(13) = b .gt. a

c = ieee_next_after(a,ieee_value(a,ieee_negative_inf))
write (6,100) c, c
la(14) = a .gt. c

lb = .true.
call check(la,lb,14)
call ieee_set_halting_mode(ieee_all, lfsav)

100 format(e22.12,2x,z8.8)
end
