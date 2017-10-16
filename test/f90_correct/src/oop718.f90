! Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
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

! Test unlimited polymorphic pointer assignment where the target is an unlimited
! polymorphic dummy argument and the corresponding actual argument is character.
! The length must be copied when the pointer is assigned.
module oop718
  implicit none
  class(*), pointer :: val => null()
contains
  subroutine set_val(in_val)
    class(*), target :: in_val
    val => in_val
  end subroutine
end module

use oop718
implicit none

character(len=8), target :: string_val

string_val = 'ABC'
call set_val(string_val)
select type(val)
  type is(character(len=*))
    if (val /= string_val) stop 'FAIL: wrong value'
  class default
    stop 'FAIL: wrong type'
end select
stop 'PASS'

end
