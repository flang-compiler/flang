! Copyright (c) 2018-2019, NVIDIA CORPORATION.  All rights reserved.
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

! sourced allocation with characters of an unlimited polymorphic value
! 
! same as oop731 except uses multiple sourced allocations

class(*), allocatable :: aa(:)
class(*), allocatable :: bb(:)
character*10 :: result
call set(aa,bb)
select type(aa)
type is (character(*))
  result = aa(1) // aa(2)
class default
  result = " FAIL"
end select
if (result .ne. " PASS") result = " FAIL"
if (result .eq. " PASS") then
  select type(bb)
  type is (character(*))
    result = bb(1) // bb(2)
  class default
    result = " FAIL"
  end select
endif
if (result .ne. " PASS") result = " FAIL"
print*, result
contains
  subroutine set(vv,uu)
    class(*), allocatable, intent(out) :: vv(:)
    class(*), allocatable, intent(out) :: uu(:)
    allocate(vv,  uu, source=[' PAS','S   '])
  end subroutine
end
