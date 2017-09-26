!
! Copyright (c) 2014, NVIDIA CORPORATION.  All rights reserved.
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
! This tests commas in replacement lists
!
#define FOO(a, b, c) a, b, c

subroutine dostuff(x, y, z)
    integer :: x, y, z
    if (x .eq. 42 .and. y .eq. 43 .and. z .eq. 44) then
        call check(.true., .true., 1)
    else
        call check(.false., .true., 1)
    endif
    print *, x, y, z
end subroutine

program p
    call dostuff(FOO(42, 43, 44))
end program
