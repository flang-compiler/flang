! Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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

! use only clause in multiple interface blocks

module mm
  integer, parameter :: kk = 4
end module mm

subroutine ss
  interface
    integer(kind=kk) function ff1(aa, bb)
      use mm, only : kk
      implicit none
      integer(kind=kk) :: aa, bb
    end function ff1
  end interface

  interface
    integer(kind=kk) function ff2(aa, bb)
      use mm, only : kk
      implicit none
      integer(kind=kk) :: aa, bb
    end function ff2
  end interface

  if (ff1(1,2) + ff2(3,4) .eq. 10) then
    print*, "PASS"
  else
    print*, "FAIL"
  endif
end subroutine ss

integer(kind=kk) function ff1(aa, bb)
  use mm, only : kk
  implicit none
  integer(kind=kk) :: aa, bb
  ff1 = aa + bb
end function ff1

integer(kind=kk) function ff2(aa, bb)
  use mm, only : kk
  implicit none
  integer(kind=kk) :: aa, bb
  ff2 = aa + bb
end function ff2

  call ss
end
