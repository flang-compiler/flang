! Copyright (c) 1990-2018, NVIDIA CORPORATION.  All rights reserved.
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

! Test with only one function inside a submodule.
!
MODULE m                                      ! The ancestor module m
  INTERFACE
    MODULE FUNCTION sub2(arg2) result (res)   ! Module procedure interface body for sub2
      INTEGER, intent(in) :: arg2
      INTEGER :: res
    END FUNCTION
  END INTERFACE
END MODULE

SUBMODULE (m) n                               ! The descendant submodule n
  CONTAINS                                    ! Module subprogram part
    MODULE FUNCTION sub2(arg2) result (res)   ! Definition of sub2 by separate module subprogram
      INTEGER, intent(in) :: arg2
      INTEGER :: res
      res = arg2 + 1
    END FUNCTION sub2
END SUBMODULE

program test 
use m
implicit none
  integer :: h
  integer :: haha
  h = 99
  haha = sub2(h)
  if (haha .ne. 100) then
    print *, "FAIL"
  else
    print *, "PASS"
  end if 
end program test
