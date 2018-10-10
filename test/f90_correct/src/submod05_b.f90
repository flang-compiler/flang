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

! Second test of using with two procedures inside a submodule.
!

MODULE m                               ! The ancestor module m
  INTEGER :: res1
  INTEGER :: res2
  INTERFACE
    MODULE SUBROUTINE sub1(arg1)       ! Module procedure interface body for sub1
      INTEGER, intent(inout) :: arg1
    END SUBROUTINE

    MODULE SUBROUTINE sub2(arg2)       ! Module procedure interface body for sub2
      INTEGER, intent(inout) :: arg2
    END SUBROUTINE
  END INTERFACE
END MODULE

SUBMODULE (m) n                        ! The descendant submodule n
  CONTAINS                             ! Module subprogram part
    MODULE SUBROUTINE sub1(arg1)       ! Definition of sub1 by subroutine subprogram
      INTEGER, intent(inout) :: arg1
      res1 = arg1 + 1
    END SUBROUTINE sub1

    MODULE SUBROUTINE sub2(arg2)       ! Definition of sub2 by separate module subprogram
      INTEGER, intent(inout) :: arg2
      res2 = arg2 + 2
    END SUBROUTINE sub2
END SUBMODULE

program test
  use m
  implicit none
  integer :: k
  k = 99
  call sub1(k)
  call sub2(k)
  if (res1 .ne. 100 .OR. res2 .ne.101) then
    print *,"FAIL"
  else 
    print *, "PASS"
  end if
end program test

