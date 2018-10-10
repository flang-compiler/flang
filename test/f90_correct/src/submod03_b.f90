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

! Test of using derived type inside procedures of a sudmodule
!

MODULE m1
  TYPE Base
    INTEGER :: a
  END TYPE
  
  INTERFACE
    MODULE SUBROUTINE sub1(b)     ! Module procedure interface body for sub1
      TYPE(Base), INTENT(IN) :: b
    END SUBROUTINE
  END INTERFACE
END MODULE

SUBMODULE (m1) m1sub
  CONTAINS
    MODULE PROCEDURE sub1         ! Implementation of sub1 declared in m1
      !PRINT *, "sub1", b
      if (b%a .ne. 11) then
        print *, "FAIL"
      else
        print *, "PASS"
      end if
    END PROCEDURE
END SUBMODULE

PROGRAM example
  USE m1
  implicit none
  CALL sub1(Base(11))
END PROGRAM
