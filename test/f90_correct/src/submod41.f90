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
!       

! C1547 (R1526) MODULE shall appear only in the function-stmt or subroutine-stmt
! of a module subprogram or of a nonabstract interface body that is declared in
! the scoping unit of a module or submodule.

PROGRAM test
  IMPLICIT NONE
  
INTERFACE
    PURE INTEGER MODULE FUNCTION f1(i) !{error "PGF90-S-0310-MODULE prefix allowed only within a module or submodule"}
      INTEGER, INTENT(IN) :: i
    END FUNCTION f1
END INTERFACE

END PROGRAM test
