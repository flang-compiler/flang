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

MODULE m
  INTEGER :: res
  INTERFACE
    PURE MODULE SUBROUTINE sub1(arg1)
      INTEGER, intent(inout) :: arg1
    END SUBROUTINE
  END INTERFACE
END MODULE

SUBMODULE (m) n
  CONTAINS
    MODULE SUBROUTINE sub1(arg1) !{error "PGF90-S-1060-The PURE function prefix of the definition and declaration of subprogram sub1 must match"}
      REAL, intent(inout) :: arg1
      print *, arg1
    END SUBROUTINE sub1
END SUBMODULE

program test
use m
integer :: a = 10
call sub1(a)


end 

