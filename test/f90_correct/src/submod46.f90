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
  INTERFACE
  INTEGER MODULE FUNCTION F1(arg1)
    INTEGER, intent(inout) :: arg1
  END FUNCTION
  
  INTEGER MODULE FUNCTION F2(arg2)
    INTEGER, intent(inout) :: arg2
  END FUNCTION

  END INTERFACE
END MODULE

SUBMODULE (m) n
  CONTAINS
    REAL MODULE FUNCTION F1(arg1) !{error "PGF90-S-1061-The definition of function return type of f1 does not match its declaration type"}
      INTEGER, intent(inout) :: arg1
    END FUNCTION F1

    REAL MODULE FUNCTION F2(arg2) RESULT(ret) !{error "PGF90-S-1061-The definition of function return type of f2 does not match its declaration type"}
      INTEGER, intent(inout) :: arg2
      ret = arg2
    END FUNCTION

END SUBMODULE

