! Copyright (c) 2000, NVIDIA CORPORATION.  All rights reserved.
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
! test that empty contained subprograms compile
!
PROGRAM test

   IMPLICIT NONE

   INTEGER :: m, n
   REAL :: x, y
   real result(1),expect(1)
   data expect/1/

   result(1) = 0
   CALL input
   CALL Write
   CALL inputs
   call check(result,expect,1)

CONTAINS

   SUBROUTINE Input
   END SUBROUTINE Input


   SUBROUTINE Write
      result(1) = 1
   END SUBROUTINE

   SUBROUTINE Inputs
   END SUBROUTINE Inputs
END PROGRAM test


