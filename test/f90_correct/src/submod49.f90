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
IMPLICIT NONE

  INTERFACE
    module function F1(A) result(B)
    integer, dimension(10), intent(in) :: A
    integer, allocatable :: B(:)
    end function 

    module function F2(C) result(D)
    integer, dimension(10), intent(in) :: C
    integer, allocatable :: D(:)
    end function 
  END INTERFACE
END MODULE

SUBMODULE (m) n
  CONTAINS
    MODULE FUNCTION F1(A) result(B) !{error "PGF90-S-1061-The definition of function return type of b does not match its declaration type"}
      integer, dimension(10), intent(in) :: A
      real, allocatable :: B(:)
    end function

    MODULE FUNCTION F2(C) result(D) !{error "PGF90-S-1061-The definition of function return type of d does not match its declaration type"}
      integer, dimension(10), intent(in) :: C
      integer, dimension(10) :: D
    end function
END SUBMODULE

