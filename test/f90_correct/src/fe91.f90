!
! Copyright (c) 2015, NVIDIA CORPORATION.  All rights reserved.
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

!
! Test F2008 iso_fortran_env constants/kinds
!
MODULE m
  IMPLICIT NONE
  TYPE dt
    INTEGER, ALLOCATABLE :: i
    CONTAINS
      PROCEDURE :: write_dt
      GENERIC :: WRITE(formatted) => write_dt
  END TYPE

  CONTAINS
    SUBROUTINE write_dt(dtv, unit, iotype, v_list, iostat, iomsg)
      CLASS(dt), INTENT(IN) :: dtv
      INTEGER, INTENT(IN) :: unit
      CHARACTER(*), INTENT(IN) :: iotype
      INTEGER, INTENT(IN) :: v_list(:)
      INTEGER, INTENT(OUT) :: iostat
      CHARACTER(*), INTENT(INOUT) :: iomsg
      INQUIRE(unit, iostat = iostat)
    END SUBROUTINE
END MODULE m

program p
 use, intrinsic :: iso_fortran_env
 use m
 
  TYPE(dt) d
  CHARACTER(10) :: internal_file
  INTEGER :: iostat

  integer, parameter :: N = 21
  integer :: rslts(N)
  integer :: expect(N) = (/ 1, 2, 4, 8, 1, 2, 4, 8, 4, 8, 1, 2, 4, 8, 1, 2, 4, 8, 4, 8, 99/)


  integer(INT8) :: i1
  integer(INT16) :: i2
  integer(INT32) :: i4
  integer(INT64) :: i8

  logical(LOGICAL8) :: l1
  logical(LOGICAL16) :: l2
  logical(LOGICAL32) :: l4
  logical(LOGICAL64) :: l8

  real(REAL32) :: r32
  real(REAL64) :: r64

  rslts(1) = sizeof(i1)
  rslts(2) = sizeof(i2)
  rslts(3) = sizeof(i4)
  rslts(4) = sizeof(i8)
  rslts(5) = sizeof(l1)
  rslts(6) = sizeof(l2)
  rslts(7) = sizeof(l4)
  rslts(8) = sizeof(l8)
  rslts(9) = sizeof(r32)
  rslts(10) = sizeof(r64)

  rslts(11:14) = INTEGER_KINDS
  rslts(15:18) = LOGICAL_KINDS
  rslts(19:20) = REAL_KINDS

  WRITE(internal_file, *, iostat = iostat) d
  rslts(21) = iostat

  call check(rslts, expect, N)
  !print*,rslts
end program 
