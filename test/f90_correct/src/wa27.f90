!
! Copyright (c) 2015-2017, NVIDIA CORPORATION.  All rights reserved.
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


program tester

    ! PGI fortran compiler array object regression testcase
    !
    ! To build:
    !    pgf90 -o test test.f90
    !
    ! To run:
    !    ./test
    !
    ! Expected:
    !    The code should build correctly, and when executed produce the
    !    following output:
    !
    ! >>  1 1
    !
    ! Observed:
    !    The code fails to compile with the error message:
    !
    ! >> PGF90-S-0153-Array objects are not conformable  (test.f90: 45)
    ! >>  0 inform,   0 warnings,   1 severes, 0 fatal for tester
    !     
    ! Tested versions:
    !    12.3-0  - passes
    !    12.5-0  - passes
    !    12.9-0  - passes
    !    13.2-0 - passes
    !    13.5-0 - passes
    !    13.6-0 - passes
    !    13.10-0 - passes
    !    14.1-0 - passes
    !    14.3-0 - passes
    !    15.4-0 - Fails as described
    !
    ! Other compilers:
    !    gfortran - passes (all versions)
    !    ifort - passes (all versions)

    implicit none
    integer :: arr1(2), arr2(2)
    logical :: rslt(2), expect(2)

    arr2 = (/1, 2/)
    
    ! The following line is valid, but not accepted.
    arr1(:) = int(arr2(1))

    !write(6,'(2i2)') arr1

    rslt = .false. 
    rslt(1) = arr1(1) .eq. 1
    rslt(2) = arr1(2) .eq. 1

    expect = .true.

    call check(rslt,expect,2)

end program
