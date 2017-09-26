
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
! Test that output of slice works o.k.
        PROGRAM test

           IMPLICIT NONE

        integer result(3),expect(3)
        data expect/2,4,6/


        integer a(2,3)
        integer res(3)

        
        a(1,1) = 1
        a(2,1) = 2
        a(1,2) = 3
        a(2,2) = 4
        a(1,3) = 5
        a(2,3) = 6

        OPEN (UNIT=10, FILE = "io02.txt", STATUS = "REPLACE")
        write(UNIT=10,*)  a(2:2,:)
        CLOSE (10)
        OPEN (UNIT=10, FILE = "io02.txt", STATUS = "OLD")
        read (10,*) result(:)
        CLOSE (10)
        call check(result,expect,3)
        end program

!           2            4            6
