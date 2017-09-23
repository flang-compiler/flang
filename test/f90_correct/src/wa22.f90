! Copyright (c) 1990-2017, NVIDIA CORPORATION.  All rights reserved.
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

! transfer function in parameter specification

! Compare the result of compile-time transfer functions in parameter
! specifications with the result of a run-time transfer function operating
! on the same data.

character(60), dimension(7), parameter :: text1 = (/ &
    "8.9  Transfer function", &
    "The transfer function allows data of one type to be", &
    "transferred to another without the physical representation", &
    "being altered.  This would be useful, for example, in", &
    "writing a data storage and retrieval system.  The system", &
    "itself could be written for one type, default integer say,", &
    "and other types handled by transfers to and from that type." /)

character(42), dimension(10) :: type2
character(70), dimension(6) :: type3

character(42), dimension(10), parameter :: text2 = transfer(text1, type2)
character(70), dimension(6), parameter :: text3 = transfer(text2, type3)
integer*4, dimension(105), parameter :: results = transfer(text3, results)
integer*4, dimension(105) :: expect

expect = transfer(text1, expect)
call check(results, expect, 105)

end
