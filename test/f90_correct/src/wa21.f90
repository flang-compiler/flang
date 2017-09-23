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

! Cases producing a real result.

! scalar and array sources
integer*8, parameter :: is = 4608238818662570461_8
real*8, parameter :: rs = 1.23456789012345_8
integer*8, dimension(2), parameter :: ia = (/ 4608238818662570461_8, 4608238818662570461_8 /)
real*8, dimension(2), parameter :: ra = (/ 1.23456789012345_8, 1.23456789012345_8 /)

! types
real*8 :: t8s
real*4 :: t4s
real*8, dimension(2) :: t8a
real*4, dimension(2) :: t4a

! named constants
real*4, dimension(2), parameter :: p4a = transfer(is, t4a, 2)
real*4, dimension(2), parameter :: p4b = transfer(is, t4a)
real*4, dimension(2), parameter :: p4c = transfer(rs, t4a, 2)
real*4, dimension(2), parameter :: p4d = transfer(rs, t4a)
real*4, dimension(2), parameter :: p4e = transfer(ia, t4a, 2)
real*4, dimension(2), parameter :: p4f = transfer(ra, t4a, 2)
real*4, parameter :: p4g = transfer(is, t4s)
real*4, parameter :: p4h = transfer(rs, t4s)
real*4, parameter :: p4i = transfer(ia, t4s)
real*4, parameter :: p4j = transfer(ra, t4s)
real*8, dimension(2), parameter :: p8a = transfer(is, t8a, 2)
real*8, dimension(2), parameter :: p8b = transfer(rs, t8a, 2)
real*8, dimension(2), parameter :: p8c = transfer(ia, t8a, 2)
real*8, dimension(2), parameter :: p8d = transfer(ia, t8a)
real*8, dimension(2), parameter :: p8e = transfer(ra, t8a, 2)
real*8, dimension(2), parameter :: p8f = transfer(ra, t8a)
real*8, parameter :: p8g = transfer(is, t8s)
real*8, parameter :: p8h = transfer(rs, t8s)
real*8, parameter :: p8i = transfer(ia, t8s)
real*8, parameter :: p8j = transfer(ra, t8s)

real*8, dimension(32) :: results, expect

expect(01) = 70.175514
expect(02) = 1.904321
expect(03) = 70.175514
expect(04) = 1.904321
expect(05) = 70.175514
expect(06) = 1.904321
expect(07) = 70.175514
expect(08) = 1.904321
expect(09) = 70.175514
expect(10) = 1.904321
expect(11) = 70.175514
expect(12) = 1.904321
expect(13) = 70.175514
expect(14) = 70.175514
expect(15) = 70.175514
expect(16) = 70.175514
expect(17) = 1.23456789012345_8
expect(18) = 0.0
expect(19) = 1.23456789012345_8
expect(20) = 0.0
expect(21) = 1.23456789012345_8
expect(22) = 1.23456789012345_8
expect(23) = 1.23456789012345_8
expect(24) = 1.23456789012345_8
expect(25) = 1.23456789012345_8
expect(26) = 1.23456789012345_8
expect(27) = 1.23456789012345_8
expect(28) = 1.23456789012345_8
expect(29) = 1.23456789012345_8
expect(30) = 1.23456789012345_8
expect(31) = 1.23456789012345_8
expect(32) = 1.23456789012345_8

results(1) = p4a(1)
results(2) = p4a(2)
results(3) = p4b(1)
results(4) = p4b(2)
results(5) = p4c(1)
results(6) = p4c(2)
results(7) = p4d(1)
results(8) = p4d(2)
results(9) = p4e(1)
results(10) = p4e(2)
results(11) = p4f(1)
results(12) = p4f(2)
results(13) = p4g
results(14) = p4h
results(15) = p4i
results(16) = p4j
results(17) = p8a(1)
results(18) = p8a(2)
results(19) = p8b(1)
results(20) = p8b(2)
results(21) = p8c(1)
results(22) = p8c(2)
results(23) = p8d(1)
results(24) = p8d(2)
results(25) = p8e(1)
results(26) = p8e(2)
results(27) = p8f(1)
results(28) = p8f(2)
results(29) = p8g
results(30) = p8h
results(31) = p8i
results(32) = p8j

!print *, results(1), results(2)
!print *, results(3), results(4)
!print *, results(5), results(6)
!print *, results(7), results(8)
!print *, results(9), results(10)
!print *, results(11), results(12)
!print *, results(13)
!print *, results(14)
!print *, results(15)
!print *, results(16)
!print *, results(17), results(18)
!print *, results(19), results(20)
!print *, results(21), results(22)
!print *, results(23), results(24)
!print *, results(25), results(26)
!print *, results(27), results(28)
!print *, results(29)
!print *, results(30)
!print *, results(31)
!print *, results(32)

call checkd(results, expect, 32)

end
