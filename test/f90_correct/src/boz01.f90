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

! 32-bit boz arguments with the high bit set for INT, REAL, CPLX intrinsics

integer(kind=8) :: i8
real(kind=8)    :: r8
complex(kind=8) :: c8
integer         :: summary = 0
integer(kind=8), parameter :: res = ishft(1_8, 31)

i8 = int(z'80000000', kind=8)
if (i8 .ne. res) summary = summary + 1

i8 = int(o'20000000000', kind=8)
if (i8 .ne. res) summary = summary + 2

i8 = int(b'10000000000000000000000000000000', kind=8)
if (i8 .ne. res) summary = summary + 4

r8 = real(z'80000000', kind=8)
if (transfer(r8, i8) .ne. res) summary = summary + 8

r8 = real(o'20000000000', kind=8)
if (transfer(r8, i8) .ne. res) summary = summary + 16

r8 = real(b'10000000000000000000000000000000', kind=8)
if (transfer(r8, i8) .ne. res) summary = summary + 32

c8 = cmplx(z'80000000', kind=8)
if (transfer(real(c8), i8) .ne. res) summary = summary + 64

c8 = real(o'20000000000', kind=8)
if (transfer(real(c8), i8) .ne. res) summary = summary + 128

c8 = real(b'10000000000000000000000000000000', kind=8)
if (transfer(real(c8), i8) .ne. res) summary = summary + 256

if (summary .eq. 0) write(*, '(A)')         'PASS'
if (summary .ne. 0) write(*, '(A, 2X, B9)') 'FAIL', summary

end
