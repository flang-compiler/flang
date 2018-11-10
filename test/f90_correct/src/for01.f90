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

! typed forall indexes

integer, parameter :: N = 6

integer :: i = -33
integer :: a(N) = 0
integer :: s = 0

forall (integer*1 :: i=N:1:-1) a(i) =  1; s = s + sum(a)/N  !  1
forall (integer*2 :: i=N:1:-1) a(i) =  2; s = s + sum(a)/N  !  3
forall (             i=N:1:-1) a(i) =  3; s = s + sum(a)/N  !  6
forall (integer*8 :: i=N:1:-1) a(i) =  4; s = s + sum(a)/N  ! 10
forall (integer*1 :: i=N:1:-1) a(i) =  5; s = s + sum(a)/N  ! 15
forall (integer*2 :: i=N:1:-1) a(i) =  6; s = s + sum(a)/N  ! 21
forall (             i=N:1:-1) a(i) =  7; s = s + sum(a)/N  ! 28
forall (             i=N:1:-1) a(i) =  8; s = s + sum(a)/N  ! 36
forall (integer*4 :: i=N:1:-1) a(i) =  9; s = s + sum(a)/N  ! 45
forall (integer*8 :: i=N:1:-1) a(i) = 10; s = s + sum(a)/N  ! 55

if (s .ne. 55 .or.  i .ne. -33) print*, 'FAIL'
if (s .eq. 55 .and. i .eq. -33) print*, 'PASS'
end
