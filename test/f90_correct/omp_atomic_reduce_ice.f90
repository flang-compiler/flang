! Copyright (c) 2019, Arm Ltd.  All rights reserved.
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

! RUN: %flang -fopenmp -S -emit-llvm %s -o - | FileCheck %s
! RUN: %flang -i8 -fopenmp -S -emit-llvm %s -o - | FileCheck %s

subroutine reduce()
integer :: j
logical(kind=8) :: error_status = .FALSE.
!$omp parallel do reduction(.or.: error_status)
do j=1,100
end do
!$omp end parallel do
end subroutine
! //CHECK: atomicrmw

subroutine atomic_logical_1(n,val)
logical(kind=1) :: lg = .FALSE.
integer :: n, val, i
!$omp parallel
do i=1,n
!$omp atomic
lg = lg .or. val==n
end do
!$omp end parallel
end subroutine
! //CHECK: atomicrmw

subroutine atomic_logical_8(n,val)
logical(kind=8) :: lg = .FALSE.
integer :: n, val, i
!$omp parallel
do i=1,n
!$omp atomic
lg = lg .or. val==n
end do
!$omp end parallel
end subroutine
! //CHECK: atomicrmw

subroutine atomic_integer_1(n,val)
integer(kind=1) :: lg, val
integer :: n, i
!$omp parallel
do i=1,n
!$omp atomic
lg = lg + val
end do
!$omp end parallel
end subroutine
! //CHECK: atomicrmw
