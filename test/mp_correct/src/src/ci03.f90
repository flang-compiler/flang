!
! Copyright (c) 2016, NVIDIA CORPORATION.  All rights reserved.
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
! Test copyin common blocks
!

integer :: foo,bar
common /CMA/foo,bar
!$omp threadprivate(/CMA/)

!$omp master
    foo = 10
    bar = 20
!$omp end master

!$omp parallel copyin(/CMA/) num_threads(4)
    foo = foo + 1
    bar = bar + 1
    call check(foo, 11, 1)
    call check(bar, 21, 1)
!$omp end parallel
    end
