! 
! Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
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

!          THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT
!   WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT
!   NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR
!   FITNESS FOR A PARTICULAR PURPOSE. 
!
! omp_lib_kinds.f90
!

module omp_lib_kinds
  integer, parameter :: omp_integer_kind = 4
  integer, parameter :: omp_logical_kind = 4
#if defined(TARGET_KMPC)
  integer, parameter :: omp_lock_kind = 8
#else
  integer, parameter :: omp_lock_kind = 4
#endif
  integer, parameter :: omp_nest_lock_kind = 8
  integer, parameter :: omp_sched_kind  = 4
  integer, parameter :: omp_real_kind          = 4
  integer, parameter :: omp_proc_bind_kind     = omp_integer_kind
  integer, parameter :: omp_lock_hint_kind     = omp_integer_kind

  integer(kind=omp_sched_kind), parameter :: omp_sched_static  = 1
  integer(kind=omp_sched_kind), parameter :: omp_sched_dynamic = 2
  integer(kind=omp_sched_kind), parameter :: omp_sched_guided  = 3
  integer(kind=omp_sched_kind), parameter :: omp_sched_auto    = 4

  integer (kind=omp_proc_bind_kind), parameter :: omp_proc_bind_false = 0
  integer (kind=omp_proc_bind_kind), parameter :: omp_proc_bind_true = 1
  integer (kind=omp_proc_bind_kind), parameter :: omp_proc_bind_master = 2
  integer (kind=omp_proc_bind_kind), parameter :: omp_proc_bind_close = 3
  integer (kind=omp_proc_bind_kind), parameter :: omp_proc_bind_spread = 4

  integer (kind=omp_lock_hint_kind), parameter :: omp_lock_hint_none           = 0
  integer (kind=omp_lock_hint_kind), parameter :: omp_lock_hint_uncontended    = 1
  integer (kind=omp_lock_hint_kind), parameter :: omp_lock_hint_contended      = 2
  integer (kind=omp_lock_hint_kind), parameter :: omp_lock_hint_nonspeculative = 4
  integer (kind=omp_lock_hint_kind), parameter :: omp_lock_hint_speculative    = 8


end module omp_lib_kinds
