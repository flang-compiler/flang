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
! copyprivate (allocatable) test

subroutine test()
  integer expected(4)
  integer, allocatable,save  :: zptr(:)
  integer omp_get_thread_num

!$omp threadprivate(zptr)
  allocate(zptr(4))
  expected(1) = 1
  expected(2) = 2
  expected(3) = 3
  expected(4) = 4

!$omp parallel num_threads(8)
 if (.not.allocated(zptr)) allocate(zptr(4))
 zptr = 0  !! Zero out the data in all threads
!$omp single
  zptr(1)=1 !! Restore the data, and copy to all threads (copypriv)
  zptr(2)=2
  zptr(3)=3
  zptr(4)=4
!$omp end single copyprivate(zptr)
  call check(expected, zptr, 4)
!$omp end parallel
  call check(expected, zptr, 4)
end subroutine

program p
  call omp_set_num_threads(2)
  call test()
end program
