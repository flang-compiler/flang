! Copyright (c) 2019, NVIDIA CORPORATION.  All rights reserved.
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

! Similar to oop749 but has multiple mold= allocations.

program p
  logical rslt(2),expect(2)
  integer, allocatable :: a(:), b(:)
  integer c(10)

  c = 99
  
  allocate(a(10), b, mold=c)

  rslt(1) = size(b,1) .eq. size(c,1)
  rslt(2) = size(a,1) .eq. size(b,1)

  expect = .true.
  call check(rslt,expect,2)


  end
