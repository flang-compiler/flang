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

! do concurrent locality lists with intrinsic and keyword identifiers

module d
  integer :: a(5) = 1
end module d

subroutine ex ! explicit declarations
  use d
  integer if, index
  do concurrent (i=1:5) local(if,index) shared(a) default(none)
    if = a(i)
    index = if
    a(i) = index + 1
  end do
end

subroutine im ! implicit declarations
  use d
  do concurrent (i=1:5) local(if,index) shared(a) default(none)
    if = a(i)
    index = if
    a(i) = index + 1
  end do
end

  use d
  call ex
  call im
  if (any(a .ne. 3)) print*, 'FAIL'
  if (all(a .eq. 3)) print*, 'PASS'
end
