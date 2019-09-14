!
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

! 2d int array, maxloc with dim, mask, and back

program main
  integer :: res(4)
  integer, dimension(4,4) :: array
  integer :: expect(4)
  array = reshape((/4,2,9,-7,9,1,5,5,8,-1,-1,5,-7,5,9,-7/),shape(array))
  res = maxloc(array, dim = 1, mask = array .lt. 7, back=.true.)
  expect = (/1,4,4,2/)
  !print *, "base=true:", res
  call check(res, expect, 4)
  res = maxloc(array, dim = 1, mask = array .lt. 7, back=.false.)
  expect = (/1,3,4,2/)
  !print *, "back=false:", res
  call check(res, expect, 4)
end
