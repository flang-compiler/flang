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

! 2d int array, minloc with dim and back

program main
  integer :: res(3)
  integer, dimension(3,4) :: array
  integer :: expect(3)
  array = reshape((/4,3,-4,-4,1,-4,4,-2,5,2,6,5/),shape(array))
  res = minloc(array, dim = 2, back=.true.)
  expect = (/2,3,2/)
  call check(res, expect, 3)
  !print *, "base=true:", res
  res = minloc(array, dim = 2, back=.false.)
  expect = (/2,3,1/)
  call check(res, expect, 3)
  !print *, "back=false:", res
end
