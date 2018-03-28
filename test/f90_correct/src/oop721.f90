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

! derived type pointer component initializations

character*8 :: result = "PASS"

type :: tt
  integer            :: aa(2) = 1
  integer            :: bb    = 2
  class(tt), pointer :: pp    => null()
  integer            :: cc    = 4
  class(tt), pointer :: qq(:) => null()
  integer            :: dd    = 6
  integer            :: ee(2) = 7
end type tt

type(tt) :: uu
type(tt) :: vv = tt((/11, 11/), 12, null(), 14, null(), 16, (/17, 17/))

if (associated(uu%pp)) result = 'fail a1'
if (associated(uu%qq)) result = 'fail a2'
if (associated(vv%pp)) result = 'fail a3'
if (associated(vv%qq)) result = 'fail a4'

allocate(uu%pp);    uu%pp%aa    =  3
allocate(uu%qq(2)); uu%qq(2)%ee =  5
allocate(vv%pp);    vv%pp%aa    = 13
allocate(vv%qq(2)); vv%qq(2)%ee = 15

if (.not. associated(uu%pp)) result = 'fail a5'
if (.not. associated(uu%qq)) result = 'fail a6'
if (.not. associated(vv%pp)) result = 'fail a7'
if (.not. associated(vv%qq)) result = 'fail a8'

if (uu%aa(1)       .ne. vv%aa(1)       - 10) result = 'fail v1'
if (uu%aa(2)       .ne. vv%aa(2)       - 10) result = 'fail v2'
if (uu%bb          .ne. vv%bb          - 10) result = 'fail v3'
if (uu%pp%aa(1)    .ne. vv%pp%aa(1)    - 10) result = 'fail v4'
if (uu%cc          .ne. vv%cc          - 10) result = 'fail v5'
if (uu%qq(2)%ee(2) .ne. vv%qq(2)%ee(2) - 10) result = 'fail v6'
if (uu%qq(2)%ee(2) .ne. vv%qq(2)%ee(2) - 10) result = 'fail v7'
if (uu%dd          .ne. vv%dd          - 10) result = 'fail v8'
if (uu%ee(1)       .ne. vv%ee(1)       - 10) result = 'fail v9'
if (uu%ee(2)       .ne. vv%ee(2)       - 10) result = 'fail v10'

print*, result

end
