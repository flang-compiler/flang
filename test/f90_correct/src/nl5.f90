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

! namelist read of a non-initial pointer component

  type tt
    integer :: jj
  end type tt

  type(tt), target  :: aa, bb
  type(tt), pointer :: pp, qq

  namelist /nnn/ pp, qq

  pp => aa
  qq => bb

  aa%jj =  5
  bb%jj = 11

  open(14, file='nl5.dat')
  read(14, nnn)
  close(14)

  if (qq%jj .ne. 17) print*, 'FAIL'
  if (qq%jj .eq. 17) print*, 'PASS'
end
