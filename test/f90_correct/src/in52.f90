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

! intrinsic declarations in host and internal routine

program pp
  integer(kind=4), intrinsic :: len_trim
  call ss

contains
  integer function kk
    integer(kind=4) :: len_trim
    len_trim = 3
    kk = len_trim
  end

  subroutine ss
    integer(kind=4), intrinsic :: len_trim
    character*4 :: s = 'FAIL'
    if (len_trim(s) - kk() .eq. 1) then
      print*, 'PASS'
    else
      print*, s
    endif
  end
end
