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

! intrinsic statement in a contained routine

call ss

contains
  subroutine ss
    integer, intrinsic :: len
    call invoke(len, 'PASS')
  end

  subroutine invoke(f, string)
    integer f
    character*(*) string
    if (f(string) .eq. len(string)) then
      print*, string
      return
    endif
    print*, 'FAIL'
  end
end
