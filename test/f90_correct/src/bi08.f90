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

module mMmM

common n

contains

  subroutine mM1() ! mangled linker name
    n = n + 1
  end subroutine

  subroutine mM2() bind(C)
    n = n + 1
  end subroutine

  subroutine mM3() bind(C,name="") ! mangled linker name
    n = n + 1
  end subroutine

  subroutine mM4() bind(C,name="mM4")
    n = n + 1
  end subroutine

end module mMmM

subroutine sS1() ! mangled linker name
  common n
  n = n + 1
end subroutine

subroutine sS2() bind(C)
  common n
  n = n + 1
end subroutine

subroutine sS3() bind(C,name="") ! mangled linker name
  common n
  n = n + 1
end subroutine

subroutine sS4() bind(C,name="sS4")
  common n
  n = n + 1
end subroutine

! --------------------

use mMmM

interface
  subroutine sS2() bind(C)
  end subroutine
  subroutine sS3() bind(C,name="")
  end subroutine
  subroutine sS4() bind(C,name="sS4")
  end subroutine
  subroutine cC() bind(C)
  end subroutine
end interface

n = 0

call mM1
call mM2
call mM3
call mM4

call sS1
call sS2
call sS3
call sS4

call cC

if (n .eq. 12) print*, 'PASS'
if (n .ne. 12) print*, 'FAIL: expected 12 calls; made', n

end
