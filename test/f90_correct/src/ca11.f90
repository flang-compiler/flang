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

! array constructor with allocated element values

module mm
  type t_aa
    integer, allocatable :: ii(:)
  end type t_aa

contains
  integer function ff(aa)
    type(t_aa) :: aa(:)

    ff = 0
    do i = lbound(aa,1), ubound(aa,1)
      do j = lbound(aa(i)%ii,1), ubound(aa(i)%ii,1)
        ff = ff + aa(i)%ii(j)
      enddo
    enddo
  end function ff

  subroutine rr(kk)
    integer            :: kk, nn
    type(t_aa), target :: aa

    allocate(aa%ii(1))
    aa%ii = 7
    nn = ff((/(aa, j=1, kk)/))

    if (aa%ii(1) .eq. 7 .and. nn .eq. 35) then
      print*, 'PASS'
    else
      print*, 'FAIL'
    endif
  end subroutine rr
end module mm

  use mm
  call rr(5)
end
