! Copyright (c) 1990-2017, NVIDIA CORPORATION.  All rights reserved.
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

! example ICE - unexpected data type at assignment

  subroutine sub2(c2)
    character(*), intent(inout) ::c2(:)
    integer,allocatable::store(:)
    integer :: storesize, i

    storesize = size(transfer(c2, store))
    allocate(store(storesize))

    store(1) = 2032166473
    store(2) = 1663071599
    store(3) = 1914728033
    store(4) =  543449445
    store(5) = 1936287860
    store(6) = 1752440876
    store(7) = 1702109285
    store(8) = 1881175155
    store(9) = 1702064993
    store(10) = 538979940

    c2 = transfer(store, c2)
    deallocate(store)
  end subroutine sub2

  interface
    subroutine sub2(c2)
      character(*), intent(inout) ::c2(:)
    end subroutine sub2
  end interface

  character(10), allocatable :: s(:)
  integer i, results(8)
  integer :: expect(8) = (/ .true., .true., .true., .true., .true., .true., .true., .true. /)
  allocate(s(4))
  call sub2(s)
!  print *, s
  results(1) = lle(s(1), "If you can")
  results(2) = lge(s(1), "If you can")
  results(3) = lle(s(2), " read this")
  results(4) = lge(s(2), " read this")
  results(5) = lle(s(3), ", the test")
  results(6) = lge(s(3), ", the test")
  results(7) = lle(s(4), " passed.  ")
  results(8) = lge(s(4), " passed.  ")
  deallocate(s)
  call check(results, expect, 8)
  end
