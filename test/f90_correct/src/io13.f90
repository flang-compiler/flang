! Copyright (c) 2013, NVIDIA CORPORATION.  All rights reserved.
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
!
! Tests F2003 defined I/O (unformatted write)
! This example is based on example from the Fortran 2003 Handbook (page 336)

module rational_stuff
  type rational
    integer n, d
    real :: rat_rslt=0.0
  end type

  interface write (unformatted)
    module procedure write_rational_value
  end interface write (unformatted)
  interface read (unformatted)
    module procedure read_rational_value
  end interface read (unformatted)


 contains
  subroutine write_rational_value(dtv, unit, iostat, iomsg)

  class(rational), intent(inout) :: dtv
  integer, intent(in) :: unit
  integer, intent(out) :: iostat
  character(len=*), intent(inout) :: iomsg
  real rat 

     if (dtv%d .ne. 0) then
       dtv%rat_rslt = real(dtv%n)/real(dtv%d)
       write(unit), dtv%n, dtv%d, dtv%rat_rslt  
     endif
  end subroutine

  subroutine read_rational_value(dtv, unit, iostat, iomsg)

  class(rational), intent(inout) :: dtv
  integer, intent(in) :: unit
  integer, intent(out) :: iostat
  character(len=*), intent(inout) :: iomsg

  end subroutine
end module

  use rational_stuff
  type(rational) x, y
  logical rslt(3), expect(3)
  x = rational(2,3)
  open(10, file='io13.output', status='replace', form='unformatted')
  write(10) , x
  close(10)
  open(10, file='io13.output', status='old', form='unformatted')
  read(10), y%n, y%d, y%rat_rslt
  close(10)

  !print *, x%n, x%d, x%rat_rslt
  !print *, y%n, y%d, y%rat_rslt

  expect = .true.
  rslt(1) = x%n .eq. y%n
  rslt(2) = x%d .eq. y%d
  rslt(3) = x%rat_rslt .eq. y%rat_rslt

  call check(rslt, expect, 3) 
  end



