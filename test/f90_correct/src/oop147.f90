! Copyright (c) 2010, NVIDIA CORPORATION.  All rights reserved.
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


module my_container
  
  type container
     integer i
     real r
   contains
     procedure :: init => init_container
     procedure :: xi => extract_i
     procedure :: xr => extract_r
     generic :: extract => xi, xr
     procedure :: assign_to_i 
     procedure :: assign_to_r
     generic :: assignment(=) => assign_to_i, assign_to_r
  end type container

!  interface assignment(=)
!    module procedure assign_to_i
!  end interface

contains
  integer function extract_i(this, ii) RESULT(iii)
    class(container) :: this
    integer ii
    iii = this%i
  end function extract_i
  
  real function extract_r(this, rr) RESULT(rrr)
    class(container) :: this
    real rr
    rrr = this%r
  end function extract_r

  subroutine init_container(this, ic, ir)
    class(container) :: this
    integer :: ic
    real :: ir
    this%i = ic
    this%r = ir
  end subroutine init_container

  subroutine assign_to_i(this, src)
  class(container),intent(inout) :: this
  integer,intent(in) ::  src
  this%i = src
  end subroutine assign_to_i

  subroutine assign_to_r(this, src)
  class(container),intent(inout) :: this
  real,intent(in) ::  src
  this%r = src
  end subroutine assign_to_r


end module my_container

program prg
USE CHECK_MOD
  use my_container

  class(container),allocatable :: t
  class(container),allocatable :: t2
  integer ei
  real er
  character ec(10)
  logical rslt(4)
  logical expect(4)
  
  rslt = .false.
  expect = .true.
  
  allocate(t) 
  allocate(t2)
  call t%init(23,4.5)

  ei = 0
  er = 0.0

  er = t%extract(1.0)
  ei = t%extract(1)

  rslt(1) = er .eq. 4.5
  rslt(2) = ei .eq. 23

  t2 = ei
  t2 = er

  er = t2%extract(1.0)
  ei = t2%extract(1)

  rslt(3) = er .eq. 4.5
  rslt(4) = ei .eq. 23
  
  call check(rslt,expect,4)
  
end program prg


