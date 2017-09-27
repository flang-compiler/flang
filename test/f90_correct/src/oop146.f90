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
!     generic :: extract => xi, xr
  end type container

  type, extends(container) :: container2
  character c
  contains
  procedure :: init => init_container2
  procedure :: xc => extract_c
!  generic :: extract => xc
  end type container2


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

  character(10) function extract_c(this,cc) RESULT(ccc)
    class(container2) :: this
    character cc
    ccc = this%c
  end function
  
  subroutine init_container2(this, ic, ir, c)
    class(container2) :: this
    integer :: ic
    real :: ir
    character,optional :: c
    this%c = c
    call this%container%init(ic, ir)
  end subroutine init_container2

  subroutine init_container(this, ic, ir, c)
    class(container) :: this
    integer :: ic
    real :: ir
    character,optional  :: c
    this%i = ic
    this%r = ir
  end subroutine init_container
  
  
end module my_container

program prg
USE CHECK_MOD
  use my_container

  class(container2),allocatable :: t
  integer ei
  real er
  character ec(10)
  logical rslt(3)
  logical expect(3)
  
  rslt = .false.
  expect = .true.
  
  allocate(t) 
  call t%init(23,4.5,'Z')
  
  
  ei = 0
  er = 0.0
  ec = 'A'

!  er = t%extract(1.0)
!  ei = t%extract(1)
!  ec = t%extract('Z')

  er = t%xr(1.0)
  ei = t%xi(1)
  ec = t%xc('Z')

  rslt(1) = er .eq. 4.5
  rslt(2) = ei .eq. 23
  rslt(3) = ec(1) .eq. 'Z'
  
  call check(rslt,expect,3)
  
end program prg


