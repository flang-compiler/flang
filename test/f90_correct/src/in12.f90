
!
! Copyright (c) 2000, NVIDIA CORPORATION.  All rights reserved.
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
! test f2008 log_gamma intrinsic

program p 
 use ISO_C_BINDING
 use check_mod

  interface
    subroutine get_expected_f( src, expct, n ) bind(C)
     use ISO_C_BINDING
      type(C_PTR), value :: src
      type(C_PTR), value :: expct
      integer(C_INT), value :: n
    end subroutine
    
    subroutine get_expected_d( src, expct, n ) bind(C)
     use ISO_C_BINDING
      type(C_PTR), value :: src
      type(C_PTR), value :: expct
      integer(C_INT), value :: n
    end subroutine
  end interface
    

  integer, parameter :: N=20
  real*4, target,  dimension(N) :: r_src
  real*4, target,  dimension(N) :: r_rslt
  real*4, target,  dimension(N) :: r_expct
  
  real*8, target,  dimension(N) :: d_src
  real*8, target,  dimension(N) :: d_rslt
  real*8, target,  dimension(N) :: d_expct
  
  do i =  0,N-1 
    r_src(i+1) = i**6.9 + .2
    d_src(i+1) = (i**20.0)  + .2
  enddo

  r_rslt = log_gamma(r_src)
  d_rslt = log_gamma(d_src)

  call get_expected_f(C_LOC(r_src), C_LOC(r_expct), N)
  call get_expected_d(C_LOC(d_src), C_LOC(d_expct), N)

  call checkr4( r_rslt, r_expct, N, rtoler=0.0000003)
  call checkr8( d_rslt, d_expct, N, rtoler=0.0000003_8)

!   print *, "r_expct:" 
!   print *, r_expct
!   print *, "r_rslt:" 
!   print *, r_rslt

!   print *, "d_expct:" 
!   print *, d_expct
!   print *, "d_rslt:" 
!   print *, d_rslt
end program 
