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
! test f2008 tanh function taking a complex arguement

program p 
 use ISO_C_BINDING
 use check_mod

  interface
    subroutine get_expected_cf( src1, expct, n ) bind(C)
     use ISO_C_BINDING
      type(C_PTR), value :: src1
      type(C_PTR), value :: expct
      integer(C_INT), value :: n
    end subroutine
    
    subroutine get_expected_cd( src1, expct, n ) bind(C)
     use ISO_C_BINDING
      type(C_PTR), value :: src1
      type(C_PTR), value :: expct
      integer(C_INT), value :: n
    end subroutine
  end interface
    

  integer, parameter :: N=10
  complex(4), target,  dimension(N) :: cf_src1
  complex(4), target,  dimension(N) :: cf_rslt
  complex(4), target,  dimension(N) :: cf_expct
  complex(4) :: valuecf
  
  complex(8), target,  dimension(N) :: cd_src1
  complex(8), target,  dimension(N) :: cd_rslt
  complex(8), target,  dimension(N) :: cd_expct
  complex(8) :: value8
  
  valuecf = CMPLX(-31.4, -9.999)
  valuecd = CMPLX(-31.4_8, -9.999_8)
  do i =  0,N-1 
    cf_src1(i+1) = valuecf + CMPLX(i*6.97, i*2.2)
    cd_src1(i+1) = valuecd + CMPLX(i*6.97_8, i*2.2_8)
  enddo

  cf_rslt = tanh(cf_src1)
  cd_rslt = tanh(cd_src1)

  call get_expected_cf(C_LOC(cf_src1), C_LOC(cf_expct), N)
  call get_expected_cd(C_LOC(cd_src1), C_LOC(cd_expct), N)

  call checkc4( cf_rslt, cf_expct, N, rtoler=(0.0000003,0.0000003))
  call checkc8( cd_rslt, cd_expct, N, rtoler=(0.0000003_8,0.0000003_8))

!   print *, "cf_expct:" 
!   print *, cf_expct
!   print *, "cf_rslt:" 
!   print *, cf_rslt
!
!   print *, "cd_expct:" 
!   print *, cd_expct
!   print *, "cd_rslt:" 
!   print *, cd_rslt
end program 
