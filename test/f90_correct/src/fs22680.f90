! Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
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

subroutine sub1
  implicit none

  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12,307) !< double precision

  INTEGER,  PARAMETER :: nbndsw   = 14 !< number of spectral bands in sw model
  INTEGER,  PARAMETER :: jpb1     = 16 !< first band in sw
  INTEGER,  PARAMETER :: jpb2     = 29 !< last band in sw

  REAL(dp), PARAMETER :: wavenum2(jpb1:jpb2) = (/ &
    3250._dp, 4000._dp, 4650._dp, 5150._dp, 6150._dp, 7700._dp, 8050._dp, &
    12850._dp,16000._dp,22650._dp,29000._dp,38000._dp,50000._dp, 2600._dp/)


  REAL(dp), PARAMETER :: delwave(jpb1:jpb2)  = (/ &
       650._dp,  750._dp,  650._dp,  500._dp, 1000._dp, 1550._dp,  350._dp, &
       4800._dp, 3150._dp, 6650._dp, 6350._dp, 9000._dp,12000._dp, 1780._dp/)

  REAL (dp), PARAMETER     :: nir_vis_boundary   = 14500._dp

  INTEGER :: i
  REAL(dp),parameter :: frc_vis(1:nbndsw) = MAX(0.0_dp, MIN(1.0_dp, &
    (/ ((wavenum2(i+jpb1-1) - nir_vis_boundary) / delwave(i+jpb1-1), i = 1, nbndsw) /) ))

  PRINT *,frc_vis

END 



subroutine sub2
  implicit none

  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12,307) !< double precision

  INTEGER,  PARAMETER :: nbndsw   = 14 !< number of spectral bands in sw model
  INTEGER,  PARAMETER :: jpb1     = 16 !< first band in sw
  INTEGER,  PARAMETER :: jpb2     = 29 !< last band in sw

  REAL(dp), PARAMETER :: wavenum2(jpb1:jpb2) = (/ &
    3250._dp, 4000._dp, 4650._dp, 5150._dp, 6150._dp, 7700._dp, 8050._dp, &
    12850._dp,16000._dp,22650._dp,29000._dp,38000._dp,50000._dp, 2600._dp/)


  REAL(dp), PARAMETER :: delwave(jpb1:jpb2)  = (/ &
       650._dp,  750._dp,  650._dp,  500._dp, 1000._dp, 1550._dp,  350._dp, &
       4800._dp, 3150._dp, 6650._dp, 6350._dp, 9000._dp,12000._dp, 1780._dp/)

  REAL (dp), PARAMETER     :: nir_vis_boundary   = 14500._dp
  INTEGER :: i

  REAL(dp), PARAMETER :: temp1(1:nbndsw)  = delwave 
  REAL(dp), PARAMETER :: temp2(1:nbndsw)  = wavenum2


  REAL(dp), PARAMETER :: frc_vis(1:nbndsw) = MAX(0.0_dp, MIN(1.0_dp, &
    (/ ((temp2(i) - nir_vis_boundary) / temp1(i), i = 1, nbndsw) /) ))
  real(dp),parameter::x=frc_vis(9)


  PRINT *,"frc_vis:",frc_vis
  

END 

call sub1
call sub2
print *, "PASS"
end
