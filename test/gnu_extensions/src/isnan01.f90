! Copyright (c) 2018, Advanced Micro Devices, Inc. All rights reserved.
! Date of Modification: September 2019

! Copyright (c) 2015, NVIDIA CORPORATION.  All rights reserved.
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

subroutine nan_check(is_nan, res)
  logical :: res(10)
  logical :: is_nan

  res(7) = is_nan
end subroutine nan_check

logical function nan_check2(is_nan)
  logical :: is_nan
  nan_check2 = is_nan
end

program isnan01
  integer, parameter :: N = 10
  integer :: exp(N), res(N)
  real ::nanf = 0.0, arr(1)
  real(kind=8) :: nand =0.d0
  res = 1
  arr(1) = 1.0

  ! constant arguments
  if (isnan(0.0)) res(1) = 0
  if (isnan(0.d0)) res(2) = 0

  ! scalar float/ double values
  if (isnan(nanf)) res(3) = 0
  if (isnan(nand)) res(4) = 0
  if (isnan(arr(1))) res(5) = 0

  ! create NaNs
  nanf = nanf / 0.0
  nand = nand / 0.d0

  ! logical expressions.
  res(6) = isnan(nanf) .and. isnan(nand) .and. .true.

  ! as argument to subroutines.
  call nan_check(isnan(nanf), res)

  ! as function call
  res(8) = nan_check2(isnan(nand))

  ! more logical expressions
  if (.not. isnan(sqrt(-1.0))) res(9) = 0
  if (.not. isnan(sqrt(-1.d0))) res(10) = 0


  exp(1:N) = 1
  call check(res, exp, N)
end program
