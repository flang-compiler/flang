! 
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
! 

!          THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT
!   WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT
!   NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR
!   FITNESS FOR A PARTICULAR PURPOSE.
!
module IEEE_FEATURES
#ifdef PGDLL
!DEC$ ATTRIBUTES DLLEXPORT :: IEEE_FEATURES
#endif
type :: ieee_features_type
    private
    integer(4) :: ieee_features
end type
type(ieee_features_type), parameter :: ieee_datatype = ieee_features_type(1)
type(ieee_features_type), parameter :: ieee_denormal = ieee_features_type(2)
type(ieee_features_type), parameter :: ieee_divide   = ieee_features_type(3)
type(ieee_features_type), parameter :: ieee_halting  = ieee_features_type(4)
type(ieee_features_type), parameter :: ieee_inexact_flag = ieee_features_type(5)
type(ieee_features_type), parameter :: ieee_inf      = ieee_features_type(6)
type(ieee_features_type), parameter :: ieee_invalid_flag = ieee_features_type(7)
type(ieee_features_type), parameter :: ieee_nan = ieee_features_type(8)
type(ieee_features_type), parameter :: ieee_rounding = ieee_features_type(9)
type(ieee_features_type), parameter :: ieee_sqrt = ieee_features_type(10)
type(ieee_features_type), parameter :: ieee_underflow_flag = ieee_features_type(11)
end module IEEE_FEATURES
