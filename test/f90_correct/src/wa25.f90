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

! Example with PARAMETER declaration causes ICE 'module:new_dtype, dt nfd'

MODULE geo_coord_class

TYPE geo_coord
  INTEGER :: i
END TYPE geo_coord

TYPE(geo_coord),PARAMETER :: geo_coord_miss = geo_coord(-1)

END MODULE geo_coord_class

MODULE vol7d_ana_class
USE geo_coord_class

TYPE vol7d_ana
  TYPE(geo_coord) :: coord
END TYPE  vol7d_ana

 TYPE(vol7d_ana),PARAMETER :: vol7d_ana_miss=vol7d_ana(geo_coord_miss)
!  TYPE(vol7d_ana) :: vol7d_ana_miss=vol7d_ana(geo_coord_miss)

END MODULE vol7d_ana_class

MODULE vol7d_class
USE vol7d_ana_class

END MODULE vol7d_class

MODULE vol7d_oraclesim_class
USE vol7d_class

END MODULE vol7d_oraclesim_class

! test passes if it compiles without errors
call check(0,0,1)
end
