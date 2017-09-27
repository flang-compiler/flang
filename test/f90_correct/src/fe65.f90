!* Copyright (c) 2000, NVIDIA CORPORATION.  All rights reserved.
!*
!* Licensed under the Apache License, Version 2.0 (the "License");
!* you may not use this file except in compliance with the License.
!* You may obtain a copy of the License at
!*
!*     http://www.apache.org/licenses/LICENSE-2.0
!*
!* Unless required by applicable law or agreed to in writing, software
!* distributed under the License is distributed on an "AS IS" BASIS,
!* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!* See the License for the specific language governing permissions and
!* limitations under the License.
!
!	array and scalar conformance (logical)

 subroutine x(nphi)
   integer            :: nphi
   integer, parameter :: maxphi=100
   double precision   :: rmax(maxphi)
   double precision  rmod
   logical :: within(maxphi)

   within(1:nphi) = rmod .lt. rmax(1:nphi)   !! LEGAL
 end subroutine x

	integer res, exp
	res = 1
	data exp/1/
	call check(res,exp,1)
	end
