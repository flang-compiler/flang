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

	program p
USE CHECK_MOD
	type dt
        integer i(10)
        integer j(10)
        end type dt
        type(dt) d
        integer expect(3)
	integer results(3)
	do ii=1, 10
          d%i(ii) = ii
          d%j(ii) = 10+ii
        enddo
	expect = .true.
	associate (x => d%i, y => d%j)
	results(1) = all( x .eq. d%i)
	results(2) = all( y .eq. d%j)
	x = y
	results(3) = all(x .eq. d%j)
	end associate
        call check(results,expect,3)
	end program
