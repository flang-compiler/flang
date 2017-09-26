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
	integer i(10)
	integer j(10)
        integer x
        integer expect(3)
	integer results(3)
	data i /1,2,3,4,5,6,7,8,9,10/
	data j /11, 12, 13, 14, 15, 16, 17, 18, 19, 20/
	x = 3
	expect = .true.
	associate (x => i, y => j)
	results(1) = all( x .eq. i)
	results(2) = all( y .eq. j)
	end associate
	results(3) = x .eq. 3
        call check(results,expect,3)
	end program
