!
! Copyright (c) 1998, NVIDIA CORPORATION.  All rights reserved.
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
!  'partisn' problem
!  use of a module with only clause for name that is both
!  a module variable and a member in the module
	module part
	integer :: ib           ! ib is a module variable
	type btype
	   integer ib           ! ib is also a member
	endtype
	type(btype)::bb
	end module

	subroutine sub1
	use part, only: ib       ! ib appears in the only clause
	implicit none
	ib = 99                 ! warning & we create a local ib
	end

	subroutine sub2
	use part, only: ib=>bb, jb=>ib       ! ib appears in the only clause
	ib%ib = jb
	end

	program p
	use part
	integer result(2),expect(2)
	data expect/99,99/
	bb%ib = 0
	ib = 0
	call sub1
	call sub2
!	print *,ib
!	print *,bb%ib
	result(1) = ib
	result(2) = bb%ib
	call check(result,expect,2)
	end
	

