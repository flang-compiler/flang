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

module z
USE CHECK_MOD
    contains
        subroutine print_me(mm)
        type rarr
        complex, pointer :: cmmm(:,:,:,:)
        endtype
	integer results(1)
	integer expect(1)
        type (rarr), allocatable :: data_p(:)
	type (rarr) :: p
	results = .false.
	expect = .true.
        allocate(data_p(mm))
	results(1) = same_type_as(p,data_p)
	call check(results,expect,1)
	
        endsubroutine
    end
    use z
    call print_me(10)
    end 

