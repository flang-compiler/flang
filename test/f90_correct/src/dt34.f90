!*** Copyright (c) 1999, NVIDIA CORPORATION.  All rights reserved.
!***
!*** Licensed under the Apache License, Version 2.0 (the "License");
!*** you may not use this file except in compliance with the License.
!*** You may obtain a copy of the License at
!***
!***     http://www.apache.org/licenses/LICENSE-2.0
!***
!*** Unless required by applicable law or agreed to in writing, software
!*** distributed under the License is distributed on an "AS IS" BASIS,
!*** WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!*** See the License for the specific language governing permissions and
!*** limitations under the License.
!
! problem found during testing of 3.1 release
! infinite loop while processing subroutine ss
! in 'scatter_dependency' in vsub.c

	program p
        type dt
	 integer ff
	 integer,pointer,dimension(:) :: gg
        end type

	integer, dimension(19) :: result,expect
	data expect/5*99,5*1,9*2/

	type(dt)::xx(2)
	xx(1)%ff = 2
	xx(2)%ff = 1

	allocate(xx(1)%gg(10))
	allocate(xx(2)%gg(9))
	xx(1)%gg(:) = 1
	xx(2)%gg(:) = 2

	call iter_mat_com( xx, 5 )

	!print *,xx(1)%gg
	!print *,xx(2)%gg
	result(1:10) = xx(1)%gg(1:10)
	result(11:19) = xx(2)%gg(1:9)
	call check(result,expect,19)

	contains
	subroutine iter_mat_com(ee,n)
         type(dt)::ee(100)
	 forall(i=1:n) ee(ee(2)%ff)%gg(i) = 99
	end subroutine
	end
