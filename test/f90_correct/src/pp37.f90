! Copyright (c) 2009, NVIDIA CORPORATION.  All rights reserved.
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
! test move_alloc intrinsic (F2003)

	module mov_mod
	implicit none
	real, allocatable :: a2(:)
        contains
	integer function test_mov(a3)
	real a3(:)
	real, allocatable :: a4(:)
	call move_alloc(from=a2, to=a4)
!	print *, a4
!	print *, all(a4 .eq. a3)
	test_mov = all(a4 .eq. a3)
	call move_alloc(from=a4, to=a2)
	end function
	end module mov_mod

	program p
	use mov_mod
	implicit none

	integer result(13), expect(13)

	real, allocatable :: a1(:)
        real :: a3(11)
        integer i

        data expect /.false.,.false.,.false.,.true.,.true.,.false.,.false.,.true.,.false.,.false.,.false.,.false.,.true./
	data a3 /10,9,8,7,6,5,4,3,2,1,0/ 

	result = -99
        call MOVE_ALLOC(TO=a2,FROM=a1)
!	print *, allocated(a1), allocated(a2)
	result(1) = allocated(a1)
	result(2) = allocated(a2)
	allocate(a1(0:10))
	do i=0,10
	   a1(i) = 10-i
	enddo
        call MOVE_ALLOC(TO=a2,FROM=a1)
	result(13) = test_mov(a3)
!	print *, a2
!	print *, allocated(a1), allocated(a2)
	result(3) = allocated(a1)
	result(4) = allocated(a2)
	call MOVE_ALLOC(FROM=a2, TO=a1)
!	print *, allocated(a1), allocated(a2)
	result(5) = allocated(a1)
        result(6) = allocated(a2)
	call MOVE_ALLOC(a1,a2)
!	print *, allocated(a1), allocated(a2)
	result(7) = allocated(a1)
        result(8) = allocated(a2)
        call MOVE_ALLOC(a2,a2)
!       print *, allocated(a1), allocated(a2)
	result(9) = allocated(a1)
        result(10) = allocated(a2)
        call MOVE_ALLOC(a1,a2)
!       print *, allocated(a1), allocated(a2)
	result(11) = allocated(a1)
        result(12) = allocated(a2)
	call check(result,expect,13)
	end
