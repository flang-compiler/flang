! Copyright (c) 2004, NVIDIA CORPORATION.  All rights reserved.
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
! achar intrinsic, array-valued
!

	integer :: result(5)
	integer :: expect(5)
	character*5 aa
	call forall_bug(aa)
	do i = 1, 5
	    result(i) = iachar(aa(i:i))
	enddo

	data expect/97,98,99,100,101/
!	print *, result
!	print *, expect
	call check(result, expect, 5)
	end

	subroutine forall_bug(word2)
	  implicit none
	  
	  integer :: i, ibuf(5)
	  character(len=5) :: word1='abcde'
	  character(len=1) :: word2(5)


	  forall(i=1:5) ibuf(i)=iachar(word1(i:i))
	  word2 = achar(ibuf)

!!	  do i=1,5
!!	     ibuf(i)=iachar(word1(i:i))
!!	     word2(i:i)=achar(ibuf(i))
!!	  end do

!	  write(*,*) word1
!	  write(*,*) word2
	end
