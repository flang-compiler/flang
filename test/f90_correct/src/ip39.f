!
! Copyright (c) 2005, NVIDIA CORPORATION.  All rights reserved.
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
! Access host dummies from within the internal procedure.  The internal
! procedure needs to generate offsets from the 'display'.  The host
! dummies may be in stack of the host or the stack of the host's caller.
! The offsets can get messed up for 32-bit osx ABI (and newer 32-bit linux)
! where the stack is kept 16-byte aligned and the stack is 0mod16 at
! the point of the call instruction
!
	module zzz
	type myt
	    integer arr(100)
	    integer s1
	    integer s2
	endtype
	integer(4) res(6)
	contains
	    subroutine sub(rec, ii, cc)
		type(myt) :: rec
		integer ii
		character*(*) cc
		type(myt) :: lrg
		lrg%arr(99) = 787
		call internal
		contains
		subroutine internal
		    res(1) = rec%s1
		    res(2) = rec%s2
		    res(3) = ii
		    res(4) = len(cc)
		    res(5) = ichar(cc(1:1))
		    res(6) = ichar(cc(2:2))
		    end subroutine
	    end subroutine
	end module

	program test
	use zzz
	integer(4) exp(6)
	data exp/17, 23, 37, 2, 97, 98/
	type (myt) st
	st%s1 = 17
	st%s2 = 23
	call sub(st, 37, 'ab')
	call check(res, exp, 6)
!!	print 99, 'EXPECT: ', exp
!!	print 99, 'RESULT: ', res
!!99	format(a, 10i3)
	end
