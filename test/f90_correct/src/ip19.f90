!
! Copyright (c) 1999, NVIDIA CORPORATION.  All rights reserved.
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
!	native pghpf problem:
!	host subprogram, both host and contained routines
!	need static-init routines
!
	subroutine ss(n,a)

	integer :: a(50)
	integer,save :: b(50)

!hpf$	distribute a(block),b(block)

	if( n .eq. 1 ) then
	 b = a
	 call sub( n )
	else
	 call sub( n )
	 a = b
	endif

	contains
	subroutine sub( n )
	integer,save :: c(50)
!hpf$	distribute c(block)

	if( n .eq. 1 ) then
	 c = a + b	! 1+1, 2+2, 3+3, ...
	else
	 b = b + c + a	! 1+2+1, 2+4+2, 3+6+3, ...
	endif
	end subroutine
	end

	subroutine tt(n,a)

	integer :: a(50)
	integer,save :: b(50)

!hpf$	distribute a(block),b(block)

	if( n .eq. 1 ) then
	 b = a
	 call sub( n )
	else
	 call sub( n )
	 a = b
	endif

	contains
	subroutine sub( n )
	integer,save :: c(50)
!hpf$	distribute c(block)

	if( n .eq. 1 ) then
	 c = a * b	! 1*1, 2*2, 3*3, ...
	else
	 b = b * c * a	! 1*1*1, 2*4*2, 3*9*3, ...
	endif
	end subroutine
	end


	program p
	integer a(50)
!hpf$	distribute a(block)
	integer result(2), expect(2)
	data expect/5100, 65666665/

	forall(i=1:50) a(i) = i

	call ss(1,a)
	call ss(2,a)
	!print *,a
	result(1) = sum(a)
	forall(i=1:50) a(i) = i
	call tt(1,a)
	call tt(2,a)
	!print *,a
	result(2) = sum(a)
	call check(result,expect,2)
	end
